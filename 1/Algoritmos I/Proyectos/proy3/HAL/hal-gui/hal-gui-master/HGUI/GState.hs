{-# LANGUAGE TemplateHaskell, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction #-}

module HGUI.GState where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.SourceView

import Control.Concurrent

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.State hiding (State,get,put)
import Control.Monad.Trans.RWS
import Data.IORef
import Data.Reference
import Data.Text (Text)

import HGUI.Evaluation.EvalState
import HGUI.ExtendedLang (ExtProgram)

type TextFilePath = Text

-- | Información sobre los items del toolBar.
data HalToolbar = HalToolbar { _evalButton :: ToggleToolButton
                             }
$(makeLenses ''HalToolbar)

data HalInfoConsole = HalInfoConsole { _infoConTView :: TextView }
$(makeLenses ''HalInfoConsole)

-- | Señales de los botones de nuevo, abrir, guardar, etc.
data HalToolBarButtons = HalToolBarButtons
                         { _newId    :: ToolButton
                         , _openId   :: ToolButton
                         , _saveId   :: ToolButton
                         , _saveAtId :: ToolButton
                         }
$(makeLenses ''HalToolBarButtons)

data HalMenuButtons = HalMenuButtons
                      { _mnewId    :: MenuItem
                      , _mopenId   :: MenuItem
                      , _msaveId   :: MenuItem
                      , _msaveAtId :: MenuItem
                      }
$(makeLenses ''HalMenuButtons)

data HalCommConsole = HalCommConsole { _cEvalBox       :: VBox
                                     , _cEvalStateBox  :: VBox
                                     , _cStateBox      :: VBox
                                     , _cStepDButton   :: Button
                                     , _cContButton    :: Button
                                     , _cBreakButton   :: Button
                                     , _cRestartButton :: Button
                                     , _cCleanButton   :: Button
                                     , _cStopButton    :: Button
                                     }
$(makeLenses ''HalCommConsole)

data HalEditorPaned = HalEditorPaned { _epaned :: VPaned }
$(makeLenses ''HalEditorPaned)

-- Con ForkFlag restringimos la creación de un thread para evaluar. La idea
-- es que si damos dos steps, el primero crea un thread pero el segundo no hace
-- nada, recién cuando el primero termina al dar de nuevo step, se crea
-- otro thread.
-- Con StopFlag restringimos el avance de la evaluación, la idea principal
-- es parar cualquier ejecución que no tenga final.
data HGReader = HGReader { _gHalToolbar     :: HalToolbar
                         , _gHalEditorPaned :: HalEditorPaned
                         , _gHalWindow      :: Window
                         , _gHalInfoConsole :: HalInfoConsole
                         , _gTextCode       :: SourceView
                         , _gInfoConsole    :: TextView
                         , _gHalCommConsole :: HalCommConsole
                         , _gHalForkFlag    :: MVar ()
                         , _gHalStopFlag    :: MVar ()
                         }
$(makeLenses ''HGReader)

-- | Tipo de mónada de estado, llevamos el environment de un modulo bien 
-- chequeado y la info sobre la parte derecha de la interfaz, es decir, 
-- la que contiene los campos de texto para escribir programas.
data HGState = HGState { _gHalConsoleState    :: Maybe ExecState
                       , _gHalPrg             :: Maybe ExtProgram
                       , _gCurrentText        :: SourceView
                       -- El siguiente campo es el nombre del archivo
                       -- sin la extensión.
                       -- Un archivo de Hal consistira de uno .lisa y uno .fun
                       , _gFileName    :: Maybe FilePath
                       , _gToolButtons :: Maybe HalToolBarButtons
                       , _gMenuButtons :: Maybe HalMenuButtons
                       , _gForkThread  :: Maybe ThreadId
                       }
$(makeLenses ''HGState)

-- | Referencia del estado.
type HGStateRef = IORef HGState

-- | Mónada de la interfaz.
type GuiMonad' = RWST HGReader () HGStateRef 
type GuiMonad = GuiMonad' IO

instance Reference IORef (StateT HGStateRef IO) where
    newRef = liftIO . newRef
    readRef = liftIO . readRef
    writeRef r = liftIO . writeRef r

instance Reference IORef GuiMonad where
    newRef = liftIO . newRef
    readRef = liftIO . readRef
    writeRef r = liftIO . writeRef r

-- | Retorna el estado de la mónada de la interfaz.
getHGState :: GuiMonad HGState
getHGState = get >>= readRef

-- | Actualiza el estado de la mónada de la interfaz.
updateHGState :: (HGState -> HGState) -> GuiMonad ()
updateHGState f = do
                r <- get
                gst <- readRef r
                writeRef r $ f gst
                put r

io :: MonadIO m => IO a -> m a
io = liftIO

eval :: GuiMonad a -> HGReader -> HGStateRef -> IO a
eval action content str = evalRWST action content str >>= return . fst
