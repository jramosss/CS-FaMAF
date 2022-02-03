{-# LANGUAGE NoMonomorphismRestriction #-}
-- Módulo para definir funciones útiles, generales a la interfaz
module HGUI.Utils where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView

import Control.Applicative

import HGUI.GState

textBufferInsertLn :: TextBufferClass self => self -> String -> IO ()
textBufferInsertLn buf str = textBufferGetEndIter buf >>= \titer ->
                             textBufferInsert buf titer ('\n':str)

getCode :: SourceView -> GuiMonad String
getCode tv = io $ 
        do
        let textV = castToTextView tv
        buf       <- textViewGetBuffer textV
        start     <- textBufferGetStartIter buf
        end       <- textBufferGetEndIter buf
        textBufferGetText buf start end False

-- | Inserta un string al final de un text buffer y scrollea el text view.
putStrAtEnd :: TextBuffer -> String -> IO ()
putStrAtEnd buf msg = do
        textBufferInsertLn buf msg
        textBufferInsertLn buf ""

-- | Pone un mensaje en una área de estado.
putMsgSB :: Statusbar -> ContextId -> String -> IO ()
putMsgSB st cid m = statusbarPush st cid m >> return ()
                 
-- | 
setLoadedModuleInfo :: Label -> Maybe String -> IO ()
setLoadedModuleInfo label Nothing = labelSetText label "Error al cargar el módulo" >>
                                    styleInfoError >>= widgetModifyFont label
setLoadedModuleInfo label (Just modN) = styleInfoModule >>= widgetModifyFont label >>
                                       labelSetText label modN

-- -- | Estilo para títulos en info-boxes
styleInfoModule ::  IO (Maybe FontDescription)
styleInfoModule = Just <$> fontBold

styleInfoError :: IO (Maybe FontDescription)
styleInfoError = Just <$> fontItalic

fontItalic :: IO FontDescription
fontItalic = fontDescriptionNew >>= \fd -> 
             fontDescriptionSetStyle fd StyleItalic >>
             return fd

fontBold :: IO FontDescription
fontBold = fontDescriptionNew >>= \fd -> 
           fontDescriptionSetWeight fd WeightBold >>
           return fd
