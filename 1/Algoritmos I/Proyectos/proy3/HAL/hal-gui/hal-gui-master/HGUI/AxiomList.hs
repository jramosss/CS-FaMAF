-- | Configuración de la lista de axiomas.
module HGUI.AxiomList where

import Equ.Proof (Basic(..),Truth (..)) 
import Equ.Rule (Relation,relRepr)
import Equ.Theories (relationList)
import qualified Equ.Theories as ET (axiomGroup,Grouped,toForest) 

import Graphics.UI.Gtk hiding (eventButton, eventSent,get)
import Graphics.UI.Gtk.SourceView

import Data.Text(unpack,pack)
import Data.Tree

import Control.Lens hiding (set)
import Control.Monad(unless)
import Control.Monad.Trans.RWS (evalRWST,ask,get)
import qualified Data.Foldable as F (mapM_) 

import HGUI.GState

type AxiomItem = (String,Maybe Basic)

-- | Genera el TreeStore con la lista de axiomas.
listAxioms :: IO (TreeStore AxiomItem)
listAxioms = treeStoreNew $ forest ET.axiomGroup ++ forest laEval
    where 
        laEval :: ET.Grouped Basic
        laEval = [(pack "Aritmética", [Evaluate])]

        forest ::  (Truth t, Show t) => ET.Grouped t -> Forest AxiomItem
        forest = ET.toForest (\t -> (unpack t,Nothing)) addItem

        addItem :: (Truth t, Show t) => t -> AxiomItem
        addItem t = (unpack $ truthName t,Just $ truthBasic t)

-- | Configuración del botón para activar/desactivar la lista de axiomas.
configAxFrameButton :: GuiMonad ()
configAxFrameButton = do
                content <-  ask
                let af          = content ^. (gHalAxList . gAxFrame)
                let afButton    = content ^. (gHalToolbar . axFrameB)
                
                active <- io $ toggleToolButtonGetActive afButton
                if active 
                   then io $ widgetShowAll af
                   else io $ widgetHideAll af

-- | Configuración general de la lista de axiomas.
configAxiomList :: GuiMonad ()
configAxiomList = do
            content <- ask
            let af  = content ^. (gHalAxList . gAxFrame)
            let tv  = content ^. (gHalAxList . gAxTreeView)
            let axr = content ^. (gHalAxList . gAxRel)
            
            list <- io relationListStore
            setupComboRel axr list
            
            list' <- io listAxioms
            io $ setupAxiomList tv list'
            eventsAxiomList tv list'
            io $ widgetHideAll af
            
            return ()

-- | Configuración del treeview de axiomas.
setupAxiomList :: TreeView -> TreeStore AxiomItem -> IO ()
setupAxiomList tv list = 
    treeViewGetColumn tv 0 >>=
    F.mapM_ (\c -> treeViewRemoveColumn tv c) >>
    treeViewColumnNew >>= \col ->
    treeViewSetHeadersVisible tv False >>
    treeViewSetModel tv list >>
    cellRendererTextNew >>= \renderer ->
    cellLayoutPackStart col renderer False >>
    cellLayoutSetAttributes col renderer list (\ind -> [ cellText := fst ind ]) >>
    treeViewAppendColumn tv col >>
    return ()

-- | Configuración de los eventos del treeview de axiomas.
eventsAxiomList :: TreeView -> TreeStore AxiomItem -> GuiMonad ()
eventsAxiomList tv list = 
            io (treeViewGetSelection tv >>= \tree -> 
            treeSelectionSetMode tree SelectionSingle >>
            treeSelectionUnselectAll tree >>
            treeViewSetModel tv list >> widgetShowAll tv >> return tree) 
            >>= \tree -> ask >>= \content -> get >>= \st ->
            io (onSelectionChanged tree (eeval (showAxiom list tree) content st))
            >> 
            io (tv `on` rowActivated $ \path _ -> eeval (putOnText list path) content st) >>
            return ()
    where
        eeval action content st = evalRWST action content st >> return ()


putOnText :: TreeStore AxiomItem -> TreePath -> GuiMonad ()
putOnText list path = 
        unless (length path == 1) $ getHGState >>= \st ->
            return (st ^. gCurrentText) >>= \tv ->
            configSelection path tv
    where
        justification :: [Relation] -> Int -> String -> String
        justification rs i j = (unpack $ relRepr (rs!!i)) ++ " { " ++ j ++ " }"
        configSelection :: TreePath -> SourceView -> GuiMonad ()
        configSelection tpath tv = ask >>= \content -> 
                return (content ^. (gHalAxList . gAxRel)) >>= \axRel ->
                io (treeStoreGetValue list tpath) >>= \(ax,_) ->
                io (relationListStore) >>= \lsrel ->
                io (listStoreToList lsrel) >>= \l ->
                io (comboBoxGetActive axRel) >>= \i ->
                addToCursorBuffer tv $ justification l i ax
        addToCursorBuffer :: SourceView -> String -> GuiMonad ()
        addToCursorBuffer tv repr = io $ do
                buf <- textViewGetBuffer tv
                textBufferInsertAtCursor buf repr
                widgetGrabFocus tv
        
showAxiom :: TreeStore AxiomItem -> TreeSelection -> GuiMonad ()
showAxiom list tree = io (treeSelectionGetSelectedRows tree) >>= \sel ->
            unless (null sel) $ return (head sel) >>= \h ->
            unless (length h == 1) $ 
            ask >>= \content -> 
            return (content ^. (gHalAxList . gAxLabelExpr)) >>= \lab ->
            io (treeStoreGetValue list h >>= \(_,Just basic) ->
            labelSetText lab (show $ truthExpr basic))

-- | ListStore de símbolos de relación.
relationListStore :: IO (ListStore Relation)
relationListStore = listStoreNew relationList

-- | Configuración del comboBox de relaciones.
setupComboRel :: ComboBox -> ListStore Relation -> GuiMonad ()
setupComboRel combo list = io $ do
    renderer <- cellRendererTextNew
    cellLayoutPackStart combo renderer False
    cellLayoutSetAttributes combo renderer list
                  (\ind -> [cellText := unpack $ relRepr ind])
    comboBoxSetModel combo (Just list)
    comboBoxSetActive combo 0