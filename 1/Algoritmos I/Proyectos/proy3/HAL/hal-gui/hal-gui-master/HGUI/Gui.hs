module HGUI.Gui where

import Graphics.UI.Gtk hiding (get)

import Control.Concurrent

import Control.Lens hiding (set)
import Control.Monad ( void )
import Control.Monad.Trans.RWS
import Data.Reference

import HGUI.TextPage
import HGUI.File
import HGUI.Console
import HGUI.EvalConsole
import HGUI.GState
import HGUI.Config

mainHalGui :: Builder -> IO ()
mainHalGui xml = do
                (gReader,gState) <- makeGState xml

                _ <- runRWST (do configWindow
                                 configMenuBarButtons xml
                                 configToolBarButtons xml
                                 configInfoConsole
                                 eventsSourceView
                                 eventsInfoConsole
                                 configEvalConsole
                             ) gReader gState

                return ()

-- | Genera el estado inicial de la mónada.
makeGState :: Builder -> IO (HGReader,HGStateRef) 
makeGState xml = do
        
        edPaned <- builderGetObject xml castToVPaned "edPaned"
        
        window <- builderGetObject xml castToWindow "mainWindow"

        infoTV <- builderGetObject xml castToTextView "infoConsoleTView"
        
        evalBox      <- builderGetObject xml castToVBox "evalBox"
        evalStateBox <- builderGetObject xml castToVBox "evalStateBox"
        stateBox     <- builderGetObject xml castToVBox "stateBox"
        evalB        <- builderGetObject xml castToToggleToolButton "evalButton"
        
        stepDB   <- builderGetObject xml castToButton "stepDownButton"
        contB    <- builderGetObject xml castToButton "contButton"
        breakB   <- builderGetObject xml castToButton "breakButton"
        restartB <- builderGetObject xml castToButton "restartButton"
        cleanB   <- builderGetObject xml castToButton "cleanButton"
        stopB    <- builderGetObject xml castToButton "stopButton"
        
        boxLisa <- builderGetObject xml castToVBox "boxLisaCode"
        
        textcode <- createSourceView halLangInfo
        
        configText boxLisa textcode
        
        forkFlag <- newEmptyMVar
        stopFlag <- newMVar ()
        
        let halToolbarST   = HalToolbar evalB
            halEditorPaned = HalEditorPaned edPaned
            halCommConsole = HalCommConsole evalBox evalStateBox stateBox
                                            stepDB contB breakB 
                                            restartB cleanB stopB
        
        gState <- newRef $ 
                      HGState Nothing
                              Nothing
                              textcode
                              Nothing
                              Nothing
                              Nothing
                              Nothing
                              
        let gReader = HGReader halToolbarST
                               halEditorPaned
                               window
                               (HalInfoConsole infoTV)
                               textcode
                               infoTV
                               halCommConsole
                               forkFlag
                               stopFlag
        
        return (gReader,gState)

-- | Configura los botones de la barra, tales como abrir, cerrar, etc...
configToolBarButtons :: Builder -> GuiMonad ()
configToolBarButtons xml = ask >>= \content -> get >>= \st ->
        do
        
        newFButton      <- io $ bGetObject "newHFileButton"
        openFButton     <- io $ bGetObject "openHFileButton"
        saveFButton     <- io $ bGetObject "saveHFileButton"
        saveAtFButton   <- io $ bGetObject "saveHFileAtButton"
        evaButton       <- io $ bGetObject "evalButton"
        
        void $ io $ onTBClicked newFButton (eval createNewFile content st)
        void $ io $ onTBClicked openFButton (eval openFile content st)
        void $ io $ onTBClicked saveFButton (eval saveFile content st)
        void $ io $ onTBClicked saveAtFButton (eval saveAtFile content st)
        void $ io $ onTBClicked evaButton (eval configEvalButton content st)
        
        let buttons = HalToolBarButtons newFButton
                                        openFButton
                                        saveFButton
                                        saveAtFButton

        updateHGState (gToolButtons .~ Just buttons)

        return ()
    where
        onTBClicked = onToolButtonClicked
        bGetObject  = builderGetObject xml castToToolButton

configMenuBarButtons :: Builder -> GuiMonad ()
configMenuBarButtons xml = ask >>= \content -> get >>= \st ->
            do
            let window = content ^. gHalWindow
            
            newB    <- io $ builderGetObject xml castToMenuItem "newButton"
            openB   <- io $ builderGetObject xml castToMenuItem "openButton"
            saveB   <- io $ builderGetObject xml castToMenuItem "saveButton"
            saveAsB <- io $ builderGetObject xml castToMenuItem "saveAsButton"
            quitB   <- io $ builderGetObject xml castToMenuItem "quitButton"
            
            _ <- io $ onActivateLeaf newB    $ eval createNewFile    content st
            _ <- io $ onActivateLeaf openB   $ eval openFile         content st
            _ <- io $ onActivateLeaf saveB   $ eval saveFile         content st
            _ <- io $ onActivateLeaf saveAsB $ eval saveAtFile       content st
            _ <- io $ onActivateLeaf quitB   $ widgetDestroy window

            let buttons = HalMenuButtons newB
                                         openB
                                         saveB
                                         saveAsB

            updateHGState (gMenuButtons .~ Just buttons)
            
            return ()

-- | Configura la ventana principal.
configWindow :: GuiMonad ()
configWindow = ask >>= \content -> get >>= \stref ->
            io $ do
            let window   = content ^. gHalWindow
            
            windowMaximize window
            widgetShowAll window
            _ <- onDestroy window $ mainQuit >> forkQuit stref
            return ()
    where
        forkQuit :: HGStateRef -> IO ()
        forkQuit stref = readRef stref >>= \st ->
            let mthreadId = st ^. gForkThread in
            maybe (return ()) killThread mthreadId

eventsInfoConsole :: GuiMonad ()
eventsInfoConsole = ask >>= \content ->
        let infoTV = content ^. gInfoConsole
        in io $ void $
           on infoTV sizeAllocate $
            \_ -> do
            Just vp     <- widgetGetParent infoTV
            Just infoSW <- widgetGetParent vp
            vAdj   <- scrolledWindowGetVAdjustment $ castToScrolledWindow infoSW
            pSize  <- adjustmentGetPageSize vAdj
            pUpper <- adjustmentGetUpper vAdj
            set vAdj [ adjustmentValue := pUpper - pSize ]

eventsSourceView :: GuiMonad ()
eventsSourceView = ask >>= \content ->
                   get >>= \st ->
    do
        let textcode = content ^. gTextCode
    
        _ <-  io (textcode `on` buttonPressEvent $ io $
                eval (updateHGState ((.~) gCurrentText textcode)) content st >>
                return False)
        return ()
