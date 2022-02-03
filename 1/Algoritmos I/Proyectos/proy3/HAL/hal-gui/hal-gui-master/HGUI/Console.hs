{-# Language OverloadedStrings #-}
module HGUI.Console ( configInfoConsole
                    , printInfoMsg
                    , printErrorMsg
                    , printInfoMsgIO
                    , printErrorMsgIO
                    ) where

import Graphics.UI.Gtk hiding (get)

import Control.Lens hiding (set)
import Control.Monad.Trans.RWS (ask)

import HGUI.Config
import HGUI.GState
import HGUI.Utils

configConsoleTV :: TextView ->  IO ()
configConsoleTV tv  = do
        buf <- textViewGetBuffer tv
        -- Tags para el text buffer, para formatear texto:
        tagTable <- textBufferGetTagTable buf
        tag <- textTagNew (Just "ErrorScheme")
        set tag [ textTagForegroundGdk := textErrColorCommTV
                , textTagForegroundSet := True]
        textTagTableAdd tagTable tag
        
        tag' <- textTagNew (Just "InfoScheme")
        set tag' [ textTagForegroundGdk := textColorCommTV
                , textTagForegroundSet := True]
        textTagTableAdd tagTable tag'
        
        widgetModifyBase tv StateNormal backColorCommTV
        widgetModifyText tv StateNormal textColorCommTV
        widgetShowAll tv        

configInfoConsole :: GuiMonad ()
configInfoConsole = ask >>= \content -> io $ do
                    let infoTV = content ^. gInfoConsole
                    configConsoleTV infoTV

printInfoMsg :: String -> GuiMonad ()
printInfoMsg = printMsg "InfoScheme"
                
printErrorMsg :: String -> GuiMonad ()
printErrorMsg = printMsg "ErrorScheme"

printInfoMsgIO :: String -> TextView -> IO ()
printInfoMsgIO = printMsg' "InfoScheme"
                
printErrorMsgIO :: String -> TextView -> IO ()
printErrorMsgIO = printMsg' "ErrorScheme"

printMsg :: TagName -> String -> GuiMonad ()
printMsg tagname msg = ask >>= \content -> io $ do 
                       let infoTV = content ^. gInfoConsole
                       printMsg' tagname msg infoTV

printMsg' :: TagName -> String -> TextView -> IO ()
printMsg' tagname msg infoTV = do
            infoBuf <- textViewGetBuffer infoTV

            titer <- textBufferGetEndIter infoBuf
            lineStart <- textIterGetLine titer
                
            -- Ingresamos el texto en el buffer
            putStrAtEnd infoBuf msg
            
            titer' <- textBufferGetEndIter infoBuf
            lineEnd <- textIterGetLine titer'
                
            start <- textBufferGetIterAtLine infoBuf lineStart
            end <- textBufferGetIterAtLine infoBuf lineEnd
                
            textBufferApplyTagByName infoBuf tagname start end
            widgetShowAll infoTV
