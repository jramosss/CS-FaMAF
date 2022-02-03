module Main where

import Graphics.UI.Gtk hiding (get)

import HGUI.Gui

main :: IO ()
main = do 
    _ <- initGUI
    
    xml <- builderNew
    builderAddFromFile xml "hal.ui"
    
    mainHalGui xml
    
    mainGUI
