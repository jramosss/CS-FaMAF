-- | Line number demo.
--  Author      :  Andy Stewart
--  Copyright   :  (c) 2010 Andy Stewart <lazycat.manatee@gmail.com>
--
-- This demo show how to build your own line number gutter.
--
module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView

main = do
  -- Init.
  initGUI
  
  -- Create window.
  window <- windowNew
  windowSetDefaultSize window 900 600
  windowSetPosition window WinPosCenter

  -- Create source view widget.
  sourceView <- sourceViewNew

  -- Create scroll window.
  scrolledWindow <- scrolledWindowNew Nothing Nothing

  -- Insert CellRendererText in source view's gutter.
  gutter <- sourceViewGetGutter sourceView TextWindowLeft
  cell   <- cellRendererTextNew
  sourceGutterInsert gutter cell 0

  -- Set gutter data.
  sourceGutterSetCellDataFunc gutter cell $ \ c l currentLine -> do
         -- Display line number.
         set (castToCellRendererText c) [cellText := show (l + 1)]
         -- Highlight current line.
         let color = if currentLine 
                        then Color 65535 0 0 
                        else Color 0 65535 0
         set (castToCellRendererText c) [cellTextForegroundColor := color]

  -- Set gutter size.
  sourceGutterSetCellSizeFunc gutter cell $ \ c -> 
      -- -1 mean cell renderer will adjust width with chars dynamically.
      set (castToCellRendererText c) [cellTextWidthChars := (-1)]

  -- Connect and show.
  scrolledWindow `containerAdd` sourceView
  window `containerAdd` scrolledWindow
  window `onDestroy` mainQuit
  widgetShowAll window

  mainGUI
