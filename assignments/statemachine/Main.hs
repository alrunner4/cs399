{-# OPTIONS_GHC -Weverything -Wno-unsafe #-}
module Main( main ) where
import Prelude( (+), ($), (++), Maybe( Just, Nothing ), IO, Int, show )

import qualified GI.Gtk as Gtk

import Data.IORef( newIORef, readIORef, writeIORef )
import Data.Text( Text, pack )

import Control.Monad.Trans.Except( ExceptT )
import Control.Monad.Trans.State( StateT )

main :: IO ()
main = do
  _ <- Gtk.init Nothing

  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.windowSetTitle window ( pack "DigiPen.CS399.StateMachine" )
  Gtk.setContainerBorderWidth window 10
  _ <- Gtk.onWidgetDestroy window Gtk.mainQuit

  box <- Gtk.boxNew Gtk.OrientationHorizontal 5
  Gtk.containerAdd window box

  lab <- Gtk.labelNew( Just $ pack "No clicks yet" )
  button <- Gtk.buttonNewWithLabel( pack "Click me" )
  Gtk.containerAdd box lab
  Gtk.containerAdd box button

  m <- newIORef( 0 :: Int )

  _ <- Gtk.onButtonClicked button $ do
    v <- readIORef m
    writeIORef m (v+1)
    Gtk.setLabelLabel lab ( pack $ "There have been " ++ show (v+1) ++ " clicks" )

  Gtk.widgetShowAll window
  Gtk.main

type Workflow a = ExceptT InputError (StateT WindowMode IO) a
data WindowMode = WindowModeInit | WindowModeValue Int
data InputError = InputErrorParse Text
