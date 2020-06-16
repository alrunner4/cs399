{-# OPTIONS_GHC -Weverything -Wno-unsafe #-}
{-# LANGUAGE BlockArguments #-}
module Main( main ) where

-- You may import additional definitions from Prelude as necessary.
import Prelude(
  Applicative( pure ), Eq( (==) ), Monad(..), Num(..), Ord(..),
  Either(Left, Right), Maybe( Just, Nothing ), IO, Int,
  ($), (++), show, undefined)

-- https://hackage.haskell.org/package/gi-gtk/docs/GI-Gtk.html
import qualified GI.Gtk as Gtk

-- https://hackage.haskell.org/package/base/docs/Data-Functor.html
import Data.Functor( void )

-- https://hackage.haskell.org/package/base/docs/Data-IORef.html
import Data.IORef( IORef, newIORef, readIORef, writeIORef )

-- https://hackage.haskell.org/package/text/docs/Data-Text.html
import Data.Text( Text, pack )

-- https://hackage.haskell.org/package/base/docs/Control-Monad.html
import Control.Monad( when )

-- https://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-Class.html
import Control.Monad.Trans.Class( lift )

-- https://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-Except.html
import Control.Monad.Trans.Except( ExceptT, catchE, runExceptT, throwE )

main :: IO ()
main = do
  void $ Gtk.init Nothing

  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.windowSetTitle window ( pack "DigiPen.CS399.StateMachine" )
  Gtk.setContainerBorderWidth window 10
  void $ Gtk.onWidgetDestroy window Gtk.mainQuit

  box <- Gtk.boxNew Gtk.OrientationHorizontal 5
  Gtk.containerAdd window box

  let addToBox :: Gtk.IsWidget w => w -> IO w
      addToBox = \widget -> do
        Gtk.widgetSetHalign widget Gtk.AlignCenter
        Gtk.containerAdd box widget
        pure widget
  button_minus  <- Gtk.buttonNewWithLabel( pack "-" )          >>= addToBox
  button_plus   <- Gtk.buttonNewWithLabel( pack "+" )          >>= addToBox
  button_select <- Gtk.buttonNewWithLabel( pack "Select" )     >>= addToBox
  message_label <- Gtk.labelNew( Just $ pack "No clicks yet" ) >>= addToBox

  -- REQUIREMENT: Change the type of the variable clicks from an Int to WorkflowState, starting in
  --   the WorkflowClicks state. This will require changes beyond this newIORef.
  clicks <- newIORef( 0 :: Int )

  -- REQUIREMENT: refactor these onButtonClicked handlers for button_plus and button_minus to
  --   deduplicate the calls to readIORef, writeIORef, and setLabelLabel. Use the
  --   onWorkflowButtonClicked function defined below.
  --   HINT: pass the remaining behavior as a parameter to the refactored function.

  void $ Gtk.onButtonClicked button_plus do
    count <- readIORef clicks
    writeIORef clicks ( count+1 )
    Gtk.setLabelLabel message_label ( pack $ "Counted value: " ++ show( count+1 ) )

  void $ Gtk.onButtonClicked button_minus do
    count <- readIORef clicks
    writeIORef clicks ( count-1 )
    Gtk.setLabelLabel message_label ( pack $ "Counted value: " ++ show( count-1 ) )

  onWorkflowButtonClicked button_select do
    count <- lift( readIORef clicks )
    when( count < 0 )( throwE BadSelection )
    -- REQUIREMENT: Augment this click handler so that clicks when count >= 0 cause the message_label
    --   to update to display "Final Selection: <count>", and the clicks reference must update to
    --   WorkflowSelected. Ensure that subsequent clicks of button_plus and button_minus throw an
    --   exception in ExceptT with the DuplicateSelection error.

  Gtk.widgetShow window
  Gtk.widgetShowAll box

  -- The GI.Gtk.main function runs the event-handling loop.
  -- https://hackage.haskell.org/package/gi-gtk/docs/GI-Gtk-Functions.html#v:main
  Gtk.main

type Workflow a = ExceptT InputError IO a

-- GIVEN: No need to change onWorkflowButtonClicked.
onWorkflowButtonClicked :: Gtk.Button -> Workflow () -> IO ()
onWorkflowButtonClicked button action = void $ Gtk.onButtonClicked button do
  result <- runExceptT action
  case result of
    Right _  -> pure ()
    Left err -> do
      msgbox <- Gtk.dialogNew
      Gtk.setContainerBorderWidth msgbox 10
      errtxt <- Gtk.labelNew( Just $ prettyError err )
      Gtk.dialogGetContentArea msgbox >>= \box -> Gtk.containerAdd box errtxt
      Gtk.widgetShowAll msgbox

-- REQUIREMENT: Add a third state, WorkflowInitial, to represent the state of the application before
--   any input has happened. Clicking button_submit while in this state should throw a BadSelection.
--   Clicking either button_plus or button_minus should transition to the WorkflowClicks state.
data WorkflowState = WorkflowClicks Int | WorkflowSelected Int

data InputError = BadSelection | DuplicateSelection Int Int
prettyError :: InputError -> Text
prettyError e = pack case e of
  BadSelection -> "Sorry, that selection is not allowed."
  DuplicateSelection x y -> "Sorry, you can't select " ++ show x ++
                            " since you've already selected " ++ show y ++ "."

