{-# OPTIONS_GHC -Weverything -Wno-unsafe #-}
{-# LANGUAGE
      BlockArguments,
      ExistentialQuantification,
      LambdaCase,
      OverloadedLabels,
      OverloadedStrings,
      RecordWildCards,
      ScopedTypeVariables
#-}

module Main( main ) where

-- NOTE: You may add relevant imports as needed.

import Prelude(
  Bool( True ), Maybe( Just, Nothing ), IO,
  ($), (>>), (>>=), (++), flip, fmap, not, pure, show, traverse)

-- https://hackage.haskell.org/package/gi-gtk/docs/GI-Gtk.html
import qualified GI.Gtk as Gtk
import GI.Gtk( AttrOp((:=)) )

import qualified GI.GLib as GLib

import Control.Concurrent( ThreadId, getNumCapabilities, killThread )

-- SUGGESTED READING: https://hackage.haskell.org/package/base/docs/Control-Concurrent-MVar.html
import Control.Concurrent.MVar( MVar, newEmptyMVar, newMVar, takeMVar, tryPutMVar, tryTakeMVar )

-- https://hackage.haskell.org/package/stm/docs/Control-Concurrent-STM.html
import Control.Concurrent.STM( newEmptyTMVar, newTMVar )

import Control.Monad.Fix( mfix )

import Data.Foldable( for_ )
import Data.Function( (&) )
import Data.Functor( void )
import Data.IORef( IORef, atomicModifyIORef', newIORef, readIORef )
import Data.IntMap( IntMap, delete, empty, insert, lookup, traverseWithKey )
import Data.Text( pack )
import Data.Unique( Unique )
import System.Random( randoms, newStdGen )

import CS399.Game(
  Action, Game( Game, addPlayer, newGame, readGame, simulate ),
  Player( playerUid ), Position,
  newPlayer, newShardedGame, playerId, simulate )
import CS399.Bots( randomBot )

import Debug.Trace( trace )

-- | A helper type to store the mutable communication channels from and to the player, respectively.
data PlayerFeedback = forall r. PlayerFeedback Unique ThreadId ( MVar( Action r ) ) ( MVar r )

main :: IO ()
main = do

  void $ Gtk.init Nothing

  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.set window [ #title := "DigiPen.CS399.Concurrency"
                 , #borderWidth := 10 ]
  void $ Gtk.on window #destroy Gtk.mainQuit

  nThreads <- getNumCapabilities
  header <- Gtk.new Gtk.Label [ #label := pack( "Thread Count: " ++ show nThreads ) ]
  report <- Gtk.new Gtk.Label [ #label := "A Concurrency Simulation" ]
  box <- Gtk.new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
  #add window box
  #add box header
  #add box report

  -- playerSync maps a unique 'Player' identifier to a mutable variable used to record its action
  --   asynchronously for each simulation interval, as well as a variable used to synchronize player
  --   threads to the frame rate.
  -- REQUIREMENT: Refactor playerSync to IntMap( TMVar( Action r ) ) and use tryTakeTMVar when
  --   reading them.
  playerSync <- newIORef( empty :: IntMap( IORef PlayerFeedback ) )

  Game{..} <- newShardedGame 10


  -- NOTE: recordActionFromPlayer will be called from the player thread, but should not return until
  --   the frame is finished.
  let recordActionFromPlayer :: Player -> ThreadId -> Action r -> IO r
      recordActionFromPlayer player threadId action = do

        -- NOTE: Because PlayerFeedback uses existential quantification (a forall type variable on
        --   a data constructor to introduce a variable not paramaterized by the type constructor)
        --   the type variable @r@ is only known by the caller of @recordActionForPlayer@ and the
        --   information is inaccessible outside this scope.
        actionVar <- newMVar action
        syncVar   <- newEmptyMVar
        -- NOTE: If you read closely, it may at first seem odd that we need to allocate a new
        --   @syncVar@ with each call to @recordActionFromPlayer@, but this is because each `Action`
        --   requires a different return MVar type, and we (currently) have no way to recover the
        --   type from the MVar upon re-evaluating this function.

        feedback <- newIORef( PlayerFeedback( playerUid player ) threadId actionVar syncVar )

        atomicModifyIORef' playerSync \ m -> ( insert( playerId player ) feedback m, () )

        -- NOTE: @takeMVar syncVar@ causes the player thread to wait for the main thread to generate
        --   a simulation response into the mutable variable @syncVar@.
        takeMVar syncVar


  let newBotPlayer :: Position -> IO( Maybe Player )
      newBotPlayer position = do
        player <- newPlayer position
        success <- addPlayer player
        if not success
          then pure Nothing
          else do
            randGen <- newStdGen
            void $ mfix \tid -> randomBot( recordActionFromPlayer player tid )( randoms randGen ) (-10) 10
            pure( Just player )


  let displayWorld = do
        world <- readGame
        let showPosition p = case lookup p world of
              Nothing -> '_'
              Just _  -> '*'
        let worldString = pack( fmap showPosition [-10..10] )
        Gtk.set report [ #label := worldString ]


  let mainloop :: IO Bool
      mainloop = do
        -- REQUIREMENT: We need to make sure all mutable reads and writes in each evaluation of
        --   mainLoop are collectively atomic.
        ( playerRefs :: IntMap( IORef PlayerFeedback ) ) <- readIORef playerSync
        ( playerVars :: IntMap PlayerFeedback ) <- traverse readIORef playerRefs
        void $ playerVars & traverseWithKey \ pId ( PlayerFeedback pUid tid actionVar syncVar ) -> do
          tryTakeMVar actionVar >>= \case
              Nothing -> pure ()
              Just action -> do
                deadOrResponse <- simulate pUid action
                case deadOrResponse of
                  Nothing -> trace( "Deleting thread for player " ++ show pId ) $
                    killThread tid >> atomicModifyIORef' playerSync \s -> ( delete pId s, () )
                  Just response -> void $ tryPutMVar syncVar response
                  -- NOTE: Somewhat interestingly, it's impossible for us to pattern match on
                  --   @response@ here due to existential quantification. See the corresponding NOTE
                  --   in `recordActionFromPlayer`.
        displayWorld
        trace( "END FRAME" ) $ pure True


  for_[-9..9] newBotPlayer

  Gtk.widgetShow window
  Gtk.widgetShowAll window
  displayWorld

  void $ GLib.timeoutAdd GLib.PRIORITY_DEFAULT 1000 mainloop -- run mainloop every 1000ms
  Gtk.main
