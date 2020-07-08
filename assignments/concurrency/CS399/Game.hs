{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BlockArguments, GADTs, LambdaCase, RankNTypes, RecordWildCards #-}
module CS399.Game (
  Action( Attack, MoveTo, ViewPos ), AttackDirection( Up, Down ), AttackResult,
  Game( Game, addPlayer, newGame, readGame, simulate ), MutableIntMap,
  Player( playerHealth, playerPosition, playerUid ), Position,
  SomeAction( SomeAction ),
  hitPlayer, newPlayer, newPureGame, newShardedGame, playerId
) where

import Control.Concurrent.STM( STM, atomically )
import Control.Monad.Trans.Class( lift )
import Control.Monad.Trans.Maybe( MaybeT( MaybeT ), runMaybeT )
import Control.Monad.Trans.State( State, get, gets, put )

import Data.Functor( (<&>) )
import Data.IORef( IORef, atomicModifyIORef', atomicWriteIORef, newIORef, readIORef )
import Data.IntMap( IntMap, adjust, delete, empty, insert, lookup, mapMaybe, unions )
import Data.Maybe( maybe )
import Data.Traversable( for )
import Data.Unique( Unique, hashUnique, newUnique )

import Prelude(
  Applicative( pure ), Bool( True, False ), Eq( (==) ), IO, Int, Maybe( Just, Nothing ), Num( (+), (-) ), Show,
  (.), ($), (<$>), (>>=), (>>), (++), div, flip, fst, id, show, snd, traverse )

import Debug.Trace( trace )

type Position = Int
type Health = Int

-- | Players have a 'Unique' identifier, 'Position' and 'Health'
data Player = Player {
  playerHealth   :: Health,
  playerPosition :: Position,
  playerUid      :: Unique
}

playerId :: Player -> Int
playerId = hashUnique . playerUid

newPlayer :: Position -> IO Player
newPlayer position = Player 2 position <$> newUnique

hitPlayer :: Player -> Player
hitPlayer player = player { playerHealth = ( playerHealth player ) - 1 }

data AttackResult = Miss | HitPlayer Unique | KilledPlayer Unique
data AttackDirection = Up | Down
  deriving (Show)

attackOffset :: AttackDirection -> Int -> Int
attackOffset Up = (+1)
attackOffset Down = (+(-1))

data Action r where
  MoveTo  :: Position -> Action Bool
  ViewPos :: Position -> Action( Maybe Player )
  Attack  :: AttackDirection -> Action AttackResult

newtype SomeAction = SomeAction( forall r. Action r )

data Game f = Game {
  -- | Create a new Game in Functor f.
  newGame   :: f( Game f ),
  -- | Simulate an action for a player in Functor f, returning an action result or Nothing if the
  --     player is dead.
  simulate  :: forall r. Unique -> Action r -> f ( Maybe r ),
  -- | Expose the current game state through Functor f.
  readGame  :: f( IntMap Player ),
  -- | Add a player to the specified position if it's unoccupied.
  addPlayer :: Player -> f Bool
}

-- | @WorldMap@ maps world position to `playerUid`.
type WorldMap = IntMap Unique

-- | @PlayerMap@ maps `playerId` to `Player` value.
type PlayerMap = IntMap Player

type PureGame = ( PlayerMap, WorldMap )

-- | An implementation of `Game` in the pure `State` monad for reference purposes (unused).
newPureGame :: State PureGame ( Game( State PureGame ) )
newPureGame = pure $ Game {

  newGame = newPureGame,

  readGame = do
    ( players, world ) <- get
    pure $ mapMaybe (\pUid -> lookup( hashUnique pUid ) players ) world,

  simulate = \ pUid action -> do
    ( players, world ) <- get
    case lookup( hashUnique pUid ) players of

      -- This player must have died.
      Nothing -> pure Nothing

      Just player@( Player _ pPos pUid ) -> case action of

        MoveTo newPosition -> Just <$> case lookup newPosition world of
          Just _  -> pure False -- @newPosition@ is already occupied.
          Nothing -> do
            put( players, insert newPosition pUid ( delete pPos world ) )
            pure True

        ViewPos position -> pure $ lookup position world <&>
            \viewedUid -> lookup( hashUnique viewedUid ) players

        Attack direction -> let adjacentPosition = attackOffset direction pPos in
          Just <$> case lookup adjacentPosition world of
            Nothing -> pure Miss
            Just adjacentUid -> let adjacentId = hashUnique adjacentUid in
              case lookup adjacentId players of
                Nothing -> pure Miss -- the player we're attacking is already dead.
                Just adjacentPlayer -> if playerHealth adjacentPlayer == 1
                  then do
                    put( delete adjacentId players, delete adjacentPosition world )
                    pure( KilledPlayer adjacentUid )
                  else do
                    put( adjust hitPlayer adjacentId players, world )
                    pure( HitPlayer adjacentUid ),

  addPlayer = \player -> get >>= \( players, world ) -> case lookup( playerId player ) players of
    Nothing -> do
      put(
        insert( playerId player ) player players,
        insert( playerPosition player )( playerUid player ) world )
      pure True
    _ -> pure False -- The given `playerId` is already present.

}


-- REQUIREMENT: Refactor @MutableIntMap@ to use `TVar` rather than `IORef`.
type MutableIntMap a = IORef( IntMap a )

-- | A simple scheme for sharding: the outer IntMap is keyed on shard number, and the inner is keyed
--     on absolute position.
--   * Shard number 0 has only one position: 0.
--   * Shard number 1 stores positions [1..width], inclusive.
--   * Shard number -1 stores positions [-1..-width], inclusive.
type ShardedGameMap = MutableIntMap Shard
type Shard = MutableIntMap Unique
type Players = MutableIntMap Player

-- | A helper function since we read a mutable map here a few times.
--   HINT: refactor this function to lookupSTM :: Int -> TVar( IntMap a _ ) -> STM( Maybe a )
lookupIORef :: Int -> IORef( IntMap a ) -> IO( Maybe a )
lookupIORef key mapR = lookup key <$> readIORef mapR

-- | REQUIREMENT: refactor @newShardedGame@ from `IO` to `STM`. Rename the refactored function
--     @newSTMShardedGame@ and use the `stmGameToIO` function defined below to re-implement
--     @newShardedGame@ in terms of @newSTMShardedGame@.
newShardedGame :: Int -> IO( Game IO )
newShardedGame width = do
  
  shards  <- newIORef( empty :: IntMap Shard  )
  players <- newIORef( empty :: IntMap Player )

  -- REQUIREMENT: Refactor `maybeShardForPosition` to use `STM` rather than `IO`.
  let maybeShardForPosition :: Position -> ( Int -> Maybe Shard -> IO a ) -> IO a
      maybeShardForPosition position op = let shardNum = position `div` width in
        lookupIORef shardNum shards >>= op shardNum

  -- REQUIREMENT: Refactor withShardForPosition to use STM rather than IO. 
  let withShardForPosition :: Position -> ( Shard -> IO a ) -> IO a
      withShardForPosition position op = maybeShardForPosition position \shardNum -> \case
        Just existingShard -> op existingShard
        Nothing -> do
          newShard <- newIORef empty
          atomicModifyIORef' shards \s -> ( insert shardNum newShard s, () )
          op newShard

  pure Game {

    newGame = newShardedGame width,

    -- The sharded readGame flattens a ShardedGameMap into a single map of the world.
    -- REQUIREMENT: ensure all mutable variable reads in readGame are collectively atomic.
    readGame = do
      currentShards   <- readIORef shards
      currentPlayers  <- readIORef players
      playerPositions <- unions <$> traverse readIORef currentShards
      pure $ mapMaybe(\ pUid -> lookup( hashUnique pUid ) currentPlayers ) playerPositions,

    -- NOTE: If you're curious about what actions the bots are running, try adding a helper function
    --   to call Debug.Trace.trace from the simulate function, logging the `playerId` and `Action`
    --   parameters.
    simulate = \ pUid action -> do

      currentPlayers <- readIORef players

      case lookup( hashUnique pUid ) currentPlayers of
        Nothing -> pure Nothing
        Just player -> case action of

          MoveTo position -> Just <$> withShardForPosition position \shard -> do
            lookupIORef position shard >>= \case
              Just _  -> pure False
              Nothing -> atomicModifyIORef' shard \s ->
                ( insert position pUid ( delete( playerPosition player ) s ), True )

          ViewPos position -> Just <$> maybeShardForPosition position \_ -> \case
            Nothing    -> pure Nothing
            Just shard -> lookupIORef position shard <&>
              maybe Nothing (\ pUid -> lookup( hashUnique pUid ) currentPlayers )

          Attack direction -> let
            adjacentPosition = attackOffset direction ( playerPosition player )
            in Just <$> maybeShardForPosition adjacentPosition \_ mShard -> maybe Miss id <$> runMaybeT do
              shard <- liftMaybe mShard
              currentShard <- lift( readIORef shard )
              adjacentUid <- liftMaybe( lookup adjacentPosition currentShard )
              let adjacentId = hashUnique adjacentUid
              adjacentPlayer <- liftMaybe( lookup adjacentId currentPlayers )  
              lift $ if playerHealth adjacentPlayer == 1
                then trace( "Killed Player " ++ show( playerId adjacentPlayer ) ) do
                  atomicWriteIORef players ( delete adjacentId currentPlayers )
                  atomicWriteIORef shard   ( delete adjacentPosition currentShard )
                  pure( KilledPlayer adjacentUid )
                else trace( "Hit Player " ++ show( playerId adjacentPlayer ) ) do
                  atomicWriteIORef players ( adjust hitPlayer adjacentId currentPlayers )
                  pure( HitPlayer adjacentUid ),

    addPlayer = \player@( Player _ pPos pUid ) -> withShardForPosition pPos \shard ->
      lookupIORef pPos shard >>= \case
        Just _  -> pure False
        Nothing -> do
          atomicModifyIORef' shard   \s -> ( insert  pPos              pUid   s , ()   )
          atomicModifyIORef' players \p -> ( insert( hashUnique pUid ) player p , True )

  }

liftMaybe :: Applicative m => Maybe a -> MaybeT m a
liftMaybe mVal = MaybeT( pure mVal )

stmGameToIO :: Game STM -> IO( Game IO )
stmGameToIO stmGame = pure Game {
  newGame = stmGameToIO stmGame,
  simulate = \ p a -> atomically( simulate stmGame p a ),
  readGame = atomically( readGame stmGame ),
  addPlayer = atomically . addPlayer stmGame
}

