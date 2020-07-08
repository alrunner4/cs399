{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BlockArguments, RankNTypes #-}
module CS399.Bots( randomBot ) where
import CS399.Game( Action( Attack, MoveTo ), AttackDirection( Up, Down ) )

-- https://hackage.haskell.org/package/base/docs/Control-Concurrent.html
import Control.Concurrent( ThreadId, forkIO )

import Data.Foldable( traverse_ )
import Data.Functor( void )
import Prelude( IO, Int, ($), (+), (-), (==), (++), mod, show )

import Debug.Trace( trace )

-- | @randomBot@ requires a function to run its action, an infinite list of random integers, and
--     lower and upper bounds for its move attempts.
randomBot :: ( forall r. Action r -> IO r ) -> [Int] -> Int -> Int -> IO ThreadId
randomBot runAction randInts low high = forkIO( traverse_ selectAction randInts ) where
  selectAction :: Int -> IO ()
  selectAction randInt = do
    case randInt `mod` 2 of
      0 -> let randPos = randInt `mod` ( high - low ) + low
           in  trace( "randomBot: MoveTo " ++ show randPos ) $ void $ runAction( MoveTo randPos )
      _ -> let direction = if randInt `mod` 2 == 0 then Up else Down
           in  trace( "randomBot: Attack " ++ show direction ) $ void $ runAction( Attack direction )
