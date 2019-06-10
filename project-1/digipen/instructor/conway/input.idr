module digipen.instructor.conway.input
import digipen.instructor.conway.spec

import Data.SortedSet

% default total

export
area0 : Area
area0 = Extents(-5,-5)(5,5)

private
setMembers : ( SortedSet Position -> SortedSet Position ) -> Position -> Bool
setMembers adds position = contains position ( adds empty )

private
drawGen : Area -> ( Position -> Bool ) -> IO ()
drawGen area@( Extents(x1,_)(x2,_) ) liveness = do
    putStrLn( pack $ replicate( toNat $ abs( x2-x1+1 ) ) '-' )
    drawBoard area liveness

export
namedInit : String -> ( Position -> Bool )

namedInit "beacon"  = setMembers $
    insert(-2,-2) . insert(-1,-2) . insert(-2,-1) . insert(-1,-1) .
    insert( 1, 1) . insert( 0, 1) . insert( 1, 0) . insert( 0, 0)

namedInit "blinker" = setMembers $
    insert(0,-1) . insert(0,0) . insert(0,1)

namedInit "blocks"  = setMembers $
    insert(-2,-2) . insert(-1,-2) . insert(-2,-1) . insert(-1,-1) .
    insert( 2, 2) . insert( 1, 2) . insert( 2, 1) . insert( 1, 1)

namedInit "glider"  = setMembers $
    insert(-5,-3) . insert(-4,-3) . insert(-3,-3) . insert(-3,-4) . insert(-4,-5)

namedInit _         = const False

export partial
main : ( life : Type -> Type ) -> GameOfLife life => IO ()
main lifeType = do
    [ _, initName, n ] <- getArgs
        | _ => putStrLn "usage: <starting state name> <number of generations>"
    let gens = cast n { to = Nat }
    let init = namedInit initName
    putStrLn "initial world"
    drawGen area0 init
    let world1 : lifeType ( Position -> Bool ) = evolve( start area0 init )
    sequence_ (
        map( drawGen area0 . extract ) $ Stream.take gens ( iterate evolve world1 )
    )
