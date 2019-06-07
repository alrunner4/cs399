module digipen.instructor.conway.input
import digipen.instructor.conway.spec

import Data.SortedSet

% default total

export
area0 : Area
area0 = Extents(-5,-5)(5,5)

export
vacant : Position -> Bool
vacant _ = False

private
setMembers : ( SortedSet Position -> SortedSet Position ) -> Position -> Bool
setMembers adds position = contains position ( adds empty )

export
blinker : Position -> Bool
blinker = setMembers $ insert(0,-1) . insert(0,0) . insert(0,1)

export
blocks : Position -> Bool
blocks = setMembers $
    insert(-2,-2) . insert(-1,-2) . insert(-2,-1) . insert(-1,-1) .
    insert( 2, 2) . insert( 1, 2) . insert( 2, 1) . insert( 1, 1)

export
beacon : Position -> Bool
beacon = setMembers $
    insert(-2,-2) . insert(-1,-2) . insert(-2,-1) . insert(-1,-1) .
    insert( 1, 1) . insert( 0, 1) . insert( 1, 0) . insert( 0, 0)

export
glider : Position -> Bool
glider = setMembers $ insert(-5,-3) . insert(-4,-3) . insert(-3,-3) . insert(-3,-4) . insert(-4,-5)

private
drawGen : Area -> ( Position -> Bool ) -> IO ()
drawGen area@( Extents(x1,_)(x2,_) ) liveness = do
    putStrLn( pack $ replicate( toNat $ abs( x2-x1+1 ) ) '-' )
    drawBoard area liveness

export
namedInit : String -> ( Position -> Bool )
namedInit "beacon"  = beacon
namedInit "blinker" = blinker
namedInit "blocks"  = blocks
namedInit "glider"  = glider
namedInit _         = vacant

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
