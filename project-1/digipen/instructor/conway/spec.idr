module digipen.instructor.conway.spec

||| The coordinates of a spot in a quantized flatland.
public export
Position : Type
Position = ( Int, Int )

||| This definition of Area is a pair-like value with a constructor
||| named Extents. De/construct it like ( Extents(x1,y1)(x2,y2) ).
public export
data Area = Extents Position Position

public export
interface GameOfLife( LifeWorld : Type -> Type ) where

    ||| An opaque reference to a cell in some LifeWorld context.
    Cell : Type

    ||| Initialize a LifeWorld with a given liveness function.
    start : Area -> ( Position -> Bool ) -> LifeWorld ()

    ||| Produce a new LifeWorld state and return its liveness function.
    evolve : LifeWorld a -> LifeWorld( Position -> Bool )

    ||| Create a way to look up information about a cell within some
    ||| LifeWorld's context.
    cell : Position -> LifeWorld a -> LifeWorld Cell
    
    ||| Check how many neighbors some cell has.
    countNeighbors : LifeWorld Cell -> LifeWorld Nat

    ||| Check if some cell is alive.
    isLive : LifeWorld Cell -> LifeWorld Bool

    ||| Discard the world context from a value.
    extract : LifeWorld a -> a


||| Run this function at the repl by typing
||| ":exec drawBoard some_area_here your_liveness_function_here"
public export
drawBoard : Area -> ( Position -> Bool ) -> IO ()
drawBoard( Extents(x1,y1)(x2,y2) ) isLive = sequence_( map( putStrLn . rowString ) ys ) where

    xs : List Int
    xs = enumFromTo( min x1 x2 )( max x1 x2 )

    ys : List Int
    ys = enumFromTo( min y1 y2 )( max y1 y2 )

    rowString : Int -> String
    rowString y = pack( map(\ x => if isLive(x,y) then 'X' else ' ' ) xs )

