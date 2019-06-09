import digipen.instructor.conway.input
import digipen.instructor.conway.spec

% default total

private
mapExtents : Area -> ( Position -> a ) -> List a
mapExtents( Extents(x1,y1)(x2,y2) ) f = concatMap row ys where
    xs : List Int
    xs = enumFromTo( min x1 x2 )( max x1 x2 )
    ys : List Int
    ys = enumFromTo( min y1 y2 )( max y1 y2 )
    row : Int -> List a
    row y = map(\ x => f (x,y) ) xs

private
data Witness = Alive Position | Neighbor Position

isNeighbor : Witness -> Bool
isNeighbor( Neighbor _ ) = True
isNeighbor _             = False

private
positionOf : Witness -> Position
positionOf( Alive    p ) = p
positionOf( Neighbor p ) = p

Eq Witness where
    (==)( Alive p )( Alive q ) = p == q
    (==)( Neighbor p )( Neighbor q ) = p == q
    (==) _ _ = False

||| This Ord implementation for Witness ensures that, when sorted in ascending
||| order, all values with the same position are contiguous, and each position
||| is led by any Alive values present.
Ord Witness where
    compare( Alive p )( Alive q ) = compare p q
    compare( Neighbor p )( Neighbor q ) = compare p q
    compare( Alive p )( Neighbor q ) = case compare p q of
        EQ  => LT
        cmp => cmp
    compare( Neighbor p )( Alive q ) = case compare p q of
        EQ  => GT
        cmp => cmp

private
candidates : Area -> ( Position -> Bool ) -> List Witness
candidates area aliveAt = concat( mapExtents area neighbors ) where
    neighbors : Position -> List Witness
    neighbors(x,y) = if aliveAt(x,y) then [
            Neighbor(x-1,y-1), Neighbor(x,y-1), Neighbor(x+1,y-1),
            Neighbor(x-1,y),   Alive   (x,y),   Neighbor(x+1,y),
            Neighbor(x-1,y+1), Neighbor(x,y+1), Neighbor(x+1,y+1)
        ] else []

private
accumulateNeighborCount : ( accum : List( Nat, Witness ) ) -> Witness -> List( Nat, Witness )
accumulateNeighborCount [] _ = []
accumulateNeighborCount( ( streak , val ) :: previous ) next
    = if positionOf val == positionOf next
        then if isNeighbor next
            then ( S streak , val ) :: previous
            else (   streak , val ) :: previous
        else if isNeighbor next
            then ( S Z , next ) :: ( streak , val ) :: previous
            else (   Z , next ) :: ( streak , val ) :: previous

private
countNeighbors : List Witness -> List( Nat, Witness )
countNeighbors list = case sort list of
    []           => []
    head :: tail => foldl accumulateNeighborCount [( S Z, head )] tail

export
data Cell = CellWithNeighbors Bool Nat

private
IndexedCell : Type
IndexedCell = ( Position, Cell )

private
WitnessesToCell : ( Nat, Witness ) -> IndexedCell
WitnessesToCell( n, Alive    p ) = ( p, CellWithNeighbors True  n )
WitnessesToCell( n, Neighbor p ) = ( p, CellWithNeighbors False n )

private
countNeighborsForAll : List Witness -> List IndexedCell
countNeighborsForAll ws = map WitnessesToCell( countNeighbors ws )

export
data World a = WorldState( List IndexedCell ) Area a

public export
GameOfLife World where

    Cell = Cell

    start area aliveAt = WorldState( countNeighborsForAll $ candidates area aliveAt ) area ()

    countNeighbors( WorldState cells area ( CellWithNeighbors _ n ) ) = WorldState cells area n

    isLive( WorldState cells area ( CellWithNeighbors live _ ) ) = WorldState cells area live

    cell position ( WorldState cells area _ ) = WorldState cells area (
        maybe ( CellWithNeighbors False Z ) id ( lookup position cells )
    )

    extract( WorldState _ _ x ) = x

    evolve( WorldState cells area _ ) = WorldState newCells area viewNewWorld where

        nextCellState : Cell -> Bool
        nextCellState( CellWithNeighbors isLive neighborCount ) =
            if isLive then
                if      neighborCount < 2 then False -- underpopulation
                else if neighborCount < 4 then True  -- survival
                else                           False -- overpopulation
            else        neighborCount == 3           -- reproduction

        viewNewWorld : Position -> Bool
        viewNewWorld position = maybe False nextCellState ( lookup position cells )

        newCells : List IndexedCell
        newCells = countNeighborsForAll( candidates area viewNewWorld )


partial
main : IO ()
main = input.main World
