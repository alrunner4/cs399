module digipen.instructor.conway.spec

public export
Position : Type
Position = ( Int, Int )

public export
Bounds : Type
Bounds = ( Position, Position )

public export
interface GameOfLife( World : Type -> Type )( Cell : Type ) where

    start : Bounds -> ( Position -> Bool ) -> World ()

    evolve :  World () -> World( Position -> Bool )
    
    countNeighbors : World Cell -> Nat

    isLive : Position -> World () -> World Bool
