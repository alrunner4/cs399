module digipen.errors

public export
data JsonFailureLocation = Value
    | Root JsonFailureLocation
    | ListElement Nat JsonFailureLocation
    | ObjectKey String JsonFailureLocation

public export
Show JsonFailureLocation where
    show Value = "value"
    show( Root sub ) = "root -> " ++ show sub
    show( ListElement n sub ) = "list[" ++ show n ++ "] -> " ++ show sub
    show( ObjectKey key sub ) = "object[" ++ show key ++ "] -> " ++ show sub

public export
data FailureMode = ParseFailure | DataFailure JsonFailureLocation

public export
Exceptional : Type -> Type
Exceptional a = Either FailureMode a

syntax FailedParse = Left ParseFailure
syntax FailedData [l] = Left( DataFailure l )
syntax NoFailure [x] = Right x
