
record Writer w a where
    constructor MkWriter
    runWriter : ( a, w )

-- applyLog : Monoid m => (a, m) -> (a -> (b, m)) -> (b, m)
-- applyLog ( x, log ) fn = let ( y , newLog ) = fn x in ( y , log <+> newLog )

Functor( Writer w ) where {}
Applicative( Writer w ) where {}

Semigroup w => Monad( Writer w ) where
    -- (>>=) : Writer w a -> ( a -> Writer w b ) -> Writer w b
    (>>=) ( MkWriter ( x, log ) ) fn = let
        MkWriter ( y , newLog ) = fn x
        in MkWriter( y , log <+> newLog )

incHalf : Int -> Writer ( Maybe Int ) Int
incHalf x = let y = x + 1
    in MkWriter( y, if y `mod` 2 == 0 then Just( y `div` 2 ) else Nothing )

doOperations : Writer ( Maybe Int ) Int
doOperations = do
    z <- incHalf 4
    q <- incHalf z
    incHalf q

mightError : Either String Int
mightError = Left "don't have any good ints"

calculate : Int -> String
calculate x = "Hi " ++ show x

dependentOperation : String -> Either String Nat
dependentOperation str = let l = length str in if l < 5
    then Left "too short"
    else Right l

doWithPossibilityOfError : Either String Nat
doWithPossibilityOfError = do
    x <- mightError -- : Either error a
    let y = calculate x
    dependentOperation y -- : a -> Either error a
