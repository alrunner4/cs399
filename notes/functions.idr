module functions

%access public export

data MyNat = Zero | Successor MyNat

myLength : Maybe String -> Nat
myLength( Just someString ) = Strings.length someString
myLength Nothing            = 0

myLengthProcedural : Maybe String -> Nat
myLengthProcedural = maybe 0 length

fromInteger : Integer -> MyNat
fromInteger 1 = Zero
fromInteger _ = Successor( Successor( Zero ) )

record StructLike a where
	constructor MakeIt
	-- repl: interestingCalculation : StructLike a -> a -> String
	interestingCalculation : a -> String
	carriesSomething : Maybe a

{-
typedef template <typename a> struct {
	std::string (*interestingCalculation)( a );
	a carriesSomething;
} StructLike;
-}	

somethingInteresting : { a : Type } -> StructLike a -> String
somethingInteresting {a} s = maybe "was empty" ( interestingCalculation s ) ( carriesSomething s )

getRecord : StructLike a -> String
getRecord( MakeIt calc stuff ) = ?x

