module digipen.expect
import digipen.errors

import Language.JSON

public export
data Expectation : Type -> Type where
    ExpectString  : Maybe String -> Expectation String
    ExpectNumber  : Maybe Double -> Expectation Double
    ExpectBoolean : Maybe Bool   -> Expectation Bool
    ExpectArray   : Expectation a -> Expectation( List a )
    ExpectObject  : Expectation( List( String, JSON ) )
    ExpectAny     : Expectation JSON

public export
data Fulfillment a = Fulfilled( Expectation a ) a
    | Abandoned( Expectation a ) JsonFailureLocation

{-

-- Exercise for the reader:
toExceptional : Fulfillment a -> Exceptional a

-- An alternative representation of Fulfillment.
-- Compare to Exceptional.
Fulfillment : Type -> Type
Fulfillment a = Either( Expectation a, JsonFailureLocation )( Expectation a, a )

-}

public export
someString : Expectation String
someString = ExpectString Nothing

public export
someNumber :  Expectation Double
someNumber = ExpectNumber Nothing

public export
someBool : Expectation Bool
someBool = ExpectBoolean Nothing

public export
reverseFulfillment : Fulfillment( List a ) -> Fulfillment( List a )
reverseFulfillment( Fulfilled e xs ) = Fulfilled e ( reverse xs )
reverseFulfillment x = x

public export total
expect : JSON -> Expectation a -> Fulfillment a

expect( JString s ) e@( ExpectString mes ) = maybe( Fulfilled e s )
    (\es => if es == s then Fulfilled e s else Abandoned e Value ) mes
expect( JString _ ) e = Abandoned e Value

expect( JNumber n ) e@( ExpectNumber men ) = maybe( Fulfilled e n )
    (\en => if en == n then Fulfilled e n else Abandoned e Value ) men
expect( JNumber _ ) e = Abandoned e Value

expect( JBoolean b ) e@( ExpectBoolean meb ) = maybe( Fulfilled e b )
    (\eb => if eb == b then Fulfilled e b else Abandoned e Value ) meb
expect( JBoolean _ ) e = Abandoned e Value

expect( JArray xs ) e@( ExpectArray inner_e ) = reverseFulfillment $ snd $ foldl accumFulfillment ( Z, Fulfilled e [] ) xs where
    accumFulfillment( n, Abandoned e l  ) _ = ( n, Abandoned e l )
    accumFulfillment( n, Fulfilled e ys ) x = validate ( expect x inner_e ) where
        validate( Abandoned _ l ) = ( n, Abandoned e ( ListElement n l ) )
        validate( Fulfilled _ y ) = ( S n, Fulfilled e ( y :: ys ) )
expect( JArray _ ) e = Abandoned e Value

expect( JObject o ) ExpectObject = Fulfilled ExpectObject o
expect( JObject _ ) e            = Abandoned e Value

expect j ExpectAny = Fulfilled ExpectAny j

expect j e = Abandoned e Value
