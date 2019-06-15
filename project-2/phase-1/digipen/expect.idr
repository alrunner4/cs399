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

someString : Expectation String
someString = ExpectString Nothing

someNumber :  Expectation Double
someNumber = ExpectNumber Nothing

someBool : Expectation Bool
someBool = ExpectBoolean Nothing


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

expect( JArray xs ) e@( ExpectArray inner_e ) = snd $ foldl accumFulfillment ( Z, Fulfilled e [] ) xs where
    accumFulfillment( n, Abandoned e l  ) _ = ( n, Abandoned e l )
    accumFulfillment( n, Fulfilled e ys ) x = case expect x inner_e of
        Abandoned _ l => ( n, Abandoned e ( ListElement n l ) )
        Fulfilled _ y => ( S n, Fulfilled e ( y :: ys ) )
expect( JArray _ ) e = Abandoned e Value

expect( JObject o ) ExpectObject = Fulfilled ExpectObject o
expect( JObject _ ) e            = Abandoned e Value

expect j ExpectAny = Fulfilled ExpectAny j

expect j e = Abandoned e Value
