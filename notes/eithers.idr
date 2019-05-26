
-- (<*>) : Applicative f => f (a -> b) -> f a -> f b
--                          f = Either String
--                          a = String
--                          b = Nat
-- length : String -> Nat
testEither : Either String Nat
testEither = Right Strings.length <*> Left "oops"

MaybeException : Type -> Type
MaybeException = Either String

multipleOperations : MaybeException Nat -> MaybeException String -> MaybeException String
multipleOperations intComp stringComp = concat <$> ( replicate <$> intComp <*> stringComp )

-- replicate : Nat -> String -> List String
-- (<$>) : ( a -> b ) -> f a -> f b
--         a = Nat
--         b = String
-- ( map replicate ) : f Nat -> f String
-- ( replicate <$> intComp ) : MaybeException( String -> String )

