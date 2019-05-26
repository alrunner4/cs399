import digipen.instructor.conway.spec

main : IO ()
main = do
	name <- getLine
	name2 <- getLine
	let fancyName = name ++ ( show ( length name ) )
	putStrLn( "Hello, " ++ fancyName ++ " and " ++ name2 )
	_ <- getLine
	pure ()


data NumAndStuff = N Int String

Num NumAndStuff where
	( N x1 s1 ) + ( N x2 s2 ) = N ( x1 + x2 ) ( s1 ++ s2 )

