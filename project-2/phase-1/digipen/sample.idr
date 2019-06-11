import digipen.errors
import Language.JSON

data Class

Show Class where show _ = "class"

parseExceptional : String -> Exceptional JSON
parseExceptional s = maybe FailedParse pure( JSON.parse s )

loadClass : JSON -> Exceptional Class

parseLoadClass : String -> Exceptional Class
parseLoadClass s = do
    json <- parseExceptional s
    loadClass json

main : IO ()
main = do
    Right contents <- readFile "classes.json"
        | Left error => printLn error
    putStrLn contents
    case parseLoadClass contents of
        FailedParse => putStrLn "classes parse failed"
        FailedData location => do
            putStrLn "classes data failed to load"
            print location
        NoFailure loadedClass => print loadedClass
