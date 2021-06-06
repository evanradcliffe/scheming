module Main where
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
    Left err -> "Error parsing expression: " ++ show err
    Right value -> "Found value: " ++ value:""

main :: IO ()
main = do
    putStrLn $ readExpr "@"
    putStrLn "testing first haskell compile"
