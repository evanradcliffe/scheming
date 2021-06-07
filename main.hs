module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool

instance Show LispVal where show = stringifyLispVal

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
    char '"'
    filterDoubleQuote <- many (noneOf "\"")
    char '"'
    return $ String filterDoubleQuote

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseParensList :: Parser LispVal
parseParensList = do
    char '('
    x <- try parseList <|> parseDottedList
    char ')'
    return x

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    exp <- parseExpr
    return $ List [Atom "quote", exp]

stringifyLispVal :: LispVal -> String
stringifyLispVal (Bool True)    = "#t"
stringifyLispVal (Bool False)   = "#f"
stringifyLispVal (String value) = value
stringifyLispVal (Number value) = show value
stringifyLispVal (Atom value)   = value
stringifyLispVal (List list)    = "(" ++ (unwords . map stringifyLispVal) list ++ ")"

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> parseParensList

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "Error parsing expression: " ++ show err
    Right value -> "Found value: " ++ stringifyLispVal value

main :: IO ()
main = do
    arg <- getLine
    putStrLn $ readExpr arg
