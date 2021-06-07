module Main where
import System.Environment
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

eval :: LispVal -> LispVal
eval value@(String _)             = value
eval value@(Number _)             = value
eval (List [Atom "quote", value]) = value
eval (List (Atom f : args))       = apply f $ map eval args

apply :: String -> [LispVal] -> LispVal
apply f args = maybe (Bool False) ($ args) $ lookup f primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop (div))]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String s) = let parsed = reads s :: [(Integer, String)] in
    if null parsed
        then 0
        else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _          = 0

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "Error parsing expression: " ++ show err
    Right value -> value

main :: IO ()
main = do
    args <- getArgs
    expr <- if null args
        then do
            putStr "> "
            getLine
        else
            liftM head getArgs
    putStrLn (stringifyLispVal (eval (readExpr expr))) -- TODO: make this pretty
