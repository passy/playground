import Prelude
import Numeric
import Data.Array
import Control.Monad (liftM)
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Vector (Array Int LispVal)
    deriving Show

oct2dig :: (Num a, Eq a) => String -> a
oct2dig x = fst $ readOct x !! 0

hex2dig :: (Num a, Eq a) => String -> a
hex2dig x = fst $ readHex x !! 0

bin2dig :: [Char] -> Integer
bin2dig = bin2dig' 0
    where
        bin2dig' digint "" = digint
        bin2dig' digint (x:xs) =
            let
                old = 2 * digint + (if x == '0' then 0 else 1)
            in
                bin2dig' old xs

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser Char
escapedChars = do
    _ <- char '\\'
    s <- oneOf "\\\"nrt"
    return $ case s of
        '\\' -> s
        '"' -> s
        'n' -> '\n'
        't' -> '\t'
        'r' -> '\r'
        _ -> error "Unmatched value"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many $ escapedChars <|> noneOf "\"\\"
    _ <- char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ Atom $ first : rest

parseNumber :: Parser LispVal
parseNumber = parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin

parseDigital1 :: Parser LispVal
parseDigital1 = many1 digit >>= (return . Number . read)

parseDigital2 :: Parser LispVal
parseDigital2 = do
    _ <- try $ string "#d"
    liftM (Number . read) (many1 digit)

parseHex :: Parser LispVal
parseHex = do
    _ <- try $ string "#x"
    liftM (Number . hex2dig) (many1 hexDigit)

parseOct :: Parser LispVal
parseOct = do
    _ <- try $ string "#o"
    liftM (Number . oct2dig) (many1 octDigit)

parseBin :: Parser LispVal
parseBin = do
    _ <- try $ string "#b"
    liftM (Number . bin2dig) (many1 $ oneOf "10")

parseCharacter :: Parser LispVal
parseCharacter = do
    _ <- try $ string "#\\"
    value <- try (string "newline" <|> string "space")
        <|> do { x <- anyChar; notFollowedBy alphaNum; return [x] }
    return $ Character $ case value of
        "space" -> ' '
        "newline" -> '\n'
        _ -> (value !! 0)

parseBool :: Parser LispVal
parseBool = do
    _ <- char '#'
    ((char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool True)))

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head' <- endBy parseExpr spaces
    tail' <- char '.' >> spaces >> parseExpr
    return $ DottedList head' tail'

parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseVector :: Parser LispVal
parseVector = do
    _ <- string "#("
    arrayValues <- sepBy parseExpr spaces
    _ <- char ')'
    return $ Vector (listArray (0, (length arrayValues - 1)) arrayValues)

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> try parseNumber
    <|> try parseBool
    <|> try parseCharacter
    <|> parseQuoted
    <|> try parseVector
    <|> do
        _ <- char '('
        x <- try parseList <|> parseDottedList
        _ <- char ')'
        return x

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (head args))
