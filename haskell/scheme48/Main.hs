import Prelude
import Numeric
import Data.Array
-- TODO: Use Control.Monad.Trans.Except instead
import Control.Monad.Error
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

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispVal where
    show (String contents) = "\"" ++ contents ++ "\""
    show (Atom name) = name
    show (Number contents) = show contents
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List contents) = "(" ++ unwordsList contents ++ ")"
    show (DottedList head' tail') = "(" ++ unwordsList head' ++ " . " ++ show tail' ++ ")"
    show (Character char') = show char'
    show (Vector contents) = "#(" ++ show contents ++ ")"

instance Show LispError where
    show (UnboundVar message varname) = message ++ ": " ++ varname
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message func) = message ++ ": " ++ show func
    show (NumArgs expected found) = "Expected " ++ show expected
        ++ " args; found values " ++ unwordsList found
    show (TypeMismatch expected found) = "Invalid type: expected " ++ expected
        ++ ", found " ++ show found
    show (Parser parseErr) = "Parse error at " ++ show parseErr
    show (Default parseErr) = "Unknown error: " ++ show parseErr

instance Error LispError where
    noMsg = Default "An error has occured"
    strMsg = Default

type ThrowsError = Either LispError

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

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

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe
    (throwError $ NotFunction "Unrecognized primitive" func)
    ($ args)
    (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
    ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
    let parsed = reads n :: [(Integer, String)] in
        if null parsed
            then throwError $ TypeMismatch "number" $ String n
            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

-- Uh, that's quite ugly. But gotta stick to the tut.
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

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
    -- Ugh, this is ugly. I hope this gets rectified later.
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
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
