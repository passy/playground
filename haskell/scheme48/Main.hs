{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}

import Prelude
import Numeric
import Data.Array
import System.IO
import Data.IORef
import Data.Maybe
import Data.Typeable
-- TODO: Use Control.Monad.Trans.Except instead
import Control.Monad.Error
import Control.Exception.Base (throw, Exception)
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
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String],
                      vararg :: Maybe String,
                      body :: [LispVal],
                      closure :: Env }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
    deriving (Typeable)

instance Exception LispError

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

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
    show (PrimitiveFunc _) = "<primitive>"
    show (Func { params = args,
                 vararg = varargs,
                 body = _,
                 closure = _ }) = "(lambda (" ++ unwords (map show args) ++ (case varargs of
                    Nothing -> ""
                    Just arg -> " . " ++ arg) ++ ") ...)"
    show (Port _) = "<IO port>"
    show (IOFunc _) = "<IO primitive>"

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

type IOThrowsError = ErrorT LispError IO

type Env = IORef [(String, IORef LispVal)]

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows = fmap extractValue . runErrorT . trapError

isBound :: Env -> String -> IO Bool
isBound = flip (fmap . (isJust . ) . lookup) . readIORef

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef)
          (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
          (liftIO . flip writeIORef value)
          (lookup var env)
    return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env)
            return value

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where
        extendEnv bindings' env = liftM (++ env) (mapM addBindings bindings')
        addBindings (var, value) = do
            ref <- newIORef value
            return (var, ref)

oct2dig :: (Num a, Eq a) => String -> a
oct2dig x = fst $ head (readOct x)

hex2dig :: (Num a, Eq a) => String -> a
hex2dig x = fst $ head (readHex x)

bin2dig :: String -> Integer
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
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval env (Atom name) = getVar env name
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", prec, conseq, alt]) = do
    result <- eval env prec
    case result of
        Bool False -> eval env alt
        Bool True -> eval env conseq
        _ -> throwError $ TypeMismatch "bool" prec
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params') : body')) =
    makeNormalFunc env params' body' >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params') varargs : body')) =
    makeVarArgs varargs env params' body' >>= defineVar env var
eval env (List (Atom "lambda" : List params' : body')) =
    makeNormalFunc env params' body'
eval env (List (Atom "lambda" : DottedList params' varargs : body')) =
    makeVarArgs varargs env params' body'
eval env (List (Atom "lambda" : varargs@(Atom _) : body')) =
    makeVarArgs varargs env [] body'
eval env (List [Atom "load", String filename]) =
    load filename >>= liftM last . mapM (eval env)
eval env (List (func : args)) = do
    func' <- eval env func
    argVals <- mapM (eval env) args
    apply func' argVals
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args =
    liftThrows $ func args
apply (Func params' varargs body' closure') args =
    if num params' /= num args && isNothing varargs
        then throwError $ NumArgs (num params') args
        else liftIO (bindVars closure' $ zip params' args) >>=
            bindVarArgs varargs >>=
            evalbody'
    where
        remainingArgs = drop (length params') args
        num = toInteger . length
        evalbody' env = liftM last $ mapM (eval env) body'
        bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
            Nothing -> return env
apply (IOFunc func) args = func args
apply notFunc _ = throwError $ NotFunction "Invalid function application" (show notFunc)

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

makeFunc :: Show a => Maybe String -> Env -> [a] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params' body' =
    return $ Func (map show params') varargs body' env

makeNormalFunc :: Show a => Env -> [a] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . show

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
    ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string=?", strBoolBinop (==)),
    ("string<?", strBoolBinop (<)),
    ("string>?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?", eqv),
    ("equal?", equal)]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [
    ("apply", applyProc),
    ("open-input-file", makePort ReadMode),
    ("open-output-file", makePort WriteMode),
    ("close-input-port", closePort),
    ("close-output-port", closePort),
    ("read", readProc),
    ("write", writeProc),
    ("read-contents", readContents),
    ("read-all", readAll)]

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>=
    (flip bindVars $ map (makeFunc' PrimitiveFunc) primitives
                   ++ map (makeFunc' IOFunc) ioPrimitives)
    where makeFunc' ctor (var, func) = (var, ctor func)

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args
applyProc _ = throwError $ Default "Invalid apply invocation"

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
makePort _ [val] = throwError $ TypeMismatch "string" val
makePort _ _ = throwError $ Default "Invalid makePort invocation"

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO (hClose port) >> return (Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr
readProc [val] = throwError $ TypeMismatch "port" val
readProc _ = throwError $ Default "Invalid readProc invocation"

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)
writeProc _ = throwError $ Default "Invalid writeProc invocation"

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename
readContents [val@_] = throwError $ TypeMismatch "string" val
readContents val@_ = throwError $ NumArgs 1 val

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
readAll [val] = throwError $ TypeMismatch "string" val
readAll _ = throwError $ Default "Invalid readAll invocation"

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params' = liftM (Number . foldl1 op) (mapM unpackNum params')


boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
    if length args /= 2
    then throwError $ NumArgs 2 args
    else
        -- TODO: I'm sure there's a nicer, less imperative
        -- way for this.
        let (argl : argr : _) = args
        in do
            left <- unpacker argl
            right <- unpacker argr
            return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
    let parsed = reads n :: [(Integer, String)] in
        if null parsed
            then throwError $ TypeMismatch "number" $ String n
            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals a b (AnyUnpacker unpacker) = do
        ua <- unpacker a
        ub <- unpacker b
        return $ ua == ub
    `catchError` (const $ return False)

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left err) = throw err

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] =
    eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] =
    return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
    where
        eqvPair (x1, x2) =
            case eqv [x1, x2] of
                Right (Bool val) -> val
                Right _ -> undefined
                Left _ -> False -- Cannot be reached.
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal [a, b] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals a b)
                       [AnyUnpacker unpackNum,
                        AnyUnpacker unpackStr,
                        AnyUnpacker unpackBool]
    eqvEquals <- eqv [a, b]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

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
        _ -> head value

parseBool :: Parser LispVal
parseBool = do
    _ <- char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool True))

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
    return $ Vector (listArray (0, length arrayValues - 1) arrayValues)

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

nullEnv :: IO Env
nullEnv = newIORef []

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred' prompt action = do
    result <- prompt
    unless (pred' result) $ action result >> until_ pred' prompt action

runRepl :: IO ()
runRepl = primitiveBindings >>=
    until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [
        ("args", List $ map String $ drop 1 args)]
    runIOThrows (liftM show $ eval env (List [Atom "load", String (head args)]))
        >>= hPutStrLn stderr

main :: IO ()
main = do
    args <- getArgs
    if null args
        then runRepl
        else runOne args
