module MyJSON where
import Data.List
import Data.Char
import Control.Applicative
import Control.Monad
-- a json string is composed with a single string or list of other jsons or a key-value object
data JValue = JString String
            | JInteger Integer
            | JFloat Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

encodeJValue :: JValue -> String
encodeJValue (JString s) = "\"" ++ s ++ "\""
encodeJValue (JInteger n) = show n
encodeJValue (JFloat n) = show n
encodeJValue (JBool b) = if b then "true" else "false"
encodeJValue (JNull) = "null"
encodeJValue (JObject o) = "{" ++ (intercalate ", " (fmap mapTuple o))++ "}"
  where
    mapTuple (s, v) = "\"" ++ s ++ "\": " ++ encodeJValue v
encodeJValue (JArray a) = "[" ++ (intercalate ", " (fmap encodeJValue a))++ "]"

encodeIndentJValue :: JValue -> String
encodeIndentJValue (JString s) = "\"" ++ s ++ "\""
encodeIndentJValue (JInteger i) = show i
encodeIndentJValue (JFloat f) = show f
encodeIndentJValue (JBool b) = if b then "true" else "false"
encodeIndentJValue (JNull) = "null"
encodeIndentJValue (JObject o) = "{\n" ++ (intercalate ",\n" $ addIndent [mapTuple a |a<-o ]) ++ "\n}\n"
  where
    addIndent = fmap $ intercalate "\n" . fmap ("  " ++) . lines
    mapTuple (s, v) = "\"" ++ s ++ "\": " ++ encodeIndentJValue v

encodeIndentJValue (JArray a) = "[\n" ++ (intercalate ",\n" $ addIndent [eij x |x<-a ]) ++ "\n]\n"
  where
    addIndent = fmap $ intercalate "\n" . fmap ("  " ++) . lines
    eij = encodeIndentJValue

-- following is two parsers
data Error = EmptyError | ItemError Bool | SatError Char | CharError Char | StringError String |
  ErrorChain [Error] deriving(Show)

appendError :: Error -> Error -> Error
appendError (ErrorChain ea) (ErrorChain eb) = ErrorChain $ eb ++ ea
appendError (ErrorChain es) e = ErrorChain (e:es)
appendError ea eb = ErrorChain [eb, ea]

newtype ApplicativeParser a = AP {runApplicativeParser :: String -> (Either Error a, String)}
newtype Parser a = Parser {runParser :: String -> (Either Error a, String)}

instance Functor Parser where
  fmap f x = pure f <*> x

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \cs -> (Right a, cs)
  (Parser parse) >>= f = Parser $ \cs -> case (parse cs) of
    (Left e, cs') -> (Left e, cs')
    (Right a, cs') -> runParser (f a) cs'

instance Alternative Parser where
  empty = Parser $ \cs -> (Left EmptyError, cs)
  (Parser p) <|> (Parser q) = Parser $ \cs -> case (p cs) of
    (Left e, _) -> q cs
    (Right a, cs') -> (Right a, cs')
  some p = do {a <- p; as <- many p; return (a:as)}

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

parseErr :: Error -> Parser a
parseErr e = Parser $ \cs -> (Left e, cs)

orErr :: Parser a -> (Error -> String -> (Error, String)) -> Parser a
(Parser p) `orErr` f = Parser $ \cs -> case (p cs) of
  (Right a, cs') -> (Right a, cs')
  (Left e, cs') -> let (e', cs'') = f e cs' in (Left e', cs'')

item :: Parser Char
item = Parser $ \cs -> case cs of
  "" -> (Left $ ItemError True, cs)
  (c:cs) -> (Right c, cs)

sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else parseErr $ SatError c }

char :: Char -> Parser Char
char c = sat (c ==) `orErr` \e cs -> (appendError e $ CharError c, cs)

string0 :: String -> Parser String
string0 "" = return ""
string0 (c:cs) = do {char c; string0 cs; return (c:cs)}

string :: String -> Parser String
string "" = return ""
string s = string0 s `orErr` \e cs -> (appendError e $ StringError s, cs)

space :: Parser String
space = many (sat isSpace)

one :: Parser String
one = do {a <- item; if a == '\\' then do {b <- item; return [a,b]} else return [a]}

manyChars :: Parser String
manyChars = someChars <|> return []

someChars :: Parser String
someChars = do
  a <- item
  if a == '"' then empty
  else if a == '\\' then do {b <- item; as <- manyChars; return (a:b:as)}
    else do {as <- manyChars; return(a:as)}

token :: Parser a -> Parser a
token p = do {a <- p; space; return a}

symb :: String -> Parser String
symb cs = token (string cs)

apply :: Parser a -> String -> (Either Error a, String)
apply p = runParser (do {space; p})

isNumb :: Char -> Bool
isNumb c = isDigit c || c == '.'

numbers :: Parser [Char]
numbers = some (sat isNumb)

splitter :: Char -> Parser Char
splitter c = do {space; x <- char c; space; return x}

parseJNull :: Parser JValue
parseJBool :: Parser JValue
parseJNumber :: Parser JValue
parseJString :: Parser JValue
parseJObject :: Parser JValue
parseJArray :: Parser JValue
parseJValue :: Parser JValue

parseJNull = do {symb "null"; return JNull}
parseJBool = do {symb "true"; return $ JBool True} <|> do {symb "false"; return $ JBool False}
parseJNumber = do
  numStr <- numbers
  if '.' `elem` numStr then return $ JFloat (read numStr :: Double)
  else return $ JInteger (read numStr :: Integer)

parseJString = do {char '"'; chars <- manyChars; char '"'; return $ JString chars}
parseJObject = do
  splitter '{'
  firstOne <- do {(JString k) <- parseJString; space; splitter ':'; v <- parseJValue; return (k, v)}
  tups <- many (do
    splitter ','
    (JString k) <- parseJString;
    splitter ':'
    v <- parseJValue
    return (k, v))
  splitter '}'
  return $ JObject (firstOne:tups)

parseJArray = do
  splitter '['
  firstOne <- (do {v <- parseJValue; return v})
  others <- many (do {splitter ','; v <- parseJValue; return v})
  splitter ']'
  return $ JArray (firstOne:others)

parseJValue = parseJObject
          <|> parseJArray
          <|> parseJNull
          <|> parseJBool
          <|> parseJString
          <|> parseJNumber

json :: String -> JValue
json s = let (Right r, _) = apply parseJValue s in r
