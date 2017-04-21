import Data.Char
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State


type Parser a = StateT String Maybe a
runParser :: Parser a -> String -> Maybe (a, String)
runParser = runStateT

item :: Parser Char
item = do
    s <- get
    case s of
        "" -> lift Nothing
        (c:cs) -> put cs >> lift (Just c)


sat :: (Char -> Bool) -> Parser Char
sat p = do
    c <- item
    if p c
        then
            lift $ Just c
        else
            lift Nothing


char :: Char -> Parser Char
char c = sat (c ==)


string :: String -> Parser String
string "" = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}


many :: Parser a -> Parser [a]
many p = many1 p `mplus` return []
many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- many p; return (a:as)}

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) `mplus` return []
sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do{a <- p;as <- many (do {sep; p});return (a:as)}

space :: Parser String
space = many (sat isSpace)

token :: Parser a -> Parser a
token p = do {a <- p; space; return a}

symb :: String -> Parser String
symb cs = token (string cs)

apply :: Parser a -> String -> Maybe (a, String)
apply p = runParser (do {space; p})


{----------------------------------------
expr ::= term addop expr | term
term ::= factor mulop factor | factor
factor ::= digit | ( expr )
digit ::= 0 | 1 | . . . | 9
addop ::= + | -
mulop ::= * | /
-----------------------------------------}

operate :: Parser (a -> a -> a) -> Parser a -> Parser a -> Parser a
operate pop pn1 pn2 = do
    n1 <- pn1
    op <- pop
    n2 <- pn2
    return $ n1 `op` n2

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = do
    s0 <- get
    x <- p1
    s <- get
    if s == ""
        then return x
        else put s0 >> p2

expr :: Parser Int
term :: Parser Int
factor :: Parser Int
addop :: Parser Int -> Parser Int -> Parser Int
mulop :: Parser Int -> Parser Int -> Parser Int
digit :: Parser Int

expr = (term `addop` expr) `mplus` term
term = (factor `mulop` term) `mplus` factor
factor = digit `mplus` do {symb "("; n <- expr; symb ")"; return n}
digit = do {x <- token (sat isDigit); return (ord x - ord '0')}
addop = operate $ do {symb "+"; return (+)} `mplus` do {symb "-"; return (-)}
mulop = operate $ do {symb "*"; return (*)} `mplus` do {symb "/"; return div}

calculate :: String -> IO()
calculate s = case apply expr s of
    Nothing -> print "Error"
    Just (ans, rest) ->
        if rest == ""
            then print ans
            else print "Error"
