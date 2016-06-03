import Data.Char

-- I won't use the Control.Monad to avoid exhausting Functor, Applicative etc. implementation
class MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

instance MonadPlus [] where
  mzero = []
  mplus = (++)

newtype Parser a = Parser { runParser :: String -> [(a, String)]}

item :: Parser Char
item = Parser $ \cs -> case cs of
  "" -> []
  (c:cs) -> [(c,cs)]

instance Functor Parser where
  fmap f x = pure f <*> x

instance Applicative Parser where
  pure = return
  ff <*> xx = do {f<-ff; x<-xx; return $ f x}

instance Monad Parser where
  return a = Parser $ \cs -> [(a,cs)]
  p >>= f = Parser $ \cs -> concat [runParser (f a) cs' | (a,cs') <- runParser p cs]

instance MonadPlus Parser where
  mzero = Parser $ const []
  (Parser p) `mplus` (Parser q) = Parser $ \cs -> p cs `mplus` q cs


(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser $ \cs -> case runParser (p `mplus` q) cs of
  [] -> []
  (x:xs) -> [x]

sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else mzero}

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser String
string "" = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}

many :: Parser a -> Parser [a]
many p = many1 p +++ return []
many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- many p; return (a:as)}

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) +++ return []
sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do{a <- p;as <- many (do {sep; p});return (a:as)}

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
  where
    rest a = do {f <- op;b <- p;rest (f a b)} +++ return a

space :: Parser String
space = many (sat isSpace)

token :: Parser a -> Parser a
token p = do {a <- p; space; return a}

symb :: String -> Parser String
symb cs = token (string cs)

apply :: Parser a -> String -> [(a,String)]
apply p = runParser (do {space; p})


{----------------------------------------
expr ::= expr addop term | term
term ::= term mulop factor | factor
factor ::= digit | ( expr )
digit ::= 0 | 1 | . . . | 9
addop ::= + | -
mulop ::= * | /
-----------------------------------------}
expr :: Parser Int
addop :: Parser (Int -> Int -> Int)
mulop :: Parser (Int -> Int -> Int)
expr = term `chainl1` addop
term = factor `chainl1` mulop
factor = digit +++ do {symb "("; n <- expr; symb ")"; return n}
digit = do {x <- token (sat isDigit); return (ord x - ord '0')}
addop = do {symb "+"; return (+)} +++ do {symb "-"; return (-)}
mulop = do {symb "*"; return (*)} +++ do {symb "/"; return div}
