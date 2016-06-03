import Control.Applicative
type Error = String
newtype Parser a = P { unP :: String -> (String, Either Error a) }

instance Functor Parser where
  fmap f (P st) = P $ \stream -> case st stream of
    (res, Left err) -> (res, Left err)
    (res, Right a ) -> (res, Right (f a))

instance Applicative Parser where
  pure a = P (\stream -> (stream, Right a))
  P ff <*> P xx = P $ \stream0 -> case ff stream0 of   -- produce an f
    (stream1, Left err) -> (stream1, Left err)
    (stream1, Right f ) -> case xx stream1 of          -- produce an x
      (stream2, Left err) -> (stream2, Left err)
      (stream2, Right x ) -> (stream2, Right (f x))    -- return (f x)

-- we want to produce a Applicative, Parser along with a primitive parser value
-- | Peek at the next character and return successfully if it satisfies a predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = P $ \stream -> case stream of
  []                 -> ([], Left "end of stream")
  (c:cs) | f c       -> (cs, Right c)
         | otherwise -> (cs, Left "did not satisfy")

--a few combinators such as try, which "undoes" a parser if it fails
-- | Run a parser but if it fails revert the stream to it's original state
try :: Parser a -> Parser a
try (P f) = P $ \stream0 -> case f stream0 of
  (_      , Left err) -> (stream0, Left err)
  (stream1, Right a ) -> (stream1, Right a )

--orElse which allows us to continue with a second parser if the first parser fails.
--Usually this is actually written with the infix combinators
orElse :: Parser a -> Parser a -> Parser a
orElse (P f1) (P f2) = P $ \stream0 -> case f1 stream0 of
  (stream1, Left err) -> f2 stream1
  (stream1, Right a ) -> (stream1, Right a)

instance Alternative Parser where
  empty = P $ \stream -> (stream, Left "empty")
  (<|>) = orElse

  many = manyParser
  some = someParser

-- | 0 or more
manyParser :: Parser a -> Parser [a]
manyParser (P f) = P go where
  go stream = case f stream of
    (_      , Left err) -> (stream, Right [])  -- throws away the error
    (stream', Right a ) -> case go stream' of
      (streamFin, Left err) -> (streamFin, Left err)
      (streamFin, Right as) -> (streamFin, Right (a : as))

-- | 1 or more
someParser :: Parser a -> Parser [a]
someParser (P f) = P $ \stream -> case f stream of
  (stream', Left err) -> (stream', Left err)
  (stream', Right a ) ->
    let (P fmany) = manyParser (P f)
    in case fmany stream' of
      (stream'', Left err) -> (stream'', Left err)
      (stream'', Right as) -> (stream'', Right (a:as))

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
-- string []     = pure []
-- string (c:cs) = (:) <$> char c <*> string cs
string = foldr (\ c -> (<*>) ((:) <$> char c)) (pure [])

oneOf :: String -> Parser Char
oneOf cs = satisfy (`elem` cs)

parens :: Parser a -> Parser a
parens parseA = dropFirstAndLast <$> char '(' <*> parseA <*> char ')'
  where
    dropFirstAndLast _ a _ = a
