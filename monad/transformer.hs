import Data.Char
import Control.Monad
import Control.Monad.Trans
import Control.Applicative

getPassphrase :: IO (Maybe String)
getPassphrase = do s <- getLine
                   if isValid s then return $ Just s
                                else return Nothing

-- The validation test could be anything we want it to be.
isValid :: String -> Bool
isValid s = length s >= 8
            && any isAlpha s
            && any isNumber s
            && any isPunctuation s

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

askPassphrase :: IO ()
askPassphrase = do putStrLn "Insert your new passphrase:"
                   maybe_value <- getPassphrase
                   if isJust maybe_value
                     then putStrLn "Storing in database..." -- do stuff
                     else putStrLn "Passphrase invalid."

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
instance Monad m => Monad (MaybeT m) where
  return  = MaybeT . return . Just
  -- The signature of (>>=), specialized to MaybeT m:
  -- (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  x >>= f = MaybeT $ do maybe_value <- runMaybeT x
                        case maybe_value of
                           Nothing    -> return Nothing
                           Just value -> runMaybeT $ f value

instance Monad m => Applicative (MaybeT m) where
    pure = return
    (<*>) = ap

instance Monad m => Functor (MaybeT m) where
    fmap = liftM

instance Monad m => Alternative (MaybeT m) where
    empty   = MaybeT $ return Nothing
    x <|> y = MaybeT $ do maybe_value <- runMaybeT x
                          case maybe_value of
                               Nothing    -> runMaybeT y
                               Just _     -> return maybe_value

instance Monad m => MonadPlus (MaybeT m) where
    mzero = empty
    mplus = (<|>)

instance MonadTrans MaybeT where
    lift = MaybeT . liftM Just

getValidPassphrase :: MaybeT IO String
getValidPassphrase = do s <- lift getLine
                        guard (isValid s) -- Alternative provides guard.
                        return s

askPassphraseT :: MaybeT IO ()
askPassphraseT = do lift $ putStrLn "Insert your new passphrase:"
                    value <- getValidPassphrase
                    lift $ putStrLn "Storing in database..."

askPassword :: MaybeT IO ()
askPassword = do lift $ putStrLn "Insert your new password:"
                 value <- msum $ repeat getValidPassphrase
                 lift $ putStrLn "Storing in database..."
