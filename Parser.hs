module Parser
    ( Parser, runParser
    )
    where

import Control.Arrow

data Parser m a = P
    { runParser :: String -> m (a, String)
    }

instance Functor m => Functor (Parser m) where
    fmap f (P g) = P $ fmap (first f) . g

instance Monad m => Applicative (Parser m) where
    pure x = P $ \s -> pure (x, s)
    P f <*> P g = P $ \s -> do
        (f', s' ) <- f s
        (x', s'') <- g s'
        return (f' x', s'')

instance Monad m => Monad (Parser m) where
    return = pure
    P f >>= g = P $ \s -> do
        (x, s') <- f s
        runParser (g x) s'
