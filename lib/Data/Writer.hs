module Data.Writer where

data W a = MkW{value :: a, msg :: String}

class Monad m => Writer m where
    write :: String -> m ()

instance Functor W where
    fmap f (MkW x s) = MkW (f x) s

instance Applicative W where
    pure a = MkW a ""
    MkW f s <*> MkW x s' = MkW (f x) (s ++ s')

instance Monad W where
    MkW x s >>= k = MkW (value (k x)) (s ++ msg (k x))

instance Writer W where
    write s = MkW () s

output :: W a -> String
output x = msg x

