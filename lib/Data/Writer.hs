module Data.Writer where

type W a = (a, String)

class Monad m => Writer m where
    write :: String -> m ()

instance Functor W where
    fmap f (a, s) = (f a, s)

instance Applicative W where
    pure a = (a, [])
    (f, s) <*> (a, s') = (f a, s ++ s')

instance Monad W where
    (a, s) >>= k = (fst (k a), s ++ snd (k a))

instance Writer W where
    write s = ((), s)

output :: W a -> String
output (a, s) = s

