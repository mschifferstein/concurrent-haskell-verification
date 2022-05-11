module Data.MonadTrans where

class MonadTrans τ where
    lift :: Monad m => m a -> τ m a

