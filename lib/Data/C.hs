{-# LANGUAGE TypeSynonymInstances #-}
module Data.C where

data Action m = Atom (m (Action m))
              | Fork (Action m) (Action m)
              | Stop

type C m a = (a -> Action m) -> Action m

instance (Monad m) => Functor (C m) where
    fmap f x = x >>= pure . f

instance (Monad m) => Applicative (C m) where
    pure = \ x c -> c x
    mf <*> ma = mf >>= \ f -> ma >>= \ a -> pure (f a)

instance (Monad m) => Monad (C m) where
    (>>=) = \ f k c -> f (\ a -> k a c)

atom :: Monad m => m a -> C m a
atom m = \ c -> Atom (fmap c m)

action :: Monad m => C m a -> Action m
action m = m (\ _ -> Stop)

stop :: Monad m => C m a
stop = \ _ -> Stop

par :: Monad m => C m a -> C m a -> C m a
par m1 m2 = \ c -> Fork (m1 c) (m2 c)

fork :: Monad m => C m a -> C m ()
fork m = \ c -> Fork (action m) (c ())

round_robin :: Monad m => [Action m] -> m ()
round_robin [] = return ()
round_robin (Atom x : xs) = x >>= \ x1 -> round_robin (xs ++ [x1])
round_robin (Fork x y : xs) = round_robin (xs ++ [x, y])
round_robin (Stop : xs) = round_robin xs

run :: Monad m => C m a -> m ()
run m = round_robin [action m]

