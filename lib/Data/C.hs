module Data.C where

import Data.MonadTrans

data Action m = Atom (m (Action m))
              | Fork (Action m) (Action m)
              | Stop
              | NonTerm

data C m a = Conc{act :: (a -> Action m) -> Action m}

instance (Monad m) => Functor (C m) where
    fmap f x = x >>= pure . f

instance (Monad m) => Applicative (C m) where
    pure = \ x -> Conc (\ c -> c x)
    mf <*> ma = mf >>= \ f -> ma >>= \ a -> pure (f a)

instance (Monad m) => Monad (C m) where
    f >>= k = Conc (\ c -> act f (\ a -> act (k a) c))

atom :: Monad m => m a -> C m a
atom m = Conc (\ c -> Atom (fmap c m))

action :: Monad m => C m a -> Action m
action m = act m (\ _ -> Stop)

stop :: Monad m => C m a
stop = Conc (\ _ -> Stop)

par :: Monad m => C m a -> C m a -> C m a
par m1 m2 = Conc (\ c -> Fork (act m1 c) (act m2 c))

fork :: Monad m => C m a -> C m ()
fork m = Conc (\ c -> Fork (action m) (c ()))

instance MonadTrans C where
    lift = atom

data MyNat = Zero
           | Suc MyNat

round_robin :: Monad m => [Action m] -> MyNat -> m Bool
round_robin [] _ = return True
round_robin xs Zero = return False
round_robin (NonTerm : xs) _ = return False
round_robin (Atom x : xs) (Suc n)
  = x >>= \ x1 -> round_robin (xs ++ [x1]) n
round_robin (Fork x y : xs) (Suc n) = round_robin (xs ++ [x, y]) n
round_robin (Stop : xs) (Suc n) = round_robin xs n

run :: Monad m => C m a -> MyNat -> m Bool
run m n = round_robin [action m] n

