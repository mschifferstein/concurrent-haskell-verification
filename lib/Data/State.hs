module Data.State where

data State s a = MkState{runState :: s -> (a, s)}

instance Functor (State s) where
    fmap f a = a >>= pure . f

instance Applicative (State s) where
    pure = \ x -> MkState (\ s -> (x, s))
    mf <*> ma = mf >>= \ f -> ma >>= \ a -> pure (f a)

instance Monad (State s) where
    m >>= k
      = MkState
          (\ s ->
             case runState m s of
                 (x, s') -> runState (k x) s')

get :: State s s
get = MkState (\ s1 -> (s1, s1))

gets :: (s -> a) -> State s a
gets f = get >>= \ x -> return (f x)

put :: s -> State s ()
put s1 = MkState (\ x -> ((), s1))

evalState :: State s a -> s -> a
evalState s1 = fst . runState s1

execState :: State s a -> s -> s
execState s1 = snd . runState s1

