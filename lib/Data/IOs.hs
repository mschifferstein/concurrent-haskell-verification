module Data.IOs where

import Numeric.Natural (Natural)

import Data.State

type Loc = Natural

data IORef a = MkIORef{mem :: Loc}

type Data = (Maybe Natural, Bool)

type Heap = Loc -> Data

data Store = St{fresh :: Loc, heap :: Heap}

emptyStore :: Store
emptyStore = St 0 (\ x -> (Nothing, False))

data IOs a = NewIORef Data (Loc -> IOs a)
           | ReadIORef Loc (Data -> IOs a)
           | WriteIORef Loc Data (IOs a)
           | Return a

instance Functor IOs where
    fmap f (NewIORef d io) = NewIORef d (fmap f . io)
    fmap f (ReadIORef l io) = ReadIORef l (fmap f . io)
    fmap f (WriteIORef l d io) = WriteIORef l d (fmap f io)
    fmap f (Return x) = Return (f x)

instance Applicative IOs where
    pure = Return
    mf <*> ma = mf >>= \ f -> ma >>= \ a -> pure (f a)

instance Monad IOs where
    Return a >>= g = g a
    NewIORef d f >>= g = NewIORef d (\ l -> f l >>= g)
    ReadIORef l f >>= g = ReadIORef l (\ d -> f d >>= g)
    WriteIORef l d s >>= g = WriteIORef l d (s >>= g)

instance Eq (IORef a) where
    x == y = mem x == mem y

newIORef :: Data -> IOs (IORef Data)
newIORef d = NewIORef d (Return . MkIORef)

readIORef :: IORef Data -> IOs Data
readIORef (MkIORef l) = ReadIORef l Return

writeIORef :: IORef Data -> Data -> IOs ()
writeIORef (MkIORef l) d = WriteIORef l d (Return ())

modifyHeap :: (Heap -> Heap) -> State Store ()
modifyHeap f = get >>= \ s -> put (St (fresh s) (f (heap s)))

modifyFresh :: (Loc -> Loc) -> State Store ()
modifyFresh f = get >>= \ s -> put (St (f (fresh s)) (heap s))

alloc :: State Store Loc
alloc
  = gets (\ r -> fresh r) >>=
      \ loc -> modifyFresh (\ x -> x + 1) >> return loc

update :: Loc -> Data -> Heap -> Heap
update l d h k = if l == k then d else h k

extendHeap :: Loc -> Data -> State Store ()
extendHeap l d = modifyHeap (update l d)

lookupHeap :: Loc -> State Store Data
lookupHeap l = gets (\ r -> heap r) >>= \ h -> return (h l)

runIOState :: IOs a -> State Store a
runIOState (Return a) = return a
runIOState (NewIORef d g)
  = alloc >>= \ loc -> extendHeap loc d >> runIOState (g loc)
runIOState (ReadIORef l g)
  = lookupHeap l >>= \ d -> runIOState (g d)
runIOState (WriteIORef l d p) = extendHeap l d >> runIOState p

runIOs :: IOs a -> a
runIOs io = evalState (runIOState io) emptyStore

