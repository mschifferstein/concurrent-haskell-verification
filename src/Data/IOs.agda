module Data.IOs where

open import Haskell.Prelude
open import Data.State
{-# FOREIGN AGDA2HS import Data.State #-}

Loc = Nat
{-# COMPILE AGDA2HS Loc #-}

record IORef (a : Set) : Set where
    constructor MkIORef
    field
        mem : Loc
open IORef public
{-# COMPILE AGDA2HS IORef #-}

-- This definition is limited because of how Data is defined (stores a natural number only) - see Swierstra section 2.4.
Data = Maybe Nat × Bool
{-# COMPILE AGDA2HS Data #-}

Heap = Loc → Data
{-# COMPILE AGDA2HS Heap #-}

record Store : Set where
  constructor St
  field
    fresh : Loc
    heap : Heap
open Store public
{-# COMPILE AGDA2HS Store #-}

emptyStore : Store
emptyStore = St 0 (λ x → (Nothing , False))
{-# COMPILE AGDA2HS emptyStore #-}

data IOs (a : Set) : Set where
    NewIORef : Data → (Loc → IOs a) → IOs a
    ReadIORef : Loc → (Data → IOs a) → IOs a
    WriteIORef : Loc → Data → (IOs a) → IOs a
    Return : a → IOs a
{-# COMPILE AGDA2HS IOs #-}

instance
    iFunctorIOs : Functor IOs
    iApplicativeIOs : Applicative IOs
    {-# TERMINATING #-} -- As this function is not obviously terminating for Agda's termination checker, we have to tell Agda explicitly that it is terminating
                        -- Doing this for either iFunctorIOs or iMonadIOs is sufficient
    iMonadIOs : Monad IOs
    
    iFunctorIOs .fmap f (NewIORef d io) = NewIORef d (fmap f ∘ io)
    iFunctorIOs .fmap f (ReadIORef l io) = ReadIORef l (fmap f ∘ io)
    iFunctorIOs .fmap f (WriteIORef l d io) = WriteIORef l d (fmap f io)
    iFunctorIOs .fmap f (Return x) = Return (f x)
    {-# COMPILE AGDA2HS iFunctorIOs #-}

    iApplicativeIOs .pure = Return
    iApplicativeIOs ._<*>_ mf ma = do 
                                    f ← mf
                                    a ← ma
                                    pure (f a)
    {-# COMPILE AGDA2HS iApplicativeIOs #-}

    iMonadIOs ._>>=_ (Return a) g = g a
    iMonadIOs ._>>=_ (NewIORef d f) g = NewIORef d (λ l → f l >>= g)
    iMonadIOs ._>>=_ (ReadIORef l f) g = ReadIORef l (λ d → f d >>= g)
    iMonadIOs ._>>=_ (WriteIORef l d s) g = WriteIORef l d (s >>= g)
    {-# COMPILE AGDA2HS iMonadIOs #-}
    
    iEqIORef : Eq (IORef a)
    iEqIORef ._==_ x y = x .mem == y .mem
    {-# COMPILE AGDA2HS iEqIORef #-}

newIORef : Data → IOs (IORef Data)
newIORef d = NewIORef d (Return ∘ MkIORef)
{-# COMPILE AGDA2HS newIORef #-}

readIORef : IORef Data → IOs Data 
readIORef (MkIORef l) = ReadIORef l Return
{-# COMPILE AGDA2HS readIORef #-}

writeIORef : IORef Data → Data → IOs ⊤
writeIORef (MkIORef l) d = WriteIORef l d (Return tt)
{-# COMPILE AGDA2HS writeIORef #-}

modifyHeap : (Heap → Heap) → State Store ⊤
modifyHeap f = do
                s ← get
                put (St (s .fresh) (f (s .heap)))
{-# COMPILE AGDA2HS modifyHeap #-}

modifyFresh : (Loc → Loc) → State Store ⊤
modifyFresh f = do
                    s ← get
                    put (St ( f (s .fresh)) (s .heap))
{-# COMPILE AGDA2HS modifyFresh #-}

alloc : State Store Loc
alloc = do
            loc ← gets fresh
            modifyFresh (λ x → x + 1)
            return loc
{-# COMPILE AGDA2HS alloc #-}

update : Loc → Data → Heap → Heap
update l d h k = if l == k then d else h k
{-# COMPILE AGDA2HS update #-}

extendHeap : Loc → Data → State Store ⊤
extendHeap l d = modifyHeap (update l d)
{-# COMPILE AGDA2HS extendHeap #-}

lookupHeap : Loc → State Store Data
lookupHeap l = do 
                h ← gets heap
                return (h l)
{-# COMPILE AGDA2HS lookupHeap #-}

runIOState : IOs a → State Store a
runIOState (Return a) = return a
runIOState (NewIORef d g) = do
                                loc ← alloc
                                extendHeap loc d
                                runIOState (g loc)
runIOState (ReadIORef l g) = do
                                d ← lookupHeap l 
                                runIOState (g d)
runIOState (WriteIORef l d p) = do
                                    extendHeap l d
                                    runIOState p
{-# COMPILE AGDA2HS runIOState #-}

runIOs : IOs a → a
runIOs io = evalState (runIOState io) emptyStore -- TODO: have such a function for C IOs
{-# COMPILE AGDA2HS runIOs #-}