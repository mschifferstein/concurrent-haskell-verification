module Data.State where

open import Haskell.Prelude

-- Definitions mostly taken from https://wiki.haskell.org/State_Monad.

record State (s a : Set) : Set where
    constructor MkState
    field
        runState : s → (a × s)
open State public
{-# COMPILE AGDA2HS State #-}

instance
    iFunctorState : { s : Set } → Functor (State s)
    iApplicativeState : { s : Set } → Applicative (State s)
    iMonadState : { s : Set } → Monad (State s)

    iFunctorState .fmap f a = a >>= (pure ∘ f)
    {-# COMPILE AGDA2HS iFunctorState #-}

    iApplicativeState .pure = λ x → MkState (λ s → (x , s)) -- again notation because of temrination checking
    iApplicativeState ._<*>_ mf ma = do 
                                        f ← mf
                                        a ← ma
                                        pure (f a)
    {-# COMPILE AGDA2HS iApplicativeState #-}

    iMonadState ._>>=_ m k = MkState (λ s → case runState m s of λ
                                                {(x , s') → runState (k x) s'})
    {-# COMPILE AGDA2HS iMonadState #-}

get : {s : Set} → State s s
get = MkState (λ s1 → (s1 , s1))
{-# COMPILE AGDA2HS get #-}

gets : {s a : Set} → (s → a) → State s a
gets f = do
            x <- get
            return (f x)
{-# COMPILE AGDA2HS gets #-}

put : {@0 s : Set} → s → State s ⊤ -- TODO: why not put : (s : Set) → State s ⊤ ?
put s1 = MkState (λ x → (tt , s1))
{-# COMPILE AGDA2HS put #-}

evalState : {@0 s a : Set} → State s a → s → a
evalState s1 = fst ∘ runState s1
{-# COMPILE AGDA2HS evalState #-}

execState : {@0 s a : Set} → State s a → s → s
execState s1 = snd ∘ runState s1
{-# COMPILE AGDA2HS execState #-}