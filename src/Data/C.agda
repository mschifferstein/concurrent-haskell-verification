module Data.C where

open import Haskell.Prelude
open import Data.MonadTrans
{-# FOREIGN AGDA2HS import Data.MonadTrans #-}

-- Whatever I'll prove holds under the assumption that the monads used are positive
{-# NO_POSITIVITY_CHECK #-}
data Action (m : Set → Set) : Set where
    Atom : m (Action m) → Action m
    Fork : Action m → Action m → Action m
    Stop : Action m
{-# COMPILE AGDA2HS Action #-}

record C (m : Set → Set) (a : Set) : Set where
    constructor Conc
    field
        act : (a → Action m) → Action m
open C public
{-# COMPILE AGDA2HS C #-}

instance
    -- See https://wiki.haskell.org/Monad
    -- Paper doesn't provide Functor/Applicative instances because only since GHC 7.10 are those superclasses of Monad
    iFunctorC : ⦃ Monad m ⦄ → Functor (C m)
    iApplicativeC : ⦃ Monad m ⦄ → Applicative (C m)
    iMonadC : ⦃ Monad m ⦄ → Monad (C m)

    iMonadC ._>>=_ f k = Conc (λ c → f .act (λ a → (k a .act) c))
    {-# COMPILE AGDA2HS iMonadC #-}

    -- Odd notation here (having x to the right instead of to the left) because otherwise the termination checker complains
    iApplicativeC .pure = λ x → Conc (λ c → c x)
    iApplicativeC ._<*>_ mf ma = do 
                                    f ← mf
                                    a ← ma
                                    pure (f a)
    {-# COMPILE AGDA2HS iApplicativeC #-}

    iFunctorC .fmap f x = x >>= (pure ∘ f)
    {-# COMPILE AGDA2HS iFunctorC #-}

atom : ⦃ Monad m ⦄ → { @0 a : Set } → m a → C m a
atom m = Conc (λ c → Atom (fmap c m))
{-# COMPILE AGDA2HS atom #-}

action : ⦃ Monad m ⦄ → C m a → Action m
-- action m = m (λ _ → Stop)
action m = m .act (λ _ → Stop)
{-# COMPILE AGDA2HS action #-}

stop : ⦃ Monad m ⦄ → C m a
stop = Conc (λ _ → Stop)
{-# COMPILE AGDA2HS stop #-} 

par : ⦃ Monad m ⦄ → { @0 a : Set } → C m a → C m a → C m a
par m1 m2 = Conc (λ c → Fork (m1 .act c) (m2 .act c))
{-# COMPILE AGDA2HS par #-} 

fork : ⦃ Monad m ⦄ → { @0 a : Set } → C m a → C m ⊤
fork m = Conc (λ c → Fork (action m) (c tt))
{-# COMPILE AGDA2HS fork #-} 

instance
    iMonadTransC : MonadTrans C
    iMonadTransC .lift = atom
    {-# COMPILE AGDA2HS iMonadTransC #-}

{-# NON_TERMINATING #-} -- TODO: what are the consequences of this?
round_robin : ⦃ Monad m ⦄ → List (Action m) → m ⊤
round_robin [] = return tt
round_robin (Atom x ∷ xs) = do
                            x1 ← x
                            round_robin (xs ++ (x1 ∷ []))
round_robin (Fork x y ∷ xs) = round_robin (xs ++ (x ∷ y ∷ []))
round_robin (Stop ∷ xs) = round_robin xs
{-# COMPILE AGDA2HS round_robin #-}

run : ⦃ Monad m ⦄ → C m a → m ⊤
run m = round_robin (action m ∷ [])
{-# COMPILE AGDA2HS run #-}