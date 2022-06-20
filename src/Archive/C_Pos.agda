module Archive.C_Pos where

open import Haskell.Prelude
open import Data.MonadTrans
open import Archive.PosMonad
open import Data.Writer
open import Data.Swierstra

{-
Version of C in which I try to work with PosMonad (rather than turning off the positivity checker).
This is not yet working, in particular for the `atom` function and the MonadTrans instance.
Both C as type (commented out) and C as record are included.
-}

data Action (m : PosMonad) : Set where
    Atom : (toMonad m) (Action m) → Action m
    Fork : Action m → Action m → Action m
    Stop : Action m
{-# COMPILE AGDA2HS Action #-}

-- C : PosMonad → Set → Set
-- C m a = (a → Action m) → Action m
-- {-# COMPILE AGDA2HS C #-}

record C (m : PosMonad) (a : Set) : Set where
    constructor Conc
    field
        act : (a → Action m) → Action m
open C public
{-# COMPILE AGDA2HS C #-}

instance
    -- See https://wiki.haskell.org/Monad
    -- Paper doesn't provide Functor/Applicative instances because only since GHC 7.10 are those superclasses of Monad
    iFunctorC : { @0 m : PosMonad } → Functor (C m)
    iApplicativeC : { @0 m : PosMonad } → Applicative (C m)
    iMonadC : { @0 m : PosMonad } → Monad (C m)

    iMonadC ._>>=_ f k = Conc (λ c → f .act (λ a → (k a .act) c))
    {-# COMPILE AGDA2HS iMonadC #-}

    iApplicativeC .pure = λ x → Conc (λ c → c x)
    iApplicativeC ._<*>_ mf ma = do 
                                    f ← mf
                                    a ← ma
                                    pure (f a)
    {-# COMPILE AGDA2HS iApplicativeC #-}

    iFunctorC .fmap f x = x >>= (pure ∘ f)
    {-# COMPILE AGDA2HS iFunctorC #-}

-- TODO: doesn't recognize m as a monad/functor instance
atom : { m : PosMonad } { α : Set } → toMonad m α → C m α
-- atom m = λ c → Atom ( fmap c m)
-- atom m = Conc (λ c → Atom (fmap ? ?))
-- atom m = Conc (λ c → Atom ({! m >>= (λ a → pure (c a))!}))
-- atom m = Conc (λ c → Atom (do
--                         a ← iFunctorPMProof m
--                         return (c a)))
atom m = {!   !}

-- -- atom : ⦃ Monad m ⦄ → { m1 : PosMonad } → { α : Set } → m α → C m1 α
-- -- atom m = λ c → Atom (fmap c m)
-- -- atom m = λ c → Atom (m >>= (λ a → return (c a)))
-- -- atom m = λ c → Atom (do
-- --                         a ← m
-- --                         return (c a))
-- -- atom m = fmap ?
-- -- atom m = λ c → Atom (do
-- --                         a ← toMonad m
-- --                         ?)
-- -- atom x = λ c → Atom ({! x >>= (λ a → return (c a))  !})
-- -- atom a x = λ c → Atom (toMonad x a >>= (λ a1 → return (c a1)))
-- -- atom a x = λ c → Atom ({! (toMonad x) a >>= ?  !})
-- -- {-# COMPILE AGDA2HS atom #-}

-- -- atom : ⦃ Monad m ⦄ → { m1 : PosMonad } → { α : Set } → m α → C m1 α

-- action : ⦃ Monad m ⦄ → C m a → Action m
action : { @0 m : PosMonad } → C m a → Action m
action m = m .act (λ _ → Stop)
{-# COMPILE AGDA2HS action #-}

-- stop : ⦃ Monad m ⦄ → C m a
stop : { @0 m : PosMonad } → C m a
stop = Conc (λ _ → Stop)
{-# COMPILE AGDA2HS stop #-} 

-- par : ⦃ Monad m ⦄ → { α : Set } → C m α → C m α → C m α
par : { @0 m : PosMonad }  → { α : Set } → C m α → C m α → C m α
par m₁ m₂ = Conc (λ c → Fork (m₁ .act c) (m₂ .act c))
{-# COMPILE AGDA2HS par #-} 

-- fork : ⦃ Monad m ⦄ → { α : Set } → C m α → C m ⊤
fork : { @0 m : PosMonad } → { α : Set } → C m α → C m ⊤
fork m = Conc (λ c → Fork (action m) (c tt))
{-# COMPILE AGDA2HS fork #-} 

-- instance
--     iMonadTransC : MonadTrans C
--     iMonadTransC .lift = atom
--     {-# COMPILE AGDA2HS iMonadTransC #-}