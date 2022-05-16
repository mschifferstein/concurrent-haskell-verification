module Data.C_Pos where

open import Haskell.Prelude
open import Data.MonadTrans
open import Data.PosMonad
open import Data.Writer

data Action (m : PosMonad) : Set where
    Atom : (toMonad m) (Action m) → Action m
    Fork : Action m → Action m → Action m
    Stop : Action m
{-# COMPILE AGDA2HS Action #-}

C : PosMonad → Set → Set
C m a = (a → Action m) → Action m
{-# COMPILE AGDA2HS C #-}

instance
    -- See https://wiki.haskell.org/Monad
    -- Paper doesn't provide Functor/Applicative instances because only since GHC 7.10 are those superclasses of Monad
    iFunctorC : { @0 m : PosMonad } → Functor (C m)
    iApplicativeC : { @0 m : PosMonad } → Applicative (C m)
    iMonadC : { @0 m : PosMonad } → Monad (C m)

    iMonadC ._>>=_ = λ f k c → f (λ a → k a c)
    {-# COMPILE AGDA2HS iMonadC #-}

    iApplicativeC .pure = λ x c → c x
    iApplicativeC ._<*>_ mf ma = do 
                                    f ← mf
                                    a ← ma
                                    pure (f a)
    {-# COMPILE AGDA2HS iApplicativeC #-}

    iFunctorC .fmap f x = x >>= (pure ∘ f)
    {-# COMPILE AGDA2HS iFunctorC #-}

-- atom : { m : PosMonad } { α : Set } → toMonad m α → C m α
-- -- atom m = λ c → Atom ( fmap c m)
-- atom m = λ c → Atom ({! m >>= (λ a → pure (c a))!})
-- atom m = λ c → Atom (do
--                         a ← m
--                         return (c a))

-- atom : ⦃ Monad m ⦄ → { m1 : PosMonad } → { α : Set } → m α → C m1 α
-- atom m = λ c → Atom (fmap c m)
-- atom m = λ c → Atom (m >>= (λ a → return (c a)))
-- atom m = λ c → Atom (do
--                         a ← m
--                         return (c a))
-- atom m = fmap ?
-- atom m = λ c → Atom (do
--                         a ← toMonad m
--                         ?)
-- atom x = λ c → Atom ({! x >>= (λ a → return (c a))  !})
-- atom a x = λ c → Atom (toMonad x a >>= (λ a1 → return (c a1)))
-- atom a x = λ c → Atom ({! (toMonad x) a >>= ?  !})
-- {-# COMPILE AGDA2HS atom #-}

-- atom : ⦃ Monad m ⦄ → { m1 : PosMonad } → { α : Set } → m α → C m1 α

-- action : ⦃ Monad m ⦄ → C m a → Action m
action : { @0 m : PosMonad } → C m a → Action m
action m = m (λ _ → Stop)
{-# COMPILE AGDA2HS action #-}

-- stop : ⦃ Monad m ⦄ → C m a
stop : { @0 m : PosMonad } → C m a
stop = λ _ → Stop
{-# COMPILE AGDA2HS stop #-} 

-- par : ⦃ Monad m ⦄ → { α : Set } → C m α → C m α → C m α
par : { @0 m : PosMonad }  → { α : Set } → C m α → C m α → C m α
par m₁ m₂ = λ c → Fork (m₁ c) (m₂ c)
{-# COMPILE AGDA2HS par #-} 

-- fork : ⦃ Monad m ⦄ → { α : Set } → C m α → C m ⊤
fork : { @0 m : PosMonad } → { α : Set } → C m α → C m ⊤
fork m = λ c → Fork (action m) (c tt)
{-# COMPILE AGDA2HS fork #-} 

-- instance
--     iMonadTransC : MonadTrans C
--     iMonadTransC .lift = atom
--     {-# COMPILE AGDA2HS iMonadTransC #-}