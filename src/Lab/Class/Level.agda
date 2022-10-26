{-# OPTIONS --without-K #-}

module Lab.Class.Level where

open import Agda.Primitive             using (Setω; _⊔_)
open import Ffi.Hs.-base.Level         using (Liftℓ; liftℓ; unliftℓ)
open import Ffi.Hs.Control.Applicative using (Applicative; Applicative[F]⇒Functor[F]; pure; _<*>_)
open import Ffi.Hs.Control.Monad       using (Monad; return; _>>_; _>>=_)
open import Ffi.Hs.Data.Function       using (_∘_; _$_)
open import Ffi.Hs.Data.Functor        using (Functor; fmap; _<$>_; _<$_)

Functorℓ : (∀{ℓ} → Set ℓ → Set ℓ) → Setω
Functorℓ F = ∀{ℓ} → Functor (F {ℓ})

Applicativeℓ : (∀{ℓ} → Set ℓ → Set ℓ) → Setω
Applicativeℓ F = ∀{ℓ} → Applicative (F {ℓ})

Monadℓ : (∀{ℓ} → Set ℓ → Set ℓ) → Setω
Monadℓ M = ∀{ℓ} → Monad (M {ℓ})

-- foldable, traversable, ...?

record Liftℓ1 (F : ∀{ℓ} → Set ℓ → Set ℓ) : Setω where
    field
        liftℓ1   : ∀{bℓ aℓ} {A : Set aℓ} → F A → F (Liftℓ bℓ A)
        unliftℓ1 : ∀{bℓ aℓ} {A : Set aℓ} → F (Liftℓ bℓ A) → F A

    -- infixl 4 _<$!>_
    infixl 1 _>>=ℓ_ _>>ℓ_
    infixr 1 _=<<ℓ_ -- _>=>_ _<=<_
    infixl 4 _<$ℓ_ {- _$>_ -} _<$>ℓ_
    -- infixl 1 _<&>_
    infixl 4 _<*>ℓ_ -- _*>_ _<*_ _<**>_
    -- infixl 3 _<|>_

    fmapℓ : ∀{aℓ bℓ} {A : Set aℓ} {B : Set bℓ} → ⦃ Functor (F {aℓ ⊔ bℓ}) ⦄ → (A → B) → F A → F B
    fmapℓ {aℓ} {bℓ} f = unliftℓ1{aℓ ⊔ bℓ} ∘ (fmap $ liftℓ ∘ f ∘ unliftℓ) ∘ liftℓ1{aℓ ⊔ bℓ}

    _<$>ℓ_ = fmapℓ

    _<$ℓ_ : ∀{aℓ bℓ} {A : Set aℓ} {B : Set bℓ} → ⦃ Functor (F {aℓ ⊔ bℓ}) ⦄ → A → F B → F A
    _<$ℓ_ {aℓ} {bℓ} x y = unliftℓ1{aℓ ⊔ bℓ} $ liftℓ x <$ liftℓ1{aℓ ⊔ bℓ} y

    pureℓ : ∀{aℓ bℓ} {A : Set aℓ} → ⦃ Applicative (F {aℓ ⊔ bℓ}) ⦄ → A → F (Liftℓ bℓ A)
    pureℓ = pure ∘ liftℓ

    _<*>ℓ_ : ∀{aℓ bℓ} {A : Set aℓ} {B : Set bℓ} → ⦃ Applicative (F {aℓ ⊔ bℓ}) ⦄ → F (A → B) → F A → F B
    _<*>ℓ_ {aℓ} {bℓ} f = unliftℓ1{aℓ ⊔ bℓ} ∘
        ((liftℓ ∘_) ∘ (_∘ unliftℓ) <$> f <*>_) ∘
        liftℓ1{aℓ ⊔ bℓ}
        where instance _ = Applicative[F]⇒Functor[F]

    returnℓ : ∀{aℓ bℓ} {A : Set aℓ} → ⦃ Monad (F {aℓ ⊔ bℓ}) ⦄ → A → F (Liftℓ bℓ A)
    returnℓ = return ∘ liftℓ

    _>>ℓ_ : ∀{aℓ bℓ} {A : Set aℓ} {B : Set bℓ} → ⦃ Monad (F {aℓ ⊔ bℓ}) ⦄ → F A → F B → F B
    _>>ℓ_ {aℓ} {bℓ} x y = unliftℓ1{aℓ ⊔ bℓ} $ liftℓ1{aℓ ⊔ bℓ} x >> liftℓ1 y

    _>>=ℓ_ : ∀{aℓ bℓ} {A : Set aℓ} {B : Set bℓ} → ⦃ Monad (F {aℓ ⊔ bℓ}) ⦄ → F A → (A → F B) → F B
    _>>=ℓ_ {aℓ} {bℓ} x f = unliftℓ1{aℓ ⊔ bℓ} (liftℓ1{aℓ ⊔ bℓ} x >>= liftℓ1 ∘ f ∘ unliftℓ)

    _=<<ℓ_ : ∀{aℓ bℓ} {A : Set aℓ} {B : Set bℓ} → ⦃ Monad (F {aℓ ⊔ bℓ}) ⦄ → (A → F B) → F A → F B
    _=<<ℓ_ f x = x >>=ℓ f

open Liftℓ1 ⦃...⦄ public

from-mapℓ : {F : ∀{ℓ} → Set ℓ → Set ℓ} → (∀{aℓ bℓ}{A : Set aℓ} {B : Set bℓ} → (A → B) → F A → F B) → Liftℓ1 F
from-mapℓ mapℓ = record
    { liftℓ1   = mapℓ liftℓ
    ; unliftℓ1 = mapℓ unliftℓ
    }



open import Agda.Builtin.IO using (IO)

postulate
    IO-mapℓ : ∀{aℓ bℓ}{A : Set aℓ} {B : Set bℓ} → (A → B) → IO A → IO B

{-# COMPILE GHC IO-mapℓ = \ aℓ bℓ a b -> fmap #-}

instance
    inst:Liftℓ1[IO] : Liftℓ1 IO
    inst:Liftℓ1[IO] = from-mapℓ IO-mapℓ
