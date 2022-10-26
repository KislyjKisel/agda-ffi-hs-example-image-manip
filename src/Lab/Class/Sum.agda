{-# OPTIONS --without-K #-}

module Lab.Class.Sum where

open import Agda.Primitive using (_⊔_)

record Sum {aℓ bℓ} (A : Set aℓ) (B : Set bℓ) : Set (aℓ ⊔ bℓ) where
    field
        inject : A → B

open Sum ⦃...⦄ public

