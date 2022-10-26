{-# OPTIONS --without-K --safe #-}

module Lab.Class.Product where

open import Agda.Primitive using (_⊔_)
open import Data.Fin.Base  using (Fin)
open import Data.List.Base using (List)
open import Data.Nat.Base  using (ℕ)
open import Data.Vec.Base  using (Vec)

record Product {aℓ bℓ} (A : Set aℓ) (B : Set bℓ) : Set (aℓ ⊔ bℓ) where
    field
        extract : B → A
        update  : (A → A) → B → B

open Product ⦃...⦄ public

record ProductAll {aℓ bℓ} (A : Set aℓ) (n : ℕ) (B : Set bℓ) : Set (aℓ ⊔ bℓ) where
    field
        extract-all  : B → Vec A n
   -- ? extract-some : (m : ℕ) → m ≤ n → B → Vec A m
        extract‼     : Fin n → B → A
        update-all   : (Vec A n → Vec A n) → B → B
        update‼      : Fin n → (A → A) → B → B

open ProductAll ⦃...⦄ public

-- bounded: Vec< n
