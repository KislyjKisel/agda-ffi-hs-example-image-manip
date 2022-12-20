{-# OPTIONS --without-K #-}

module Lab.Data.Vec.Relation.Unary.All where

open import Data.List.Base       using (List)
open import Data.Product         using (∃; _,_)
open import Data.Vec.Base as Vec using (Vec)
open import Function.Base        using (_∘′_)

open import Data.Vec.Relation.Unary.All public


toVec : ∀{aℓ pℓ} {n} {A : Set aℓ} {P : A → Set pℓ} {xs : Vec A n} → All P xs → Vec (∃ P) n
toVec = reduce λ {x} px → x , px

toList : ∀{aℓ pℓ} {n} {A : Set aℓ} {P : A → Set pℓ} {xs : Vec A n} → All P xs → List (∃ P)
toList = Vec.toList ∘′ toVec
