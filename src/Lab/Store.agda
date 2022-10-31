{-# OPTIONS --without-K --allow-unsolved-metas #-}

module Lab.Store where

open import Agda.Primitive using (Level)

private
    variable
        ℓ : Level
        A B S : Set ℓ

record Store (S A : Set ℓ) : Set ℓ where
    constructor mkStore
    field
        peek : S → A
        pos  : S

open Store public

map : (A → B) → Store S A → Store S B
map = {!   !}

extract : Store S A → A
extract = {!   !}

duplicate : Store S A → Store S (Store S A)
duplicate = {!   !}

extend : (Store S A → B) → Store S A → Store S B
extend = {!   !}
