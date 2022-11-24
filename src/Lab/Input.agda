{-# OPTIONS --without-K #-}

module Lab.Input where

open import Lab.Prelude


record Input : Set₁ where
    field
        State : Set
        Value : Set
        new   : IO State
        load  : State → IO Value
        ui    : State → IO {0ℓ} ⊤′
