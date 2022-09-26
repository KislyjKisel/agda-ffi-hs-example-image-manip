{-# OPTIONS --without-K #-}

module Lab.Util where

open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Primitive

private
    variable
        aℓ : Level
        A : Set aℓ

if_then_else_ : Bool → A → A → A
if true  then x else _ = x
if false then _ else y = y
