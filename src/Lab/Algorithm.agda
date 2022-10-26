{-# OPTIONS --without-K #-}

module Lab.Algorithm where

open import Lab.Prelude
open import Lab.Input using (Input)

record Algorithm : Set₁ where
    field
        name  : Text
        input : Input
        run   : Input.Value input → Image → IO Image
