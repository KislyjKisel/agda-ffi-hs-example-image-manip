{-# OPTIONS --without-K #-}

module Lab.Input.Empty where

open import Lab.Prelude
open import Lab.Input using (Input)


empty : Input
empty = record
    { State = ⊤
    ; Value = ⊤
    ; new   = pure tt
    ; load  = const $ pure tt
    ; ui    = const $ pure tt′
    }
