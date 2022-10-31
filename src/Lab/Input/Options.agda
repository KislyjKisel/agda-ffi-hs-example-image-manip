{-# OPTIONS --without-K #-}

module Lab.Input.Options where

open import Lab.Input using (Input)
open import Ffi.Hs.Control.Applicative using (when)
open import Ffi.Hs.Data.Foldable using (forM-)
open import Lab.Prelude

import Ffi.Hs.DearImGui as ImGui

options : Text → List Text → Input
options name opts = record
    { State = IORef Int
    ; Value = Int
    ; new   = newIORef 0
    ; load  = readIORef
    ; ui    = λ idxRef → do
        idxs0 ← readIORef idxRef
        liftℓ combo ← ImGui.beginCombo (Text.append name "\0") (Text.append (opts !! idxs0) "\0")
        when combo do
            forM- (zip opts [ 0 ⋯ length opts - 1 ]) λ (mkTuple2 opt idx) → do
                liftℓ selected ← ImGui.selectable (Text.append opt "\0")
                when selected $ writeIORef idxRef idx >> pure _
            ImGui.endCombo
    }
