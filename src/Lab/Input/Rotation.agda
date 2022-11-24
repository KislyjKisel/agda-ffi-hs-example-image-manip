{-# OPTIONS --without-K #-}

module Lab.Input.Rotation where

import Ffi.Hs.DearImGui as ImGui

open import Lab.Input   using (Input)
open import Lab.Prelude


rotation : Input
rotation = record
    { State = IORef Int
    ; Value = Int
    ; new   = newIORef 0
    ; load  = readIORef
    ; ui    = λ r → do
        ImGui.dragInt "Rotation Angle" r (f64⇒f32 1.0) -180 180
        pure tt′
    }
