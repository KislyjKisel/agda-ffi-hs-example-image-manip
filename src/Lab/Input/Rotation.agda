{-# OPTIONS --without-K #-}

module Lab.Input.Rotation where

open import Lab.Prelude
open import Lab.Input using (Input)

import Ffi.Hs.DearImGui as ImGui

Rotation : Input
Rotation = record
    { State = IORef Int
    ; Value = Int
    ; new   = newIORef 0
    ; load  = readIORef
    ; ui    = λ r → ImGui.dragInt "Rotation Angle" r (f64⇒f32 1.0) -180 180 >> pure tt′
    }
