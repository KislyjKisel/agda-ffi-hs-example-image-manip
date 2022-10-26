{-# OPTIONS --without-K #-}

module Lab.Input.Scale2D where

open import Lab.Prelude
open import Lab.Input using (Input)

import Ffi.Hs.DearImGui as ImGui

Scale2D : Input
Scale2D = record
    { State = Tuple2 (IORef Float) (IORef Float)
    ; Value = Tuple2 Float Float
    ; new = mkTuple2
        <$> newIORef (doubleToFloat 1.0)
        <*> newIORef (doubleToFloat 1.0)

    ; load = λ (mkTuple2 refSx refSy) → mkTuple2
        <$> readIORef refSx
        <*> readIORef refSy

    ; ui = λ (mkTuple2 refSx refSy) → do
        ImGui.dragFloat "X scale coeff" refSx (doubleToFloat 0.1) (doubleToFloat 0.1) (doubleToFloat 32.0)
        ImGui.dragFloat "Y scale coeff" refSy (doubleToFloat 0.1) (doubleToFloat 0.1) (doubleToFloat 32.0)
        pure tt′
    }
