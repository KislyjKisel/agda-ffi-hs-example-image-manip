{-# OPTIONS --without-K #-}

module Lab.Input.Scale2D where

open import Lab.Prelude
open import Lab.Input using (Input)

import Ffi.Hs.DearImGui as ImGui

scale2d : Input
scale2d = record
    { State = Tuple2 (IORef Float) (IORef Float)
    ; Value = Tuple2 Float Float
    ; new = mkTuple2
        <$> newIORef (f64⇒f32 1.0)
        <*> newIORef (f64⇒f32 1.0)

    ; load = λ (mkTuple2 refSx refSy) → mkTuple2
        <$> readIORef refSx
        <*> readIORef refSy

    ; ui = λ (mkTuple2 refSx refSy) → do
        ImGui.dragFloat "X scale coeff" refSx (f64⇒f32 0.1) (f64⇒f32 0.1) (f64⇒f32 32.0)
        ImGui.dragFloat "Y scale coeff" refSy (f64⇒f32 0.1) (f64⇒f32 0.1) (f64⇒f32 32.0)
        pure tt′
    }
