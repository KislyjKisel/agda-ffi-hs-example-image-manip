{-# OPTIONS --without-K #-}

module Lab.Algorithm.Neighborhood.Kernel where

open import Data.Nat.Divisibility        using (_∣?_)
open import Data.Product                 using (_×_; _,_)
open import Data.Vec.Base         as Vec using (Vec; _∷_; [])
import Relation.Nullary.Decidable.Core as Dec

open import Ffi.Hs.Data.Ord  using (clamp)
open import Ffi.Hs.Data.Word using (Word8)

open import Lab.Algorithm              using (Algorithm)
open import Lab.Algorithm.Neighborhood using (neighborhoodC)
open import Lab.Prelude
import Lab.Input.Options as Input
import Lab.Input.Kernel  as Input


private
    f64⇒w8 : Double → Word8
    f64⇒w8 = floor ∘ clamp (mkTuple2 0.0 255.0)

Kernel : ℕ → Set
Kernel n = Bool × Double × Vec (Vec Double n) n

kernel : ∀{size} → ⦃ Dec.False (2 ∣? size) ⦄ → Kernel size → Image → IO Image
kernel {size} (krec , kk , km) = neighborhoodC krec size (JP.mkPixelRGBA8 0 0 0 0)
    (f64⇒w8 ∘ (_* kk) ∘ sum ∘ Vec.toList ∘ Vec.concat ∘ Vec.zipWith (Vec.zipWith (λ k → (_* k) ∘ realToFrac)) km)

kernel-custom : Algorithm
kernel-custom = record
    { name  = "Kernel (Custom)"
    ; input = Input.kernel 3
    ; run   = λ (k , ks) → kernel (False , realToFrac k , Vec.map (Vec.map realToFrac) ks)
    }

presets : List (Tuple2 Text (Kernel 3))
presets =
    mkTuple2 "Identity" (
        False ,
        1.0 ,
        (0.0 ∷ 0.0 ∷ 0.0 ∷ []) ∷
        (0.0 ∷ 1.0 ∷ 0.0 ∷ []) ∷
        (0.0 ∷ 0.0 ∷ 0.0 ∷ []) ∷
        []) ∷
    mkTuple2 "Smooth" (
        False ,
        1.0 / 9.0 ,
        (1.0 ∷ 1.0 ∷ 1.0 ∷ []) ∷
        (1.0 ∷ 1.0 ∷ 1.0 ∷ []) ∷
        (1.0 ∷ 1.0 ∷ 1.0 ∷ []) ∷
        []) ∷
    mkTuple2 "Edge" (
        False ,
        1.0 ,
        (-1.0 ∷ -1.0 ∷ -1.0 ∷ []) ∷
        (-1.0 ∷  9.0 ∷ -1.0 ∷ []) ∷
        (-1.0 ∷ -1.0 ∷ -1.0 ∷ []) ∷
        []) ∷
    mkTuple2 "Sharpen" (
        False ,
        1.0 ,
        ( 0.0 ∷ -1.0 ∷  0.0 ∷ []) ∷
        (-1.0 ∷  5.0 ∷ -1.0 ∷ []) ∷
        ( 0.0 ∷ -1.0 ∷  0.0 ∷ []) ∷
        []) ∷
    mkTuple2 "Weee" (
        True ,
        0.5 ,
        (0.5 ∷ 0.0 ∷ 0.0 ∷ []) ∷
        (0.0 ∷ 1.0 ∷ 0.0 ∷ []) ∷
        (0.0 ∷ 0.0 ∷ 0.5 ∷ []) ∷
        []) ∷
    []

kernel-preset : Algorithm
kernel-preset = record
    { name  = "Kernel (Preset)"
    ; input = Input.options "Presets" $ fst $ unzip presets
    ; run   = λ i → kernel (snd $ presets !! i)
    }
