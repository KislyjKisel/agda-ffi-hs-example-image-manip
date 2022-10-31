{-# OPTIONS --without-K #-}

module Lab.Algorithm.Neighborhood.Median where

open import Lab.Prelude
open import Lab.Algorithm using (Algorithm)
open import Lab.Algorithm.Neighborhood using (neighborhoodC; neighborhoodP)
open import Data.Vec.Base as Vec using ()
open import Ffi.Hs.Data.List using (sort; sortBy)
open import Function.Base using (_on_)

import Lab.Input.Empty as Inp

median : ∀{A : Set} → List A → A
median xs = xs !! div (length xs) 2

luma : Pixel → Double
luma (JP.mkPixelRGBA8 r g b _) =
    0.33 * realToFrac r +
    0.33 * realToFrac g +
    0.33 * realToFrac b

medianC : Algorithm
medianC = record
    { name  = "Median-C"
    ; input = Inp.empty
    ; run   = const $ neighborhoodC False 3
        (JP.mkPixelRGBA8 0 0 0 0)
        (median ∘ sort ∘ Vec.toList ∘ Vec.concat)
    }

medianP : Algorithm
medianP = record
    { name  = "Median-P"
    ; input = Inp.empty
    ; run   = const $ neighborhoodP False 3
        (JP.mkPixelRGBA8 0 0 0 0)
        (median ∘ sortBy (compare on luma) ∘ Vec.toList ∘ Vec.concat)
    }
