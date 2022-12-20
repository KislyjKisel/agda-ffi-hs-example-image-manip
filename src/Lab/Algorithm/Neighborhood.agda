{-# OPTIONS --without-K #-}

module Lab.Algorithm.Neighborhood where

open import Data.Nat.Divisibility                  using (_∣?_)
open import Relation.Nullary.Decidable.Core as Dec using ()

open import Ffi.Hs.Data.Word     using (Word8)
open import Ffi.Hs.Data.Foldable using (forM-)

open import Lab.Data.Vec as Vec using (Vec)
open import Lab.Prelude


red : Pixel → Word8
red (JP.mkPixelRGBA8 r g b a) = r

blue : Pixel → Word8
blue (JP.mkPixelRGBA8 r g b a) = b

green : Pixel → Word8
green (JP.mkPixelRGBA8 r g b a) = g

alpha : Pixel → Word8
alpha (JP.mkPixelRGBA8 r g b a) = a

percomp : ∀{n} → (Vec (Vec Word8 n) n → Word8) → Vec (Vec Pixel n) n → Pixel
percomp f p = JP.mkPixelRGBA8
    (f $ Vec.map (Vec.map red)   p)
    (f $ Vec.map (Vec.map green) p)
    (f $ Vec.map (Vec.map blue)  p)
    (f $ Vec.map (Vec.map alpha) p)

neighborhoodP : Bool → (size : ℕ) → ⦃ Dec.False (2 ∣? size) ⦄ → Pixel → (Vec (Vec Pixel size) size → Pixel) → Image → IO Image
neighborhoodP rec size border f src = case rec of λ
    { False → pure $ JP.generateImage (λ x y → f $ near x y) srcW srcH
    ; True → do
        dst ← JP.thawImage src
        forM- [ 0 ⋯ srcH - 1 ] λ y →
            forM- [ 0 ⋯ srcW - 1 ] λ x → do
                ns ← nearMut dst x y
                JP.writePixel dst x y (f ns)
        JP.freezeImage dst
    }
    where
    srcW = JP.imageWidth src
    srcH = JP.imageHeight src
    kernelOffset = ℕ.⌊ size /2⌋
    inside   = λ x y → x >= 0 && x < srcW && y >= 0 && y < srcH

    near : Int → Int → Vec (Vec Pixel size) size
    near x y = Vec.tabulate λ i → Vec.tabulate λ j →
        let x′ = x + fromℕ (Fin.toℕ j) - fromℕ kernelOffset
            y′ = y + fromℕ (Fin.toℕ i) - fromℕ kernelOffset
        in if inside x′ y′ then JP.pixelAt src x′ y′ else border

    nearMut : MImage → Int → Int → IO $ Vec (Vec Pixel size) size
    nearMut dst x y = Vec.tabulateIO λ i → Vec.tabulateIO λ j →
        let x′ = x + fromℕ (Fin.toℕ j) - fromℕ kernelOffset
            y′ = y + fromℕ (Fin.toℕ i) - fromℕ kernelOffset
        in if inside x′ y′ then JP.readPixel dst x′ y′ else (pure $ border)

neighborhoodC : Bool → (size : ℕ) → ⦃ Dec.False (2 ∣? size) ⦄ → Pixel → (Vec (Vec Word8 size) size → Word8) → Image → IO Image
neighborhoodC rec size border f = neighborhoodP rec size border (percomp f)
