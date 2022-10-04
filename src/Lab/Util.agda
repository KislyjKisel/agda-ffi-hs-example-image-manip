module Lab.Util where

open import Ffi.Hs.Prelude
open import Agda.Builtin.Int as ℤ            using ()
open import Agda.Builtin.Nat                 using (Nat)
open import Ffi.Hs.Codec.Picture.Types as JP using ()
open import Ffi.Hs.Foreign.Storable          using (sizeOf)
open import Ffi.Hs.GHC.Stack                 using (mkHasCallStack)

instance
    inst:mkHasCallStack = mkHasCallStack
    inst:Pixel[Pixel] = JP.Pixel[PixelRGBA8]
    inst:Pixel[A]⇒Storable[PixelBaseComponent[A]] = JP.Pixel[A]⇒Storable[PixelBaseComponent[A]]

fromℕ : ∀{A : Set} → ⦃ Num A ⦄ → Nat → A
fromℕ = fromInteger ∘ ℤ.pos

Pixel : Set
Pixel = JP.PixelRGBA8

Image : Set
Image = JP.Image Pixel

pixelComponentSize : Int
pixelComponentSize = sizeOf (JP.PixelBaseComponent Pixel :: undefined)

pixelSize : Int
pixelSize = pixelComponentSize * (fromℕ 4)
