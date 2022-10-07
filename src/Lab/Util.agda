module Lab.Util where

open import Agda.Builtin.Int as ℤ          using ()
open import Agda.Builtin.Nat               using (Nat)
open import Agda.Primitive                 using (lzero)
open import Ffi.Hs.Control.Monad.Primitive using (PrimState; PrimMonad[IO])
open import Ffi.Hs.Data.Word as Word       using ()
open import Ffi.Hs.Foreign.Storable        using (sizeOf)
open import Ffi.Hs.GHC.Stack               using (mkHasCallStack)
open import Ffi.Hs.Linear.V4               using (Functor[V4]; Additive[V4])
open import Ffi.Hs.Prelude

import Ffi.Hs.Codec.Picture.Types as JP
import Ffi.Hs.Codec.Picture as JP

import Ffi.Hs.SDL.Vect as SDL

instance
    inst:mkHasCallStack = mkHasCallStack
    inst:Pixel[Pixel] = JP.Pixel[PixelRGBA8]
    inst:Pixel[A]⇒Storable[PixelBaseComponent[A]] = JP.Pixel[A]⇒Storable[PixelBaseComponent[A]]

    inst:Functor[V4] = Functor[V4]
    inst:Additive[V4] = Additive[V4]

    _ = Word.Real[Word8]
    _ = Word.Integral[Word8]
    inst:Num[Word8] = Word.Num[Word8]

    inst:PrimMonad[IO] = PrimMonad[IO]

fromℕ : ∀{A : Set} → ⦃ Num A ⦄ → Nat → A
fromℕ = fromInteger ∘ ℤ.pos

Pixel : Set
Pixel = JP.PixelRGBA8

Image : Set
Image = JP.Image Pixel

MImage : Set
MImage = JP.MutableImage (PrimState {lzero} IO) Pixel

pixelComponentSize : Int
pixelComponentSize = sizeOf (JP.PixelBaseComponent Pixel :: undefined)

pixelSize : Int
pixelSize = pixelComponentSize * (fromℕ 4)

doubleToFloat : Double → Float
doubleToFloat = realToFrac

pixelToV4f : Pixel → SDL.V4 Float
pixelToV4f (JP.mkPixelRGBA8 r g b a) =
    SDL.mkV4 (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)

v4fToPixel : SDL.V4 Float → Pixel
v4fToPixel (SDL.mkV4 r g b a) = JP.mkPixelRGBA8 (floor r) (floor g) (floor b) (floor a)
