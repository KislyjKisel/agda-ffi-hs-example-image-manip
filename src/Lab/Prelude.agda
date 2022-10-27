{-# OPTIONS --without-K #-}

module Lab.Prelude where

open import Ffi.Hs.Control.Monad.Primitive using (PrimState)
open import Ffi.Hs.Foreign.Storable        using (sizeOf)
open import Ffi.Hs.GHC.Stack               using (mkHasCallStack)


---------- Re-export

open import Ffi.Hs.Prelude           public
open import Ffi.Hs.Graphics.GL.Types public

open import Agda.Primitive public
    using ()
    renaming (lzero to 0ℓ; lsuc to sucℓ)

1ℓ = sucℓ 0ℓ
2ℓ = sucℓ 1ℓ

open import Ffi.Hs.Data.StateVar as SV public
    using (_$=_; get)

open import Ffi.Hs.Data.ObjectName public
    using (genObjectName)

open import Ffi.Hs.Data.Text public
    using (Text)

module Text = Ffi.Hs.Data.Text

open import Ffi.Hs.Data.IORef public
    using (IORef; newIORef; readIORef; writeIORef; modifyIORef)

open import Agda.Builtin.Int public
    using ()
    renaming (Int to ℤ)
    hiding (module Int)

module ℤ = Agda.Builtin.Int

open import Agda.Builtin.Nat public
    using ()
    renaming (Nat to ℕ)
    hiding (module Nat)

module ℕ = Agda.Builtin.Nat

open import Agda.Builtin.Equality public
    using (_≡_)

module JP where
    open import Ffi.Hs.Codec.Picture.Types public
    open import Ffi.Hs.Codec.Picture       public

module SDL where
    open import Ffi.Hs.SDL public

open import Ffi.Hs.Data.StateVar-Instanced                public
open import Ffi.Hs.Data.Word-Instanced                    public
open import Ffi.Hs.Data.Text-Instanced                    public
open import Ffi.Hs.Data.Int-Instanced                     public
open import Ffi.Hs.SDL-Instanced                          public
open import Ffi.Hs.Control.Monad.Primitive-Instanced      public
open import Ffi.Hs.Codec.Picture-Instanced                public
open import Ffi.Hs.Foreign.C.Types-Instanced              public
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL-Instanced public

--------------------


instance
    inst:mkHasCallStack = mkHasCallStack
    inst:Pixel[A]⇒Storable[PixelBaseComponent[A]] = JP.Pixel[A]⇒Storable[PixelBaseComponent[A]]

fromℕ : ∀{A : Set} → ⦃ Num A ⦄ → ℕ → A
fromℕ = fromInteger ∘ ℤ.pos

Pixel : Set
Pixel = JP.PixelRGBA8

Image : Set
Image = JP.Image Pixel

MImage : Set
MImage = JP.MutableImage (PrimState {0ℓ} IO) Pixel

f64⇒f32 : Double → Float
f64⇒f32 = realToFrac

pixelToV4f : Pixel → SDL.V4 Float
pixelToV4f (JP.mkPixelRGBA8 r g b a) =
    SDL.mkV4 (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)

v4fToPixel : SDL.V4 Float → Pixel
v4fToPixel (SDL.mkV4 r g b a) = JP.mkPixelRGBA8 (floor r) (floor g) (floor b) (floor a)
