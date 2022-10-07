module Lab.ImageBox where

open import Ffi.Hs.Prelude
open import Lab.Util

open import Ffi.Hs.Data.IORef as IORef using (IORef; newIORef; readIORef)
open import Ffi.Hs.Foreign.C.Types as FC using (CInt)
open import Ffi.Hs.Foreign.ForeignPtr using (ForeignPtr)
open import Relation.Binary.PropositionalEquality using (subst)

import Agda.Builtin.Int as ℤ
open import Agda.Primitive using (lzero)

import Ffi.Hs.SDL.Vect           as SDL
import Ffi.Hs.SDL.Video.Renderer as SDL

import Ffi.Hs.Data.Vector.Storable as Vector

import Ffi.Hs.Codec.Picture as JP

import Ffi.Hs.Data.ByteString as BS
import Ffi.Hs.Data.ByteString.Internal as BS

instance
    _ = SDL.Functor[V2]
    _ = FC.Enum[CInt]

record ImageBox : Set where
    field
        content : Maybe (Tuple2 Image SDL.Texture)
        vpPos : SDL.V2 CInt

    width : Int
    width = maybe (fromIntegral $ ℤ.pos 0) (JP.imageWidth ∘ fst) content

    height : Int
    height = maybe (fromIntegral $ ℤ.pos 0) (JP.imageHeight ∘ fst) content

    size : SDL.V2 Int
    size = SDL.mkV2 width height

    vpRect : SDL.Rectangle CInt
    vpRect = SDL.mkRectangle (SDL.P vpPos) (toEnum <$> size)

    unload : IO {lzero} ⊤′
    unload = maybe (pure _) (λ (mkTuple2 _ t) → SDL.destroyTexture t) content

    load : SDL.Renderer → Image → IO ImageBox
    load renderer img = do
        unload
        let imgData = JP.Image.imageData img
        let size = SDL.mkV2 (JP.imageWidth img) (JP.imageHeight img)
        let imgDataLen = Vector.length imgData * pixelComponentSize
        texture ← unliftℓ <$> SDL.createTexture renderer SDL.ABGR8888 SDL.TextureAccessStatic (toEnum <$> size)

        let imgDataPtr = subst ForeignPtr JP.PixelBaseComponent[PixelRGBA8] $ fst $
                Vector.unsafeToForeignPtr0 imgData

        SDL.updateTexture texture Nothing
            (BS.fromForeignPtr0 imgDataPtr imgDataLen)
            (toEnum $ pixelSize * JP.imageWidth img)

        return $ record
            { content = Just $ mkTuple2 img texture
            ; vpPos   = vpPos
            }

    loadFile : SDL.Renderer → String → IO (Either String ImageBox)
    loadFile renderer path = do
        eimg ← JP.readImage path
        either (pure ∘ Left) (λ img → Right <$> load renderer (JP.convertRGBA8 img)) eimg

    render : SDL.Renderer → IO {lzero} ⊤′
    render renderer = do
        maybe (return _) (λ (mkTuple2 _ t) → SDL.copy renderer t Nothing (Just vpRect)) content

withImageBoxRef : ∀{A : Set} → SDL.V2 CInt → (IORef ImageBox → IO A) → IO A
withImageBoxRef vpPos f = do
    ref ← newIORef $ record { content = Nothing ; vpPos = vpPos }
    res ← f ref
    imgBox' ← readIORef ref
    ImageBox.unload imgBox'
    pure res
