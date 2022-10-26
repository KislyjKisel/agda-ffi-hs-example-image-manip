{-# OPTIONS --without-K #-}

module Lab.ImageBox where

open import Lab.Prelude

open import Data.Vec.Relation.Unary.All           as VAll using ()
open import Ffi.Hs.Control.Monad.IO.Class                 using (liftIO)
open import Ffi.Hs.Control.Monad.Trans.Class              using (lift)
open import Ffi.Hs.Control.Monad.Trans.Except             using (ExceptT; mkExceptT)
open import Ffi.Hs.Control.Monad.Trans.Reader             using (ReaderT; ask)
open import Ffi.Hs.Foreign.ForeignPtr                     using (ForeignPtr)
open import Ffi.Hs.Foreign.ForeignPtr.Unsafe      as FPtr using ()
open import Lab.Class.Product                             using (Product; extract)
open import Lab.Rendering.Mesh.Quad               as Mesh using ()
open import Lab.Rendering.Program                 as Prog using (Program)
open import Lab.Rendering.Program.Textured2D      as Prog using ()
open import Relation.Binary.PropositionalEquality         using (subst)
open import Lab.Class.Level                               using (liftℓ1)

import Ffi.Hs.Control.Monad.Trans.Reader-Instanced
import Ffi.Hs.Control.Monad.Trans.Except-Instanced

import Ffi.Hs.Data.Vector.Storable         as Vector
import Ffi.Hs.Data.ByteString              as BS
import Ffi.Hs.Data.ByteString.Internal     as BS
import Ffi.Hs.Graphics.Rendering.OpenGL.GL as GL


record ImageBox : Set where
    field
        content  : IORef (Maybe (Tuple2 Image GL.TextureObject))
        position : GL.Vector2 GLfloat
        scale    : GL.Vector2 GLfloat

    contentWidth : IO Int
    contentWidth = readIORef content >>= pure ∘ maybe 0 (JP.imageWidth ∘ fst)

    contentHeight : IO Int
    contentHeight = readIORef content >>= pure ∘ maybe 0 (JP.imageHeight ∘ fst)

    load : Image → IO {0ℓ} ⊤′
    load img = do
        -- ? unload
        let imgData    = JP.Image.imageData img
            width      = JP.imageWidth img
            height     = JP.imageHeight img
            -- size       = SDL.mkV2 width height
            -- imgDataLen = Vector.length imgData * pixelComponentSize
            imgDataPtr = subst ForeignPtr JP.PixelBaseComponent[PixelRGBA8] $ fst $
                Vector.unsafeToForeignPtr0 imgData

        texture ← genObjectName
        GL.activeTexture $= GL.mkTextureUnit 0
        GL.textureBinding GL.Texture2D $= Just texture
        GL.textureFilter GL.Texture2D $= mkTuple2 (mkTuple2 GL.Nearest Nothing) GL.Nearest
        GL.textureWrapMode GL.Texture2D GL.S $= mkTuple2 GL.Repeated GL.ClampToEdge
        GL.textureWrapMode GL.Texture2D GL.T $= mkTuple2 GL.Repeated GL.ClampToEdge
        GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 (GL.mkTextureSize2D (toEnum width) (toEnum height)) 0 (GL.mkPixelData GL.RGBA GL.UnsignedByte $ FPtr.unsafeForeignPtrToPtr imgDataPtr)
        GL.textureBinding GL.Texture2D $= Nothing

        writeIORef content (Just (mkTuple2 img texture))
        pure _

    loadFile : String → ExceptT String IO ⊤′
    loadFile path = do
        img ← mkExceptT $ JP.readImage path
        liftIO $ load (JP.convertRGBA8 img)

    render : ∀{Env : Set 1ℓ} →
        ⦃ Product Mesh.Quad Env ⦄ →
        ⦃ Product (Program Prog.textured2d) Env ⦄ →
        ReaderT Env IO ⊤′
    render {M} = do
        liftℓ (Just (mkTuple2 _ texture)) ← liftIO $ liftℓ1 $ readIORef content
            where liftℓ Nothing → pure _
        env ← ask
        liftIO $ Prog.bind (extract env) $
            0        VAll.∷
            position VAll.∷
            scale    VAll.∷
            VAll.[]

        GL.activeTexture $= GL.mkTextureUnit 0
        GL.textureBinding GL.Texture2D $= Just texture
        liftIO $ Mesh.Quad.render (extract env)
        GL.textureBinding GL.Texture2D $= Nothing

new : GL.Vector2 GLfloat → GL.Vector2 GLfloat → IO ImageBox
new position scale = newIORef Nothing >>= λ content → pure record
    { content  = content
    ; position = position
    ; scale    = scale
    }
