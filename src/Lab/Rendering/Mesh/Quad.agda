{-# OPTIONS --without-K #-}

module Lab.Rendering.Mesh.Quad where

open import Lab.Prelude

open import Ffi.Hs.Foreign.Ptr using (Ptr; nullPtr)
open import Ffi.Hs.Foreign.Marshal.Array using (withArray)
open import Ffi.Hs.Foreign.Storable using (sizeOf)
open import Lab.Class.Level using (liftℓ1)

import Ffi.Hs.Graphics.Rendering.OpenGL.GL as GL

record Quad : Set where
    field
        vao           : GL.VertexArrayObject
        vbo-positions : GL.BufferObject
        vbo-texcoords : GL.BufferObject
        vertex-count  : Int

    render : ∀{ℓ} → IO {ℓ} ⊤′
    render = do
        GL.bindVertexArrayObject $= Just vao
        liftℓ1 $ GL.drawArrays GL.Triangles 0 (fromIntegral vertex-count)
        GL.bindVertexArrayObject $= Nothing

new : IO Quad
new = do
    vao ← IO GL.VertexArrayObject ∋ genObjectName
    GL.bindVertexArrayObject $= Just vao

    -- Positions buffer
    let positions : List (GL.Vertex2 GLfloat)
        positions =
            GL.mkVertex2 (realToFrac -1.0) (realToFrac  1.0) ∷
            GL.mkVertex2 (realToFrac -1.0) (realToFrac -1.0) ∷
            GL.mkVertex2 (realToFrac  1.0) (realToFrac -1.0) ∷
            GL.mkVertex2 (realToFrac -1.0) (realToFrac  1.0) ∷
            GL.mkVertex2 (realToFrac  1.0) (realToFrac -1.0) ∷
            GL.mkVertex2 (realToFrac  1.0) (realToFrac  1.0) ∷
            []

        positionCount    = length positions
        positionLocation = GL.mkAttribLocation 0

    positionsBuffer ← IO GL.BufferObject ∋ genObjectName
    GL.bindBuffer GL.ArrayBuffer $= Just positionsBuffer
    withArray positions λ ptr → do
        let size = fromIntegral (positionCount * sizeOf (head positions))
        GL.bufferData GL.ArrayBuffer $= mkTuple3 size ptr GL.StaticDraw

    GL.vertexAttribArray positionLocation $= GL.Enabled
    GL.vertexAttribPointer positionLocation $= mkTuple2
        GL.ToFloat
        (GL.mkVertexArrayDescriptor 2 GL.Float 0 (Ptr ⊤ ∋ nullPtr))


    -- Texcoords buffer
    let texcoords : List (GL.Vertex2 GLfloat)
        texcoords =
            GL.mkVertex2 (realToFrac 0.0) (realToFrac 0.0) ∷
            GL.mkVertex2 (realToFrac 0.0) (realToFrac 1.0) ∷
            GL.mkVertex2 (realToFrac 1.0) (realToFrac 1.0) ∷
            GL.mkVertex2 (realToFrac 0.0) (realToFrac 0.0) ∷
            GL.mkVertex2 (realToFrac 1.0) (realToFrac 1.0) ∷
            GL.mkVertex2 (realToFrac 1.0) (realToFrac 0.0) ∷
            []

        texcoordCount    = length texcoords
        texcoordLocation = GL.mkAttribLocation 1

    texcoordsBuffer ← IO GL.BufferObject ∋ genObjectName
    GL.bindBuffer GL.ArrayBuffer $= Just texcoordsBuffer
    withArray texcoords λ ptr → do
        let size = fromIntegral (texcoordCount * sizeOf (head texcoords))
        GL.bufferData GL.ArrayBuffer $= mkTuple3 size ptr GL.StaticDraw

    GL.vertexAttribPointer texcoordLocation $= mkTuple2
        GL.ToFloat
        (GL.mkVertexArrayDescriptor 2 GL.Float 0 (Ptr ⊤ ∋ nullPtr))
    GL.vertexAttribArray texcoordLocation $= GL.Enabled

    return record
        { vao           = vao
        ; vbo-positions = positionsBuffer
        ; vbo-texcoords = texcoordsBuffer
        ; vertex-count  = positionCount
        }
