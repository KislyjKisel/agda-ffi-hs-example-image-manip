{-# OPTIONS --without-K #-}

module Lab.Rendering.Program where

open import Data.Product  using (_×_; _,_; proj₁; proj₂)

open import Ffi.Hs.Control.Applicative using (unless)
open import Ffi.Hs.Data.Foldable       using (forM-)
import Ffi.Hs.Data.ByteString              as BS
import Ffi.Hs.Graphics.Rendering.OpenGL.GL as GL

open import Lab.Data.Vec.Relation.Unary.All as VAll using ()
open import Lab.Data.Vec as Vec using (Vec)
open import Lab.Prelude


data ShaderDescr : Set where
    shader-file : String → GL.ShaderType → ShaderDescr

load-shader : ShaderDescr → IO GL.Shader
load-shader (shader-file filePath shaderType) = do
    shader ← GL.createShader shaderType
    source ← BS.readFile filePath
    GL.shaderSourceBS shader $= source
    GL.compileShader shader
    return shader

data UniformType : Set where
    uSampler2D : UniformType
    uVec2      : UniformType

UniformValue : UniformType → Set
UniformValue uSampler2D = GLint
UniformValue uVec2      = GL.Vector2 GLfloat

uniform : ∀{ℓ t} → GL.UniformLocation → UniformValue t → IO {ℓ} ⊤′
uniform {t = uSampler2D} loc val = GL.uniform loc $= val
uniform {t = uVec2}      loc val = GL.uniform loc $= val

record ProgramDescr : Set where
    field
        {uniform-count} : ℕ
        uniforms        : Vec (UniformType × String) uniform-count
        shaders         : List ShaderDescr

    UniformValues : Set
    UniformValues = VAll.All UniformValue (Vec.map proj₁ uniforms)

record Program (descr : ProgramDescr) : Set where
    open ProgramDescr descr
    field
        program           : GL.Program
        uniform-locations : Vec GL.UniformLocation uniform-count

new : (descr : ProgramDescr) → IO (Program descr)
new descr = let open ProgramDescr descr in do
    program ← GL.createProgram

    forM- shaders λ shaderDescr → do
        shader ← load-shader shaderDescr
        GL.attachShader program shader

    GL.linkProgram program
    linkOk ← get $ GL.linkStatus program
    unless linkOk $ do
        progLog ← ("PROG: " ++_) <$> GL.programInfoLog program
        fail $ "Shader program link failed: " ++ progLog

    uniform-locations ← Vec.iforM uniforms (const $ GL.uniformLocation program ∘ proj₂)
    pure $ record
        { program           = program
        ; uniform-locations = uniform-locations
        }

bind : ∀{ℓ descr} → Program descr → (ProgramDescr.UniformValues descr) → IO {ℓ} ⊤′
bind prog us = let open Program prog in do
    GL.currentProgram $= Just program
    forM- (zip (VAll.toList us) (Vec.toList uniform-locations))
        λ (mkTuple2 (_ , val) loc) → uniform loc val

unbind : ∀{ℓ} → IO {ℓ} ⊤′
unbind = GL.currentProgram $= Nothing
