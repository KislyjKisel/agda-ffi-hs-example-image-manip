{-# OPTIONS --without-K #-}

module Lab.Rendering.Program where

open import Lab.Prelude
open import Data.Vec.Base as Vec using (Vec)
open import Data.Product using (_×_; _,_; proj₁; proj₂)
open import Ffi.Hs.Data.Foldable using (forM-)
open import Ffi.Hs.Control.Applicative using (unless)

import Ffi.Hs.Data.ByteString as BS
import Ffi.Hs.Graphics.Rendering.OpenGL.GL as GL

module VAll where
    open import Data.Vec.Relation.Unary.All public

    open Data.Product using (∃)

    toVec : ∀{aℓ pℓ} {n} {A : Set aℓ} {P : A → Set pℓ} {xs : Vec A n} → All P xs → Vec (∃ P) n
    toVec = reduce λ {x} px → x , px

    toList : ∀{aℓ pℓ} {n} {A : Set aℓ} {P : A → Set pℓ} {xs : Vec A n} → All P xs → List (∃ P)
    toList = Vec.toList ∘ toVec

Vec-forM : ∀{aℓ bℓ n} {A : Set aℓ} {B : Set bℓ} {M : Set bℓ → Set bℓ} → ⦃ Monad M ⦄ → Vec A n → (A → M B) → M (Vec B n)
Vec-forM Vec.[]       f = return $ Vec.[]
Vec-forM (x Vec.∷ xs) f = do
    y ← f x
    ys ← Vec-forM xs f
    return $ y Vec.∷ ys

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

-- Uniform[UniformValue[T]] : ∀{t} → GL.Uniform (UniformValue t)
-- Uniform[UniformValue[T]] {t} = ?

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

    uniform-locations ← Vec-forM uniforms (GL.uniformLocation program ∘ proj₂)
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
