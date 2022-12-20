{-# OPTIONS --without-K #-}

module Lab.Rendering.Program.Textured2D where

open import Data.Product  using (_,_)
open import Data.Vec.Base using (Vec)

import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Shaders as GL

open import Lab.Prelude
open import Lab.Rendering.Program as Prog using (ShaderDescr; ProgramDescr)


textured2d : ProgramDescr
textured2d = record
    { uniforms =
        (Prog.uSampler2D , "uImageTexture") Vec.∷
        (Prog.uVec2      , "uOffset"      ) Vec.∷
        (Prog.uVec2      , "uScale"       ) Vec.∷
        Vec.[]

    ; shaders  =
        Prog.shader-file "image.frag" GL.FragmentShader ∷
        Prog.shader-file "image.vert" GL.VertexShader   ∷
        []
    }
