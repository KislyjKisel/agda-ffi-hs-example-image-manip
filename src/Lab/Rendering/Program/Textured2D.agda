{-# OPTIONS --without-K #-}

module Lab.Rendering.Program.Textured2D where

open import Lab.Prelude
open import Lab.Rendering.Program as Prog using (ShaderDescr; ProgramDescr)
open import Data.Product using (_,_)

import Data.Vec.Base as Vec
import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Shaders as GL

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

-- record ProgramTextured2D : Set where
--     field
--         program    : GL.Program
--         locSampler : GL.UniformLocation

--     bind : IO ⊤
--     bind = unliftℓ <$> (GL.currentProgram $= Just program)

-- unbind : IO ⊤
-- unbind = unliftℓ <$> (GL.currentProgram $= Nothing)

-- load : IO ImageProgram
-- load = do
--     program ← GL.createProgram

--     frag ← GL.createShader GL.FragmentShader
--     fragSrc ← BS.readFile "image.frag"
--     GL.shaderSourceBS frag $= fragSrc
--     GL.compileShader frag
--     GL.attachShader program frag
--     -- fragLog ← ("FRAG: " ++_) <$> GL.shaderInfoLog frag

--     vert ← GL.createShader GL.VertexShader
--     vertSrc ← BS.readFile "image.vert"
--     GL.shaderSourceBS vert $= vertSrc
--     GL.compileShader vert
--     GL.attachShader program vert
--     -- vertLog ← ("VERT: " ++_) <$> GL.shaderInfoLog vert

--     GL.linkProgram program
--     linkOk ← get $ GL.linkStatus program
--     unless linkOk $ do
--         progLog ← ("PROG: " ++_) <$> GL.programInfoLog program
--         fail $ "Shader program link failed: " ++ progLog

--     locSampler ← GL.uniformLocation program "uImageTexture"

--     return record
--         { program = program
--         ; locSampler = locSampler
--         }
