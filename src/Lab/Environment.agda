{-# OPTIONS --without-K #-}

module Lab.Environment where

open import Lab.Prelude
open import Lab.Class.Product using (Product)
open import Lab.Rendering.Mesh.Quad using (Quad)
open import Lab.Rendering.Program using (Program)
open import Lab.Rendering.Program.Textured2D using (textured2d)
open import Data.Product using (Σ)
open import Lab.Input using (Input)
open import Lab.Algorithm using (Algorithm)
open import Lab.ImageBox using (ImageBox)

record Env : Set₁ where
    field
        mesh-quad        : Quad
        prog-textured2d  : Program textured2d
        window           : SDL.Window
        imageFilePathUi  : IORef Text
        algorithm        : IORef (Σ Algorithm (Input.State ∘ Algorithm.input)) 
        srcIB            : ImageBox
        dstIB            : ImageBox
        infoUi           : IORef String

instance
    Product[Mesh,Env] : Product Quad Env
    Product[Mesh,Env] = record
        { extract = Env.mesh-quad
        ; update  = λ f e → record e { mesh-quad = f (Env.mesh-quad e) }
        }

    Product[Program[textured2d],Env] : Product (Program textured2d) Env
    Product[Program[textured2d],Env] = record
        { extract = Env.prog-textured2d
        ; update  = λ f e → record e { prog-textured2d = f (Env.prog-textured2d e) }
        }
