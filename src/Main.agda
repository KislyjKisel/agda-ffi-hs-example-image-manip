{-# OPTIONS --without-K #-}

module Main where

open import Ffi.Hs.-base.Class as Class using ()
open import Ffi.Hs.Data.Maybe as Maybe using (Maybe; Just; Nothing)
open import Agda.Primitive             using (lzero)
open import Ffi.Hs.-base.Level         using (liftℓ; unliftℓ)
open import Ffi.Hs.-base.Unit          using (⊤; ⊤′)
open import Ffi.Hs.Control.Applicative using (when; unless)
open import Ffi.Hs.Control.Monad       using (_>>=_; _>>_; _=<<_; return)
open import Ffi.Hs.Data.Eq             using (_==_)
open import Ffi.Hs.Data.Foldable       using (any; mapM-)
open import Ffi.Hs.Data.Function       using (_∘_; _$_)
open import Ffi.Hs.Data.Functor        using (_<$>_)
open import Ffi.Hs.Data.List as List   using (map)
open import Ffi.Hs.System.IO as IO     using (IO)
open import Ffi.Hs.Data.Text using (Text)
open import Agda.Builtin.Int as ℤ using ()
open import Ffi.Hs.GHC.Num as Num using ()
open import Ffi.Hs.GHC.Real as Real using ()
open import Ffi.Hs.Data.Int as Int using ()
open import Lab.Util using (if_then_else_)
open import Ffi.Hs.-base.Float using (Float; Double)

import Ffi.Hs.SDL.Init           as SDL
import Ffi.Hs.SDL.Video          as SDL
import Ffi.Hs.SDL.Video.Renderer as SDL
import Ffi.Hs.SDL.Video.OpenGL   as SDL
import Ffi.Hs.SDL.Event          as SDL
import Ffi.Hs.SDL.Internal.Types as SDL

import Ffi.Hs.DearImGui            as ImGui
import Ffi.Hs.DearImGui.SDL        as ImGui
import Ffi.Hs.DearImGui.SDL.OpenGL as ImGui
import Ffi.Hs.DearImGui.OpenGL3    as ImGui

import Ffi.Hs.Data.StateVar as SV
open import Ffi.Hs.Data.IORef as IORef using (IORef; newIORef)

{-# FOREIGN GHC import MAlonzo.Code.QZ45Zbase.Dictionaries #-}

instance
    _ = IO.Functor[IO]
    _ = IO.Monad[IO]
    _ = IO.MonadIO[IO]
    _ = IO.Applicative[IO]

    _ = SDL.Eq[EventPayload]
    _ = List.Foldable[List]

    _ = SV.HasGetter[IORef[A],A]
    _ = SV.HasSetter[IORef[A],A]

    _ = Int.Num[Int]

    _ = Maybe.Foldable[Maybe]

    postulate
        Real[Float] : Class.Real Float
        Fractional[Float] : Class.Fractional Float
        Real[Double] : Class.Real Double
        Fractional[Double] : Class.Fractional Double 

{-# COMPILE GHC Real[Float] = AgdaReal #-}
{-# COMPILE GHC Fractional[Float] = AgdaFractional #-}
{-# COMPILE GHC Real[Double] = AgdaReal #-}
{-# COMPILE GHC Fractional[Double] = AgdaFractional #-}

record Env : Set where
    field
        filePath : IORef Text
        window   : SDL.Window
        texture  : Maybe SDL.Texture
        scale    : IORef Float

{-# NON_TERMINATING #-}
loop : Env → IO Env
loop env = unlessQuit do
    ImGui.openGL3NewFrame
    ImGui.sdl2NewFrame
    ImGui.newFrame

    ImGui.begin "Stats"
    ImGui.end

    ImGui.begin "Options"
    ImGui.dragFloat "Scale" scale (Real.realToFrac 1.0) (Real.realToFrac 0.1) (Real.realToFrac 4.0)
    ImGui.end

    ImGui.begin "File"
    ImGui.inputText "Path" filePath (Num.fromInteger $ ℤ.pos 255)
    ImGui.button "Load" >>= λ (liftℓ btnClicked) → when btnClicked loadImageFile
    ImGui.button "Save"
    ImGui.end

    ImGui.render
    ImGui.openGL3RenderDrawData =<< unliftℓ <$> ImGui.getDrawData
    SDL.glSwapWindow window
    loop env

    where
    open Env env

    unlessQuit : IO Env → IO Env
    unlessQuit act = do
        events ← unliftℓ <$> ImGui.pollEventsWithImGui
        let quit = any ⦃ List.Foldable[List] ⦄ ((SDL.QuitEvent ==_) ∘ SDL.Event.eventPayload) events
        unless quit (act >> return _)
        return env

    loadImageFile : IO ⊤′
    loadImageFile = do
        mapM- SDL.destroyTexture texture

main : IO ⊤
main = do
    SDL.initializeAll
    window ← unliftℓ <$> SDL.createWindow "Agda SDL2 example" (record SDL.defaultWindow
        { windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
        })
    glContext ← unliftℓ <$> SDL.glCreateContext window
    imguiContext ← unliftℓ <$> ImGui.createContext
    imguiSdl ← unliftℓ <$> ImGui.sdl2InitForOpenGL window glContext
    imguiGl ← unliftℓ <$> ImGui.openGL3Init

    filePath ← newIORef ""
    scale ← newIORef (Real.realToFrac 1.0)
    env ← loop record
        { window = window
        ; filePath = filePath
        ; texture = Nothing
        ; scale = scale
        }
    
    let module env = Env env

    mapM- SDL.destroyTexture env.texture

    SDL.destroyWindow window
    ImGui.openGL3Shutdown
    ImGui.sdl2Shutdown
    ImGui.destroyContext imguiContext
    SDL.glDeleteContext glContext
    SDL.quit
    return _
