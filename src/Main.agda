{-# OPTIONS --without-K #-}

module Main where

open import Data.Product                          using (_,_)
open import Relation.Binary.PropositionalEquality using (subst)

open import Ffi.Hs.Control.Applicative        using (when; unless)
open import Ffi.Hs.Control.Concurrent         using (threadDelay)
open import Ffi.Hs.Control.Monad.IO.Class     using (liftIO)
open import Ffi.Hs.Control.Monad.Trans.Except using (runExceptT)
open import Ffi.Hs.Control.Monad.Trans.Reader using (runReaderT)
open import Ffi.Hs.Data.Foldable              using (any; mapM-; forM-)
open import Ffi.Hs.Foreign.C.Types            using (CInt)
open import Ffi.Hs.Foreign.ForeignPtr         using (ForeignPtr)

open import Lab.Algorithm                                using (Algorithm)
open import Lab.Class.Level
open import Lab.Environment                              using (Env)
open import Lab.ImageBox                     as ImageBox using (ImageBox)
open import Lab.Input                                    using (Input)
open import Lab.Params                                   using (windowWidth; windowHeight)
open import Lab.Prelude
open import Lab.Rendering.Mesh.Quad          as Quad     using (Quad)
open import Lab.Rendering.Program            as Program  using (Program)
open import Lab.Rendering.Program.Textured2D             using (textured2d)

import Lab.Algorithm.NearestNeighbor as Algorithm
import Lab.Algorithm.Bilinear        as Algorithm
import Lab.Algorithm.CrucianCarp     as Algorithm

import Ffi.Hs.Data.ByteString           as BS
import Ffi.Hs.Data.ByteString.Internal  as BS
import Ffi.Hs.Data.Vector.Storable      as Vector
import Ffi.Hs.Graphics.Rendering.OpenGL as GL

import Ffi.Hs.DearImGui            as ImGui
import Ffi.Hs.DearImGui.SDL        as ImGui
import Ffi.Hs.DearImGui.SDL.OpenGL as ImGui
import Ffi.Hs.DearImGui.OpenGL3    as ImGui


algorithms : List Algorithm
algorithms =
    Algorithm.nearest-neighbor ∷
    Algorithm.bilinear         ∷
    Algorithm.crucian-carp     ∷
    []

selectableAlgGui : Env → Algorithm → IO {1ℓ} ⊤′
selectableAlgGui env a = do
    liftℓ selected ← ImGui.selectable (Text.append (Algorithm.name a) "\0")
    when selected do
        liftℓ inp ← liftℓ1 $ Input.new (Algorithm.input a)
        Env.algorithm env $= a , inp

{-# NON_TERMINATING #-}
loop : Env → IO {sucℓ 0ℓ} ⊤′
loop env = unlessQuit $ do
    ImGui.openGL3NewFrame
    ImGui.sdl2NewFrame
    ImGui.newFrame

    ImGui.begin "File\0"
    ImGui.inputText "Path\0" env.imageFilePathUi 255
    ImGui.button "Load\0" >>= λ (liftℓ btnClicked) → when btnClicked $ do
        liftℓ path ← liftℓ1 $ get env.imageFilePathUi
        liftℓ (Right _) ← liftℓ1 $ runExceptT $ ImageBox.loadFile env.srcIB (Text.unpack path)
            where liftℓ (Left err) → do
                env.infoUi $= Text.unpack "Image reading error" ++ err
                ImGui.openPopup "InfoPopup"
        pure _

    ImGui.button "Save\0" >>= λ (liftℓ btnClicked) → when btnClicked $ do
        liftℓ path ← liftℓ1 $ get env.imageFilePathUi
        liftℓ (Just (mkTuple2 dstImg _)) ← liftℓ1 $ readIORef $ ImageBox.content env.dstIB
            where liftℓ Nothing → do
                env.infoUi $= Text.unpack "Nothing to save"
                ImGui.openPopup "InfoPopup"
        liftℓ1 $ JP.writePng (Text.unpack path) dstImg

    liftℓ1 $ ImGui.button "Run\0" >>= λ (liftℓ clicked) → when clicked $ do
        Just (mkTuple2 srcImg _) ← readIORef $ ImageBox.content env.srcIB
            where Nothing → do
                env.infoUi $= Text.unpack "No source image"
                ImGui.openPopup "InfoPopup"
        dstImg ← readIORef env.algorithm >>=ℓ λ (alg , inpSt) → do
            inpVal ← Input.load (Algorithm.input alg) inpSt
            Algorithm.run alg inpVal srcImg
        ImageBox.load env.dstIB dstImg

    get env.algorithm >>= λ (alg , inpSt) → do
        (liftℓ combo) ← ImGui.beginCombo "Algorithm\0" (Text.append (Algorithm.name alg) "\0")
        when combo $ do
            forM- algorithms (selectableAlgGui env)
            ImGui.endCombo
        liftℓ1 $ Input.ui (Algorithm.input alg) inpSt

    -- num ← newIORef (fromℕ 0)
    -- ImGui.listBox "Log\0" num [] >>= λ (liftℓ b) → when b $ do
    --     pure _

    liftℓ1 do
        errors ← get GL.errors
        forM- errors λ{ (GL.mkError c s) → do
                putStrLn s
                env.infoUi $= "GL error: " ++ s
                ImGui.openPopup "InfoPopup"
            }

    ImGui.beginPopupModal "InfoPopup" >>= λ (liftℓ flag) → when flag $ do
        get env.infoUi >>=ℓ ImGui.text ∘ Text.pack
        unliftℓ <$> ImGui.button "Ok\0" >>=ℓ flip when ImGui.closeCurrentPopup
        ImGui.endPopup
    ImGui.end

    GL.clearColor $= GL.mkColor4 (realToFrac 0.0) (realToFrac 0.3) (realToFrac 0.5) (realToFrac 1.0)
    liftℓ1 {bℓ = 1ℓ} $ GL.clear (GL.ColorBuffer ∷ GL.DepthBuffer ∷ [])

    runReaderT (ImageBox.render env.srcIB) env
    runReaderT (ImageBox.render env.dstIB) env

    ImGui.render
    ImGui.openGL3RenderDrawData =<<ℓ unliftℓ <$> ImGui.getDrawData
    SDL.glSwapWindow env.window

    liftℓ1 $ threadDelay 30
    loop env

    where
    module env = Env env 

    unlessQuit : IO {1ℓ} ⊤′ → IO {1ℓ} ⊤′
    unlessQuit act = do
        liftℓ events ← ImGui.pollEventsWithImGui
        let quit = any ⦃ inst:Foldable[List] ⦄
                ((SDL.QuitEvent ==_) ∘ SDL.Event.eventPayload) events
        unless quit act

main : IO ⊤
main = do
    SDL.initializeAll
    liftℓ window ← SDL.createWindow "Lab 5-gfx-1" (record SDL.defaultWindow
        { windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
        ; windowResizable       = False
        ; windowInitialSize     = SDL.mkV2 (fromIntegral windowWidth) (fromIntegral windowHeight)
        })
    liftℓ glContext    ← SDL.glCreateContext window
    liftℓ imguiContext ← ImGui.createContext
    liftℓ imguiSdl     ← ImGui.sdl2InitForOpenGL window glContext
    liftℓ imguiGl      ← ImGui.openGL3Init

    GL.texture GL.Texture2D $= GL.Enabled -- ? legacy
    do
        inputSt         ← Input.new (Algorithm.input Algorithm.nearest-neighbor)
        imageFilePathUi ← newIORef ""
        infoUi          ← newIORef ""
        mesh-quad       ← Quad.new
        prog-textured2d ← Program.new textured2d
        srcIB           ← ImageBox.new (GL.mkVector2 (doubleToFloat -0.5) (doubleToFloat 0.0)) (GL.mkVector2 0.421 0.8)
        dstIB           ← ImageBox.new (GL.mkVector2 (doubleToFloat 0.452) (doubleToFloat 0.0)) (GL.mkVector2 0.421 0.8)
        unliftℓ1 $
            newIORef (Algorithm.nearest-neighbor , inputSt) >>= λ algorithm →
                loop record
                    { mesh-quad       = mesh-quad
                    ; prog-textured2d = prog-textured2d
                    ; window          = window
                    ; imageFilePathUi = imageFilePathUi
                    ; algorithm       = algorithm
                    ; srcIB           = srcIB
                    ; dstIB           = dstIB
                    ; infoUi          = infoUi
                    }

    SDL.destroyWindow window
    ImGui.openGL3Shutdown
    ImGui.sdl2Shutdown
    ImGui.destroyContext imguiContext
    SDL.glDeleteContext glContext
    SDL.quit
    return _
