module Main where

open import Ffi.Hs.Prelude
open import Lab.Util
open import Ffi.Hs.Control.Monad.IO.Class using (liftIO)
open import Ffi.Hs.Control.Applicative using (when; unless)
open import Ffi.Hs.Data.Foldable       using (any; mapM-)
open import Ffi.Hs.Data.Text as Text using (Text)
open import Agda.Builtin.Int as ℤ using ()
open import Agda.Primitive using (lzero)
open import Ffi.Hs.Foreign.C.Types as FC using (CInt)
open import Ffi.Hs.Foreign.ForeignPtr using (ForeignPtr)
open import Relation.Binary.PropositionalEquality using (subst)
open import Agda.Builtin.Nat using (Nat)
open import Ffi.Hs.Data.Word as Word using ()
open import Ffi.Hs.Control.Concurrent using (threadDelay)

open import Lab.ImageBox using (ImageBox; withImageBoxRef)

import Ffi.Hs.Data.ByteString as BS
import Ffi.Hs.Data.ByteString.Internal as BS

import Ffi.Hs.Data.Vector.Storable as Vector

import Ffi.Hs.SDL.Hint           as SDL
import Ffi.Hs.SDL.Vect           as SDL
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

import Ffi.Hs.Codec.Picture       as JP
import Ffi.Hs.Codec.Picture.Types as JP

open import Ffi.Hs.Data.StateVar as SV using (_$=_; get)
open import Ffi.Hs.Data.IORef as IORef using (IORef; newIORef)

open import Ffi.Hs.Debug.Trace using (trace)

import Lab.NearestNeighbor
import Lab.Bilinear
import Lab.CrucianCarp

instance
    _ = SDL.Eq[EventPayload]
    _ = SDL.HasSetter[Hint[V],V]
    _ = SDL.Functor[V2]

    _ = SV.HasGetter[IORef[A],A]
    _ = SV.HasSetter[IORef[A],A]
    _ = SV.HasSetter[StateVar[A],A]

    _ = FC.Enum[CInt]
    _ = FC.Num[CInt]
    _ = Word.Num[Word8]

data Alg : Set where
    NearestNeighbor : Alg
    CrucianCarp     : Alg
    Bilinear        : Alg

-- _alg==_ : Alg → Alg → Bool
-- NearestNeighbor alg== NearestNeighbor = True
-- NearestNeighbor alg== CrucianCarp     = False
-- NearestNeighbor alg== Bilinear        = False
-- CrucianCarp     alg== NearestNeighbor = False
-- CrucianCarp     alg== CrucianCarp     = True
-- CrucianCarp     alg== Bilinear        = False
-- Bilinear        alg== NearestNeighbor = False
-- Bilinear        alg== CrucianCarp     = False
-- Bilinear        alg== Bilinear        = True

showAlg : Alg → Text
showAlg NearestNeighbor = "Nearest Neighbor"
showAlg CrucianCarp     = "Crucian Carp"
showAlg Bilinear        = "Bilinear"

alg : Alg → SDL.V2 Float → Image → IO Image
alg NearestNeighbor s i = pure $ Lab.NearestNeighbor.scale s i
alg CrucianCarp     s i = Lab.CrucianCarp.scale s i
alg Bilinear        s i = pure $ Lab.Bilinear.scale s i

record Env : Set where
    field
        window   : SDL.Window
        renderer : SDL.Renderer
        filePath : IORef Text
        scaleX   : IORef Float
        scaleY   : IORef Float
        srcIB    : IORef ImageBox
        dstIB    : IORef ImageBox
        info     : IORef String
        curAlg   : IORef Alg

selectableAlgGui : Env → Alg → IO ⊤′
selectableAlgGui env a = do
    selected ← unliftℓ <$> ImGui.selectable (showAlg a)
    when selected do
        Env.curAlg env $= a

{-# NON_TERMINATING #-}
loop : Env → IO ⊤′
loop env = unlessQuit do

    -- # ImGui frame start
    ImGui.openGL3NewFrame
    ImGui.sdl2NewFrame
    ImGui.newFrame

    -- # SDL Renderer
    SDL.rendererDrawColor env.renderer $= SDL.mkV4 (fromℕ 0) (fromℕ 0) (fromℕ 0) (fromℕ 255)
    SDL.clear env.renderer
    SDL.rendererDrawColor env.renderer $= SDL.mkV4 (fromℕ 55) (fromℕ 200) (fromℕ 255) (fromℕ 255)
    SDL.drawLine env.renderer (SDL.P (SDL.mkV2 (fromℕ 100) (fromℕ 100))) (SDL.P (SDL.mkV2 (fromℕ 300) (fromℕ 400)))
    srcIB ← get env.srcIB
    dstIB ← get env.dstIB
    ImageBox.render srcIB env.renderer
    ImageBox.render dstIB env.renderer
    SDL.present env.renderer

    -- # ImGui u
    ImGui.begin "File"
    ImGui.inputText "Path" env.filePath (fromℕ 255)
    ImGui.button "Load" >>= λ (liftℓ btnClicked) → when btnClicked $ do
        path ← get env.filePath
        srcIB ← get env.srcIB
        (Right srcIB') ← ImageBox.loadFile srcIB env.renderer (Text.unpack path)
            where (Left err) → do
                env.info $= Text.unpack "Image reading error" ++ err
                ImGui.openPopup "InfoPopup"
        env.srcIB $= srcIB'
        pure _

    ImGui.button "Save"

    ImGui.dragFloat "X coeff" env.scaleX (doubleToFloat 0.05) (doubleToFloat 0.1) (doubleToFloat 16.0)
    ImGui.dragFloat "Y coeff" env.scaleY (doubleToFloat 0.05) (doubleToFloat 0.1) (doubleToFloat 16.0)
    ImGui.button "Scale" >>= λ (liftℓ clicked) → when clicked $ do
        srcIB ← get env.srcIB
        maybe
            (env.info $= Text.unpack "No source image" >> ImGui.openPopup "InfoPopup")
            (λ (mkTuple2 srcImg _) → do
                curAlg ← get env.curAlg
                scaleX ← get env.scaleX
                scaleY ← get env.scaleY
                dstIB  ← get env.dstIB
                dstImg ← alg curAlg (SDL.mkV2 scaleX scaleY) srcImg
                dstIB' ← ImageBox.load dstIB env.renderer dstImg
                env.dstIB $= dstIB'
                )
            (ImageBox.content srcIB)

    get env.curAlg >>= λ curAlg → do
        combo ← unliftℓ <$> ImGui.beginCombo "Algorithm" (showAlg curAlg)
        when combo $ do
            selectableAlgGui env NearestNeighbor
            selectableAlgGui env CrucianCarp
            selectableAlgGui env Bilinear
            ImGui.endCombo

    ImGui.beginPopupModal "InfoPopup" >>= λ (liftℓ flag) → when flag $ do
        ImGui.text ∘ Text.pack =<< get env.info
        unliftℓ <$> ImGui.button "Ok" >>= flip when ImGui.closeCurrentPopup
        ImGui.endPopup
    ImGui.end

    -- # End frame
    ImGui.render
    ImGui.openGL3RenderDrawData =<< unliftℓ <$> ImGui.getDrawData
    SDL.glSwapWindow env.window

    threadDelay (fromℕ 30)
    loop env

    where
    module env = Env env

    unlessQuit : IO ⊤′ → IO ⊤′
    unlessQuit act = do
        events ← unliftℓ <$> ImGui.pollEventsWithImGui
        let quit = any ⦃ inst:Foldable[List] ⦄ ((SDL.QuitEvent ==_) ∘ SDL.Event.eventPayload) events
        unless quit act

withRenderer : SDL.Window → (SDL.Renderer → IO ⊤) → IO ⊤
withRenderer window f = do
    renderer ← unliftℓ <$> SDL.createRenderer
        window (toEnum $ fromInteger $ ℤ.negsuc 0) SDL.defaultRenderer
    f renderer
    SDL.destroyRenderer renderer
    pure _

main : IO ⊤
main = do
    SDL.initializeAll
    SDL.HintRenderDriver $= SDL.OpenGL
    window ← unliftℓ <$> SDL.createWindow "Agda SDL2 example" (record SDL.defaultWindow
        { windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
        ; windowResizable       = False
        ; windowInitialSize     = SDL.mkV2 (fromℕ 900) (fromℕ 500)
        })
    glContext    ← unliftℓ <$> SDL.glCreateContext window
    imguiContext ← unliftℓ <$> ImGui.createContext
    imguiSdl     ← unliftℓ <$> ImGui.sdl2InitForOpenGL window glContext
    imguiGl      ← unliftℓ <$> ImGui.openGL3Init

    withRenderer window λ renderer → do
        withImageBoxRef (SDL.mkV2 (toEnum $ fromℕ 50) (fromℕ 50)) $ λ srcIBr → do
            withImageBoxRef (SDL.mkV2 (toEnum $ fromℕ 450) (fromℕ 50)) $ λ dstIBr → do
                info     ← newIORef (Text.unpack "")
                curAlg      ← newIORef NearestNeighbor
                filePath ← newIORef ""
                scaleX   ← newIORef $ doubleToFloat 1.0
                scaleY   ← newIORef $ doubleToFloat 1.0
                loop $ record
                    { window   = window
                    ; renderer = renderer
                    ; filePath = filePath
                    ; scaleX   = scaleX
                    ; scaleY   = scaleY
                    ; srcIB    = srcIBr
                    ; dstIB    = dstIBr
                    ; info     = info
                    ; curAlg   = curAlg
                    }
                pure _

    SDL.destroyWindow window
    ImGui.openGL3Shutdown
    ImGui.sdl2Shutdown
    ImGui.destroyContext imguiContext
    SDL.glDeleteContext glContext
    SDL.quit
    return _
 