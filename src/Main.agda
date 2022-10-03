{-# OPTIONS --without-K --warn=noUserWarning #-}

module Main where

open import Ffi.Hs.Prelude
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

open import Ffi.Hs.Control.Monad.Trans.State

import Ffi.Hs.Codec.Picture       as JP
import Ffi.Hs.Codec.Picture.Types as JP

open import Ffi.Hs.Data.StateVar as SV using (_$=_)
open import Ffi.Hs.Data.IORef as IORef using (IORef; newIORef; readIORef)

open import Ffi.Hs.Debug.Trace using (trace)

instance
    _ = SDL.Eq[EventPayload]
    _ = SDL.HasSetter[Hint[V],V]
    _ = SDL.Functor[V2]

    _ = SV.HasGetter[IORef[A],A]
    _ = SV.HasSetter[IORef[A],A]
    _ = SV.HasSetter[StateVar[A],A]

    _ = Functor[StateT[S,M]]
    _ = Applicative[StateT[S,M]]
    _ = Monad[StateT[S,M]]
    _ = MonadIO[StateT[S,M]]

    _ = FC.Enum[CInt]
    _ = FC.Num[CInt]
    _ = Word.Num[Word8]

    _ = JP.Pixel[PixelRGBA8]
    _ = JP.Pixel[A]⇒Storable[PixelBaseComponent[A]]

fromℕ : ∀{A : Set} → ⦃ Num A ⦄ → Nat → A
fromℕ = fromInteger ∘ ℤ.pos

Image : Set
Image = JP.Image JP.PixelRGBA8

record ImageBox : Set
record Env : Set

record Env where
    constructor mkEnv
    field
        window   : SDL.Window
        renderer : SDL.Renderer
        filePath : IORef Text
        scale    : IORef Float
        srcIB    : ImageBox
        dstIB    : ImageBox

        info : String

record ImageBox where
    field
        content : Maybe (Tuple2 Image SDL.Texture)
        vpPos : SDL.V2 CInt

    width : Int
    width = maybe (fromIntegral $ ℤ.pos 0) (JP.Image.imageWidth ∘ fst) content

    height : Int
    height = maybe (fromIntegral $ ℤ.pos 0) (JP.Image.imageHeight ∘ fst) content

    size : SDL.V2 Int
    size = SDL.mkV2 width height

    vpRect : SDL.Rectangle CInt
    vpRect = SDL.mkRectangle (SDL.P vpPos) (toEnum <$> size)

    unload : IO {lzero} ⊤′
    unload = maybe (pure _) (λ (mkTuple2 _ t) → SDL.destroyTexture t) content

    load : Env → Image → IO ImageBox
    load env img = let module env = Env env in do
        unload
        let size = SDL.mkV2 (JP.Image.imageWidth img) (JP.Image.imageHeight img)
        let length = SDL.V2.x size * SDL.V2.y size * (fromℕ 4)
        texture ← unliftℓ <$> SDL.createTexture env.renderer SDL.RGBA8888 SDL.TextureAccessStatic (toEnum <$> size)
        let fptr = subst ForeignPtr JP.PixelBaseComponent[PixelRGBA8] $ fst $ Vector.unsafeToForeignPtr0 $ JP.Image.imageData img
        SDL.updateTexture texture Nothing (BS.fromForeignPtr0 fptr length) (toEnum length)
        return $ record { content = Just $ mkTuple2 img texture ; vpPos = vpPos }

    loadFile : Env → String → IO (Either String ImageBox)
    loadFile env path = do
        eimg ← JP.readImage path
        either (pure ∘ Left) (λ img → Right <$> load env (JP.convertRGBA8 img)) eimg

    render : Env → IO {lzero} ⊤′
    render env = let module env = Env env in do
        maybe (return _) (λ (mkTuple2 _ t) → SDL.copy env.renderer t Nothing (Just vpRect)) content


disposeEnv : Env → IO ⊤
disposeEnv env = let module env = Env env in do
    ImageBox.unload env.dstIB
    ImageBox.unload env.srcIB
    SDL.destroyRenderer env.renderer
    pure _


{-# NON_TERMINATING #-}
loop : StateT Env IO ⊤′
loop = unlessQuit do
    env2 ← get
    let module env2 = Env env2

    -- # ImGui frame start
    ImGui.openGL3NewFrame
    ImGui.sdl2NewFrame
    ImGui.newFrame

    -- # SDL Renderer
    SDL.rendererDrawColor env2.renderer $= SDL.mkV4 (fromℕ 0) (fromℕ 0) (fromℕ 0) (fromℕ 255)
    SDL.clear env2.renderer
    SDL.rendererDrawColor env2.renderer $= SDL.mkV4 (fromℕ 55) (fromℕ 200) (fromℕ 255) (fromℕ 255)
    SDL.drawLine env2.renderer (SDL.P (SDL.mkV2 (fromℕ 100) (fromℕ 100))) (SDL.P (SDL.mkV2 (fromℕ 300) (fromℕ 400)))
    liftIO $ ImageBox.render env2.srcIB env2
    liftIO $ ImageBox.render env2.dstIB env2
    SDL.present env2.renderer

    -- # ImGui u
    ImGui.begin "File"
    ImGui.inputText "Path" env2.filePath (fromℕ 255)
    ImGui.button "Load" >>= λ (liftℓ btnClicked) → when btnClicked $ do
        env ← get
        path ← liftIO $ readIORef (Env.filePath env)
        (Right srcIB') ← liftIO $ ImageBox.loadFile (Env.srcIB env) env (Text.unpack path)
            where (Left err) → do
                put (record env { info = Text.unpack "Image reading error" ++ err })
                ImGui.openPopup "InfoPopup"
        put (record env { srcIB = srcIB' })
        pure _

    env3 ← get
    let module env3 = Env env3

    ImGui.beginPopupModal "InfoPopup" >>= λ (liftℓ flag) → when flag $ do
        ImGui.text $ Text.pack env3.info
        ImGui.button "Ok" >>= λ (liftℓ okClicked) → when okClicked ImGui.closeCurrentPopup
        ImGui.endPopup

    ImGui.button "Save"
    ImGui.end

    -- # End frame
    ImGui.render
    ImGui.openGL3RenderDrawData =<< unliftℓ <$> ImGui.getDrawData
    SDL.glSwapWindow env3.window

    liftIO $ threadDelay (fromℕ 30)
    loop

    where
    unlessQuit : StateT Env IO ⊤′ → StateT Env IO ⊤′
    unlessQuit act = do
        events ← liftIO $ unliftℓ <$> ImGui.pollEventsWithImGui
        let quit = any ⦃ inst:Foldable[List] ⦄ ((SDL.QuitEvent ==_) ∘ SDL.Event.eventPayload) events
        unless quit act

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


    (mkEnv window
        <$> (unliftℓ <$> SDL.createRenderer window (toEnum $ fromInteger $ ℤ.negsuc 0) SDL.defaultRenderer)
        <*> newIORef ""
        <*> newIORef (realToFrac ⦃ inst:Real[Double] ⦄ ⦃ inst:Fractional[Float] ⦄ 1.0)
        <*> pure (record { content = Nothing ; vpPos = SDL.mkV2 (toEnum $ fromℕ 50) (fromℕ 50) })
        <*> pure (record { content = Nothing ; vpPos = SDL.mkV2 (toEnum $ fromℕ 450) (fromℕ 50) })
        <*> {- info -} (pure $ Text.unpack "")
        ) >>= execStateT loop >>= disposeEnv

    SDL.destroyWindow window
    ImGui.openGL3Shutdown
    ImGui.sdl2Shutdown
    ImGui.destroyContext imguiContext
    SDL.glDeleteContext glContext
    SDL.quit
    return _
 