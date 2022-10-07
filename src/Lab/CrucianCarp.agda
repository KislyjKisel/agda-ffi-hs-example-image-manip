module Lab.CrucianCarp where

open import Ffi.Hs.SDL.Vect as SDL using (V2; mkV2)
open import Lab.Util
open import Ffi.Hs.Prelude
open import Ffi.Hs.Linear.Vector using (_*^_; _^+^_; _^-^_; _^/_)
open import Ffi.Hs.Control.Monad.Primitive using ()
open import Ffi.Hs.Control.Applicative using (when)
open import Ffi.Hs.Data.Foldable using (forM-)

import Ffi.Hs.Codec.Picture       as JP
import Ffi.Hs.Codec.Picture.Types as JP

scale : V2 Float → Image → IO Image
scale (mkV2 scaleX scaleY) src = do
    dst ← grid
    when (scaleX > doubleToFloat 1.0) $ fillRows    dst
    when (scaleY > doubleToFloat 1.0) $ fillColumns dst
    JP.unsafeFreezeImage dst
    where
    srcW = JP.imageWidth src
    srcH = JP.imageHeight src
    dstW = floor $ scaleX * (fromIntegral srcW)
    dstH = floor $ scaleY * (fromIntegral srcH)

    grid : IO MImage
    grid = do
        dst ← JP.newMutableImage dstW dstH
        JP.pixelFoldM (const $ aux dst) tt′ src
        pure dst
        where
        aux : MImage → Int → Int → Pixel → IO ⊤′
        aux dst x y c = JP.writePixel dst (floor $ realToFrac x * scaleX) (floor $ realToFrac y * scaleY) c

    fillRows : MImage → IO ⊤′
    fillRows dst = do
        JP.pixelFoldM (λ _ x y _ → aux x y) tt′ src
        where
        aux : Int → Int → IO ⊤′
        aux srcX srcY = when (srcX < (srcW - fromℕ 1)) $ do
            let dstX0 = floor (realToFrac srcX * scaleX)
                dstX1 = floor (realToFrac (srcX + fromℕ 1) * scaleX)
                dstY  = floor (realToFrac srcY * scaleY)
                gap   = dstX1 - dstX0 - fromℕ 1
                col0  = pixelToV4f $ JP.pixelAt src srcX srcY
                col1  = pixelToV4f $ JP.pixelAt src (srcX + fromℕ 1) srcY
                delta = (col1 ^-^ col0) ^/ realToFrac (gap + fromℕ 1)
            forM- [ fromℕ 1 ⋯ gap ] (λ Δx → JP.writePixel dst (dstX0 + Δx) dstY (v4fToPixel $ col0 ^+^ (realToFrac Δx) *^ delta))

    fillColumns : MImage → IO ⊤′
    fillColumns dst = forM- [ fromℕ 0 ⋯ dstW - fromℕ 1 ] $
        λ dstX → forM- [ fromℕ 0 ⋯ srcH - fromℕ 2 ] $
            λ srcY → do
                let dstY0 = floor $ realToFrac srcY * scaleY
                    dstY1 = floor $ realToFrac (srcY + fromℕ 1) * scaleY
                    gap   = dstY1 - dstY0 - fromℕ 1

                col0 ← pixelToV4f <$> JP.readPixel dst dstX dstY0
                col1 ← pixelToV4f <$> JP.readPixel dst dstX dstY1

                let delta = (col1 ^-^ col0) ^/ realToFrac (gap + fromℕ 1)
                forM- [ fromℕ 1 ⋯ gap ] (λ Δy → JP.writePixel dst dstX (dstY0 + Δy) (v4fToPixel $ col0 ^+^ (realToFrac Δy) *^ delta))
