{-# OPTIONS --without-K #-}

module Lab.Algorithm.CrucianCarp where

open import Ffi.Hs.Control.Applicative using (when)
open import Ffi.Hs.Data.Foldable       using (forM-)
open import Ffi.Hs.Linear.Vector       using (_*^_; _^+^_; _^-^_; _^/_)
open import Lab.Algorithm              using (Algorithm)
open import Lab.Prelude

import Lab.Input.Scale2D as Inp


scale : Tuple2 Float Float → Image → IO Image
scale (mkTuple2 scaleX scaleY) src = do
    dst ← grid
    when (scaleX > f64⇒f32 1.0) $ fillRows    dst
    when (scaleY > f64⇒f32 1.0) $ fillColumns dst
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
        aux dst x y c = JP.writePixel dst
            (floor $ realToFrac x * scaleX)
            (floor $ realToFrac y * scaleY)
            c

    fillRows : MImage → IO ⊤′
    fillRows dst = do
        JP.pixelFoldM (λ _ x y _ → aux x y) tt′ src
        where
        aux : Int → Int → IO ⊤′
        aux srcX srcY = when (srcX < (srcW - 1)) $ do
            let dstX0 = floor (realToFrac srcX * scaleX)
                dstX1 = floor (realToFrac (srcX + 1) * scaleX)
                dstY  = floor (realToFrac srcY * scaleY)
                gap   = dstX1 - dstX0 - 1
                col0  = pixelToV4f $ JP.pixelAt src srcX srcY
                col1  = pixelToV4f $ JP.pixelAt src (srcX + 1) srcY
                delta = (col1 ^-^ col0) ^/ realToFrac (gap + 1)
            forM- [ 1 ⋯ gap ] λ Δx →
                JP.writePixel dst (dstX0 + Δx) dstY $
                    v4fToPixel $ col0 ^+^ (realToFrac Δx) *^ delta

    fillColumns : MImage → IO ⊤′
    fillColumns dst = forM- [ 0 ⋯ dstW - 1 ] $
        λ dstX → forM- [ 0 ⋯ srcH - 2 ] $
            λ srcY → do
                let dstY0 = floor $ realToFrac srcY * scaleY
                    dstY1 = floor $ realToFrac (srcY + 1) * scaleY
                    gap   = dstY1 - dstY0 - 1

                col0 ← pixelToV4f <$> JP.readPixel dst dstX dstY0
                col1 ← pixelToV4f <$> JP.readPixel dst dstX dstY1

                let delta = (col1 ^-^ col0) ^/ realToFrac (gap + 1)
                forM- [ 1 ⋯ gap ] λ Δy →
                    JP.writePixel dst dstX (dstY0 + Δy) $
                        v4fToPixel $ col0 ^+^ (realToFrac Δy) *^ delta


crucian-carp : Algorithm
crucian-carp = record
    { name  = "Crucian Carp"
    ; input = Inp.Scale2D
    ; run   = scale
    }
