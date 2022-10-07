module Lab.Bilinear where

open import Ffi.Hs.SDL.Vect as SDL using (V2; mkV2)
open import Lab.Util
open import Ffi.Hs.Prelude
open import Ffi.Hs.Linear.Vector using (_*^_; _^+^_; _^-^_)

import Ffi.Hs.Codec.Picture as JP

scale : V2 Float → Image → Image
scale (mkV2 scaleX scaleY) src = JP.generateImage generator dstW dstH
    where
    srcW = JP.imageWidth src
    srcH = JP.imageHeight src
    dstW = floor $ scaleX * (fromIntegral srcW)
    dstH = floor $ scaleY * (fromIntegral srcH)

    lerp : SDL.V4 Float → SDL.V4 Float → Float → SDL.V4 Float
    lerp v1 v2 k = v1 ^+^ k *^ (v2 ^-^ v1)

    generator : Int → Int → Pixel
    generator dstX dstY = do
        let srcX = fromIntegral dstX / (realToFrac $ dstW - fromℕ 1) * (realToFrac $ srcW - fromℕ 1)
            srcY = fromIntegral dstY / (realToFrac $ dstH - fromℕ 1) * (realToFrac $ srcH - fromℕ 1)
            src0X = floor srcX
            src0Y = floor srcY
            src1X = src0X + fromℕ 1
            src1Y = src0Y + fromℕ 1

            rightEdge  = src1X == srcW
            bottomEdge = src1Y == srcH

        case mkTuple2 rightEdge bottomEdge of λ
            { (mkTuple2 False False) → do
                let w00 = (realToFrac src1X - srcX) * (realToFrac src1Y - srcY)
                    w01 = (realToFrac src1X - srcX) * (srcY - realToFrac src0Y)
                    w10 = (srcX - realToFrac src0X) * (realToFrac src1Y - srcY)
                    w11 = (srcX - realToFrac src0X) * (srcY - realToFrac src0Y)
                    q00 = pixelToV4f $ JP.pixelAt src src0X src0Y
                    q01 = pixelToV4f $ JP.pixelAt src src0X src1Y
                    q10 = pixelToV4f $ JP.pixelAt src src1X src0Y
                    q11 = pixelToV4f $ JP.pixelAt src src1X src1Y

                v4fToPixel $ w00 *^ q00 ^+^ w01 *^ q01 ^+^ w10 *^ q10 ^+^ w11 *^ q11

            ; (mkTuple2 True True)  → JP.pixelAt src src0X src0Y

            ; (mkTuple2 False True) → do
                let q0 = pixelToV4f $ JP.pixelAt src src0X src0Y
                    q1 = pixelToV4f $ JP.pixelAt src src1X src0Y

                v4fToPixel $
                    lerp q0 q1 $ (srcX - realToFrac src0X) / (realToFrac $ src1X - src0X)

            ; (mkTuple2 True Flase) →  do
                let q0 = pixelToV4f $ JP.pixelAt src src0X src0Y
                    q1 = pixelToV4f $ JP.pixelAt src src0X src1Y

                v4fToPixel $
                    lerp q0 q1 $ (srcY - realToFrac src0Y) / (realToFrac $ src1Y - src0Y)

            }
