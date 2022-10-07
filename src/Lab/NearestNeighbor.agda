module Lab.NearestNeighbor where

open import Ffi.Hs.SDL.Vect using (V2; mkV2)
open import Lab.Util
open import Ffi.Hs.Prelude

import Ffi.Hs.Codec.Picture as JP

scale : V2 Float → Image → Image
scale (mkV2 scaleX scaleY) src = JP.generateImage generator dstW dstH
    where
    srcW = JP.imageWidth src
    srcH = JP.imageHeight src
    dstW = floor $ scaleX * (fromIntegral srcW)
    dstH = floor $ scaleY * (fromIntegral srcH)

    generator : Int → Int → Pixel
    generator dstX dstY =
        let srcX = floor $ fromIntegral dstX / scaleX
            srcY = floor $ fromIntegral dstY / scaleY
        in
            JP.pixelAt src srcX srcY
