{-# OPTIONS --without-K #-}

module Lab.Algorithm.NearestNeighbor where

open import Lab.Prelude
open import Lab.Algorithm using (Algorithm)

import Lab.Input.Scale2D as Inp


scale : Tuple2 Float Float → Image → IO Image
scale (mkTuple2 scaleX scaleY) src = pure $
    JP.generateImage generator dstW dstH
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

nearest-neighbor : Algorithm
nearest-neighbor = record
    { name  = "Nearest Neighbor"
    ; input = Inp.scale2d
    ; run   = scale
    }
