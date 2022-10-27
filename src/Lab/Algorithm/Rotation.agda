{-# OPTIONS --without-K #-}

module Lab.Algorithm.Rotation where

open import Data.Product  using (_×_; _,_; proj₁; proj₂)
open import Lab.Algorithm using (Algorithm)
open import Lab.Prelude

import Lab.Input.Rotation as Inp

rotateV2 : Double → Double × Double → Double × Double
rotateV2 θ (x , y) = let
    x′ = realToFrac x * cos θ - realToFrac y * sin θ
    y′ = realToFrac x * sin θ + realToFrac y * cos θ
    in
    x′ , y′

-- rotateV2Around : Double → (o p : Double × Double) → Double × Double
-- rotateV2Around θ (x₀ , y₀) (x , y) with rotateV2 θ (x - x₀ , y - y₀)
-- ... | (x′ , y′) = x₀ + x′ , y₀ + y′

rotate : Int → Image → IO Image
rotate angleDeg src = pure $ JP.generateImage generator dstW dstH
    where
    angleRad  = realToFrac angleDeg / 180.0 * pi
    srcW      = JP.imageWidth src
    srcWf     = realToFrac srcW
    srcH      = JP.imageHeight src
    srcHf     = realToFrac srcH
    diagLen   = 0.5 * sqrt (srcWf * srcWf + srcHf * srcHf)
    diagAngle = atan2 srcHf srcWf
    dstWf     = 2.0 * diagLen * max (abs $ cos $ angleRad + diagAngle) (abs $ cos $ angleRad - diagAngle)
    dstW      = floor dstWf
    dstHf     = 2.0 * diagLen * max (abs $ sin $ angleRad + diagAngle) (abs $ sin $ angleRad - diagAngle)
    dstH      = floor dstHf
    dstX₀     = 0.5 * dstWf
    dstY₀     = 0.5 * dstHf
    srcX₀     = 0.5 * srcWf
    srcY₀     = 0.5 * srcHf

    generator : Int → Int → Pixel
    generator dstX dstY = if inside
        then JP.pixelAt src srcX srcY
        else JP.mkPixelRGBA8 0 0 0 255
        where
        dstXf = realToFrac dstX
        dstYf = realToFrac dstY
        srcOff = rotateV2 (negate angleRad) (dstXf - dstX₀ , dstYf - dstY₀)
        srcX = floor $ srcX₀ + proj₁ srcOff
        srcY = floor $ srcY₀ + proj₂ srcOff
        inside = srcX >= 0 && srcX < srcW && srcY >= 0 && srcY < srcH

rotation : Algorithm
rotation = record
    { name  = "Rotation"
    ; input = Inp.Rotation
    ; run   = rotate
    }
