module Waves where

import           Notes
import           Numeric                        ( showFFloat )

data Point = Point Float Float

sampleRate = 48000 :: Int
step = 1 / fromIntegral sampleRate :: Float

mapPoint :: (Float -> a) -> Point -> (a, a)
mapPoint f (Point x y) = (f x, f y)

showFullPrecision :: Float -> String
showFullPrecision x = showFFloat Nothing x ""

sinWave :: Anchor -> Note -> [Float]
sinWave anchor note =
  let frequency = freq anchor note
      xs        = [0, step ..]
  in  map (sin . (* (2 * pi * frequency))) xs

sinWave' :: Note -> [Float]
sinWave' = sinWave defAnchor

test :: Note -> [(String, String)]
test note =
  map (mapPoint showFullPrecision) $ zipWith Point [0, step ..] $ sinWave defAnchor note

volume :: Float -> [Float] -> [Float]
volume vol = map (* (vol / 100))

duration :: Float -> [Float] -> [Float]
duration = take . round . (* 48000.0)
