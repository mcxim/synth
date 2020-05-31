module Waves where

import           Notes
import           Numeric                        ( showFFloat )


bpm :: Float
bpm = 120.0

mapPoint :: (Float -> a) -> Point -> (a, a)
mapPoint f (Point x y) = (f x, f y)

sinWave :: Anchor -> Note -> [Float]
sinWave anchor note = map (sin . (* (2 * pi * freq anchor note))) [0, step ..]

sinWave' :: Note -> [Float]
sinWave' = sinWave defAnchor

volume :: Float -> [Float] -> [Float]
volume vol = map (* (vol / 100))

duration :: Float -> [Float] -> [Float]
duration = take . round . (* (48000.0 * 60.0 / bpm))

playTups :: [(Float, Note)] -> [Float]
playTups = concatMap (\(dur, note) -> duration dur $ sinWave defAnchor note)

sevenNationArmy = playTups
  [ (1.5 , Note A nat 2)
  , (0.5 , Note A nat 2)
  , (0.75, Note C nat 3)
  , (0.75, Note A nat 2)
  , (0.5 , Note G nat 2)
  , (2.0 , Note F nat 2)
  , (2.0 , Note E nat 2)
  , (1.5 , Note A nat 2)
  , (0.5 , Note A nat 2)
  , (0.75, Note C nat 3)
  , (0.75, Note A nat 2)
  , (0.5 , Note G nat 2)
  , (0.75, Note F nat 2)
  , (0.75, Note G nat 2)
  , (0.5 , Note F nat 2)
  , (2.0 , Note E nat 2)
  ]





data Point = Point Float Float

sampleRate = 48000 :: Int
step = 1 / fromIntegral sampleRate :: Float

showFullPrecision :: Float -> String
showFullPrecision x = showFFloat Nothing x ""

test :: Note -> [(String, String)]
test note =
  map (mapPoint showFullPrecision) $ zipWith Point [0, step ..] $ sinWave defAnchor note

