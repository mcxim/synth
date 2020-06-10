module Waves where

import           Notes
import           Numeric                        ( showFFloat )


bpm :: Float
bpm = 113.0
-- mystery: 113.0
-- hurricane: 82.0

sampleRate = 48000 :: Int
step = 1 / fromIntegral sampleRate :: Float

sinWave :: Anchor -> Note -> [Float]
sinWave anchor note = map (sin . (* (2 * pi * freq anchor note))) [0, step ..]

sinWave' :: Note -> [Float]
sinWave' = sinWave defAnchor

volume :: Float -> [Float] -> [Float]
volume vol = map (* (vol / 100))

duration :: Float -> [Float] -> [Float]
duration = take . round . (* (48000.0 * 60.0 / bpm))

playTups :: [(Float, Maybe Note)] -> [Float]
playTups = concatMap
  (\(dur, mnote) -> duration dur $ case mnote of
    Just note -> sinWave defAnchor note
    Nothing   -> repeat 0
  )

sevenNationArmy = playTups
  [ (20  , Nothing)
  , (1.0 , Just (Note A nat 2))
  , (0.5 , Nothing)
  , (0.5 , Just (Note A nat 2))
  , (0.75, Just (Note C nat 3))
  , (0.75, Just (Note A nat 2))
  , (0.5 , Just (Note G nat 2))
  , (1.0 , Just (Note F nat 2))
  , (1.0 , Nothing)
  , (1.0 , Just (Note E nat 2))
  , (1.0 , Nothing)
  , (1.0 , Just (Note A nat 2))
  , (0.5 , Nothing)
  , (0.5 , Just (Note A nat 2))
  , (0.75, Just (Note C nat 3))
  , (0.75, Just (Note A nat 2))
  , (0.5 , Just (Note G nat 2))
  , (0.75, Just (Note F nat 2))
  , (0.75, Just (Note G nat 2))
  , (0.5 , Just (Note F nat 2))
  , (2.0 , Just (Note E nat 2))
  ]





data Point = Point Float Float

mapPoint :: (Float -> a) -> Point -> (a, a)
mapPoint f (Point x y) = (f x, f y)

showFullPrecision :: Float -> String
showFullPrecision x = showFFloat Nothing x ""

test :: Note -> [(String, String)]
test note =
  map (mapPoint showFullPrecision) $ zipWith Point [0, step ..] $ sinWave defAnchor note

