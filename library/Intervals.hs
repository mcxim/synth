module Intervals where

import Notes

next (Note B (:♮) num) = Note C        (:♮) (succ num)
next (Note E (:♮) num) = Note F        (:♮) num
next (Note l (:♭) num) = Note l        (:♮) num
next (Note l (:♯) num) = Note (succ l) (:♮) num
next (Note l (:♮) num) = Note l        (:♯) num
prev (Note C (:♮) num) = Note B        (:♮) (pred num)
prev (Note F (:♮) num) = Note E        (:♮) num
prev (Note l (:♯) num) = Note l        (:♮) num
prev (Note l (:♭) num) = Note (pred l) (:♮) num
prev (Note l (:♮) num) = Note l        (:♭) num


range :: Note -> Note -> [Note]
range start end | start == end = pure start
                | otherwise    = start : range (next start) end

rangeUp :: Note -> [Note]
rangeUp start = start : rangeUp (next start)

rangeDown :: Note -> [Note]
rangeDown start = start : rangeDown (prev start)

majorFrom :: Note -> [Note]
majorFrom = scale [0, 2, 2, 1, 2, 2, 2, 1]

minorFrom :: Note -> [Note]
minorFrom = scale [0, 2, 1, 2, 2, 1, 2, 2]

down5 :: Note -> [(Float, Maybe Note)]
down5 anchor =
  zipWith (\dur note -> (dur, Just note)) [1, 1, 1, 1, 2] (scale [7, -2, -1, -2, -2] anchor)
    <> [(2, Nothing)]

scale :: [Int] -> Note -> [Note]
scale []       _ = []
scale (x : xs) n = newNote : scale xs newNote where newNote = appInterval x n

appInterval :: Int -> Note -> Note
appInterval x | x > 0     = (!! x) . rangeUp
              | x < 0     = (!! (-x)) . rangeDown
              | otherwise = id