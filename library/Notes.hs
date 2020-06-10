module Notes where

{-# LANGUAGE UnicodeSyntax #-}

import           Data.Function                  ( on )
import           Data.Tuple                     ( swap )
import           Data.Maybe                     ( fromJust )

data Let = A | B | C | D | E | F | G deriving (Show, Read, Eq)

instance Enum Let where
  succ = fromJust . flip lookup table
  pred = fromJust . flip lookup (map swap table)
  
table = [(A, B), (B, C), (C, D), (D, E), (E, F), (F, G), (G, A)]

data Acc = (:♮) | (:♭) | (:♯) deriving (Show, Eq)

nat = (:♮)
flt = (:♭)
srp = (:♯)

type Oct = Int

data Note = Note Let Acc Oct deriving (Show, Eq)

next (Note B (:♮) num) = Note C nat (succ num)
next (Note E (:♮) num) = Note F nat num
next (Note l (:♭) num) = Note l nat num
next (Note l (:♯) num) = Note (succ l) nat num
next (Note l (:♮) num) = Note l srp num
prev (Note C (:♮) num) = Note B nat (pred num)
prev (Note F (:♮) num) = Note E nat num
prev (Note l (:♯) num) = Note l nat num
prev (Note l (:♭) num) = Note (pred l) nat num
prev (Note l (:♮) num) = Note l flt num


range :: Note -> Note -> [Note]
range start end | start == end = pure start
                | otherwise    = start : (range (next start) end)

rangeUp :: Note -> [Note]
rangeUp start = start : (rangeUp (next start))

rangeDown :: Note -> [Note]
rangeDown start = start : (rangeDown (prev start))

majorFrom :: Note -> [Note]
majorFrom = appIntervals [2, 2, 1, 2, 2, 2, 1]

minorFrom :: Note -> [Note]
minorFrom = appIntervals [2, 1, 2, 2, 1, 2, 2]

appIntervals :: [Int] -> Note -> [Note]
appIntervals intervals note = note : (go intervals note)
 where
  go []       _ = []
  go (x : xs) n = newNote : (go xs newNote) where newNote = (iterate next n) !! x


type Frequency = Float

type Anchor = (Note, Frequency)

freq :: Anchor -> Note -> Frequency
freq (anchorNote, anchorFreq) note = anchorFreq * (a ** n)
 where
  a = 2.0 ** (1 / 12) :: Float
  n = fromIntegral $ noteDiff note anchorNote

noteDiff :: Note -> Note -> Int
noteDiff = (-) `on` getNum

getNum :: Note -> Int
getNum (Note letter acc oct) = oct * 12 + appAcc acc (value letter)
 where
  appAcc (:♮) num = num + 0
  appAcc (:♭) num = num - 1
  appAcc (:♯) num = num + 1

value :: Let -> Int
value C = 0
value D = 2
value E = 4
value F = 5
value G = 7
value A = 9
value B = 11

defAnchor = (Note A (:♮) 4, 440) :: Anchor

