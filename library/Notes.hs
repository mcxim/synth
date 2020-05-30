module Notes where

import           Data.Function                  ( on )

data Let = A | B | C | D | E | F | G deriving (Show, Eq)

data Acc = Natural | Flat | Sharp deriving (Show, Eq)

type Oct = Int

data Note = Note Let Acc Oct deriving (Show, Eq)

type Frequency = Double

type Anchor = (Note, Frequency)

freq :: Anchor -> Note -> Frequency
freq (anchorNote, anchorFreq) note = anchorFreq * (a ** n)
 where
  a = 2.0 ** (1 / 12) :: Double
  n = fromIntegral $ noteDiff note anchorNote

noteDiff :: Note -> Note -> Int
noteDiff = (-) `on` getNum

getNum :: Note -> Int
getNum (Note letter acc oct) = oct * 12 + appAcc
  acc
  (value letter)
 where
  appAcc Natural num = num
  appAcc Flat    num = num - 1
  appAcc Sharp   num = num + 1

value :: Let -> Int
value C = 0
value D = 2
value E = 4
value F = 5
value G = 7
value A = 9
value B = 11

