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

data Note = Note Let Acc Oct deriving (Show, Eq)

type Oct = Int

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

