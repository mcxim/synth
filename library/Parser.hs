module Parser where

import           Notes
import           Data.Char                      ( isNumber
                                                , isLetter
                                                )

parseNotes :: String -> [(Float, Maybe Note)]
parseNotes = map parseWord . words

parseWord :: String -> (Float, Maybe Note)
parseWord word =
  let (dur, rest) = break isLetter word
  in  if head rest `elem` ['p', 'P']
        then (read dur, Nothing)
        else
          let (letters, octave)  = break isNumber rest
              (noteLetter : acc) = letters
          in  ( read dur
              , Just (Note (read $ pure noteLetter) (parseAcc acc) (read octave))
              )

parseAcc :: String -> Acc
parseAcc c | c `elem` ["n", ""]  = nat
           | c `elem` ["b", "f"] = flt
           | c `elem` ["s", "#"] = srp
           | otherwise           = nat
