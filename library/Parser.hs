module Parser where

import           Notes
import           Data.Char                      ( isNumber
                                                , isLetter
                                                )

parseNotes :: String -> [(Float, Maybe Note)]
parseNotes = map parseWord . words

parseWord :: String -> (Float, Maybe Note)
parseWord word =
  let (dur, rest) = break isLetter ('0':word)
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


mystery = ".25G3 .25A3 .25C4 .25A3 .5E4 .25p .5E4 .25p .75D4 .75p "
       <> ".25G3 .25A3 .25C4 .25A3 .5D4 .25p .5D4 .25p .75C4 .75p "
       <> ".25G3 .25A3 .25C4 .25A3 1C4 .5D4 .2A3 .3B3 .5A3 .5G3 .5p .5G3 .2C4 .8D4 1C4 1p "
       <> ".25G3 .25A3 .25C4 .25A3 .5E4 .25p .5E4 .25p .75D4 .75p "
       <> ".25G3 .25A3 .25C4 .25A3 .5D5 .25p .5B3 .25p .2B3 .3C4 .5B3 .5A3 "
       <> ".25G3 .25A3 .25C4 .25A3 1C4 .5D4 .2A3 .3B3 .5A3 .5G3 .5p .5G3 .2C4 .8D4 1C4 1p "

-- how to play: play $ volume 30 $ playTups $ parseNotes mystery
