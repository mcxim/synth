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


mystery = "20p .25G3 .25A3 .25C4 .25A3 .5E4 .25p .5E4 .25p .75D4 .75p "
       <> ".25G3 .25A3 .25C4 .25A3 .5D4 .25p .5D4 .25p .75C4 .75p "
       <> ".25G3 .25A3 .25C4 .25A3 1C4 .5D4 .2A3 .3B3 .5A3 .5G3 .5p .5G3 .2C4 .8D4 1C4 1p "
       <> ".25G3 .25A3 .25C4 .25A3 .5E4 .25p .5E4 .25p .75D4 .75p "
       <> ".25G3 .25A3 .25C4 .25A3 .5D5 .25p .5B3 .25p .2B3 .3C4 .5B3 .5A3 "
       <> ".25G3 .25A3 .25C4 .25A3 1C4 .5D4 .2A3 .3B3 .5A3 .5G3 .5p .5G3 .2C4 .8D4 1C4 1p "

hurricane = ".5p .25A3 .25G3 .25A3 .25p .25A3 .25G3 .25A3 .25G3 .25E3 .25p .25E3 .5D3 .75F#3 .75D3 1.5p "
         <> ".25A2 .5D3 .25C3 .5E3 1C3 2.25p .25A2 .25D3 .25E3 1D3 2.5p "
         <> ".25E3 .25A3 .25G3 .25A3 .25G3 .25A3 .25G3 .25A3 .25G3 1E3 .25p .25D3 .75F#3 .75G3 .25G3 1F#3 "
         <> ".25G3 .75A3 .25G3 .25F#3 .25F#3 .75E3 1.75p .25D3 .25E3 .5A2 .5D3 .25E3 .5D3"

papaoutai = "20p 1p .5A3 .5A3 .25p .5A3 .25A3 .5C4 .5D4 1p .5A3 .5A3 .25p .5A3 .25A3 .5C4 .5B3 "
         <> "1p .5A3 .5A3 .25p .5A3 .25A3 .5C4 .5D4 .5D4 .5C4 .5B3 .5C4 .25A3 .5A3 .25A3 .5A3 .5A3"

-- how to play: play $ volume 30 $ playTups $ parseNotes mystery
