module Visual where

import           Graphics.Gloss
import           Notes
import           Intervals
import           Data.List                      ( intersperse )
import           Data.Maybe                     ( fromMaybe )
import           Waves


genSingingNotes :: (Note, Note) -> [(Float, Maybe Note)]
genSingingNotes = concat . intersperse [(2.0, Nothing)] . map down5 . uncurry range

slides = genSingingNotes ((Note B flt 3), (Note C nat 4))

adjustTime = scanl1 (\(x, _) (y, s) -> (x + y, s))
  . map (\(dur, mnote) -> (dur * (60.0 / bpm), mnote))

slides' :: [(Float, Maybe Note)]
slides' = adjustTime slides

screenWidth = 720
screenHeight = 480

animation time =
  Translate (-340) (-20)
    . Scale 0.5 0.5
    . color white
    . Text
    . maybe "" show
    . snd
    . head
    . dropWhile ((< time) . fst)
    $ slides'

runAnim :: IO ()
runAnim = animate (InWindow "Window name" (screenWidth, screenHeight) (600, 300))
                  black
                  animation
