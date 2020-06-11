module Visual where

import           Graphics.Gloss
import           Notes

glossShowAcc :: Acc -> String
glossShowAcc acc | acc == nat = ""
                 | acc == flt = "b"
                 | acc == srp = "#"

glossShow :: Note -> String
glossShow (Note l a o) = show l <> glossShowAcc a <> " (" <> show o <> ")"

slides :: [(Float, Note)]
slides = [(1.0, Note A nat 4), (0.5, Note C flt 7)]

accSlides = scanl1 (\(x, _) (y, s) -> (x + y, s)) slides

runAnim :: IO ()
runAnim = animate
  (InWindow "Window name" (700, 100) (10, 10))
  black
  (\s ->
    Translate (-340) (-20)
      . Scale 0.5 0.5
      . color white
      . Text
      . glossShow
      . snd
      . head
      . dropWhile ((< s) . fst)
      $ accSlides
  )
