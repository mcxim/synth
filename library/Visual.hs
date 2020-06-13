module Visual where

import           Graphics.Gloss          hiding ( play )
import           Notes
import           Intervals
import           Data.List                      ( intersperse )
import           Data.Maybe                     ( fromMaybe )
import           Waves
import           Control.Concurrent             ( forkIO )
import           Saving
import           Data.List                      ( groupBy )
import           Data.Function                  ( on )

screenWidth = 720
screenHeight = 480

rootPrefix = "Root "

genSingingNotes :: (Note, Note) -> [(String, [(Float, Maybe Note)])]
genSingingNotes =
  map (\root -> (rootPrefix <> show root, down5 root <> pure (2.0, Nothing)))
    . uncurry range

singingNotes = genSingingNotes ((Note B flt 3), (Note D flt 6))

playable = concat . map snd $ singingNotes

adjustTime = scanl1 (\(x, _) (y, s) -> (x + y, s))
  . map (\(dur, mnote) -> (dur * (60.0 / bpm), mnote))

adjusted :: [(Float, String)]
adjusted =
  adjustTime
    . map
        (\(root, (duration, mnote)) -> (duration, (root <> ": " <> maybe "" show mnote))
        )
    . concat
    . map (\(root, notes) -> map ((,) root) notes)
    $ singingNotes


animation time =
  Translate (-340) (-20)
    . Scale 0.5 0.5
    . color white
    . Text
    . snd
    . head
    . dropWhile ((< time) . fst)
    $ adjusted

runAnim :: IO ()
runAnim = animate (InWindow "Window name" (screenWidth, screenHeight) (600, 300))
                  black
                  animation

runAll :: IO ()
runAll = do
  save $! volume 30 $ playTups $ playable
  forkIO $ onlyPlay
  forkIO runAnim
  return ()


stamps =
  unlines
    . zipWith
        (\root time ->
          ((\(mins, secs) -> show mins <> ":" <> show secs) $ quotRem (round time) 60)
            <> " : "
            <> root
        )
        (map (takeWhile (/= ':') . snd . head) xs)
    . (:) 0
    . map (fst . last)
    $ xs
 where
  xs =
    groupBy ((==) `on` ((takeWhile (/= ':') . drop (length rootPrefix)) . snd)) adjusted
