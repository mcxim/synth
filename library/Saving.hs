module Saving where

import           System.Process
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Builder       as BU
import           Data.Foldable                  ( fold )

outputFile = "output.bin" :: FilePath

save :: [Float] -> IO ()
save = B.writeFile outputFile . BU.toLazyByteString . fold . map BU.floatLE

-- example: play $ volume 30 $ playTups $ parseNotes "1A2 .5p .5A2 .75C3 .75A2 .5G2 1F2 1p 1E2 1p"
play :: [Float] -> IO ()
play wave =
  save wave
    >> runCommand ("ffplay -showmode 1 -f f32le -ar 48000 " ++ outputFile)
    >> return ()
