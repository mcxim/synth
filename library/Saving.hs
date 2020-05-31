module Saving where

import           System.Process
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Builder       as BU
import           Data.Foldable                  ( fold )

outputFile = "output.bin" :: FilePath

save :: [Float] -> IO ()
save = B.writeFile outputFile . BU.toLazyByteString . fold . map BU.floatLE

play :: [Float] -> IO ()
play wave =
  save wave
    >> runCommand ("ffplay -showmode 1 -f f32le -ar 48000 " ++ outputFile)
    >> return ()

-- play $ take 200 $ sinWave defAnchor $ Note A Natural 4
