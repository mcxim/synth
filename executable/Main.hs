module Main where

import Parser
import Saving
import Waves
import Notes

main :: IO ()
main = play $ volume 30 $ playTups $ parseNotes mystery
