{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Test
import qualified Data.BloomFilter as BF

main :: IO ()
main = do
  runInTerminal
  --Test.errorRateTest "experiment.out"

-- | Reads a word from the terminal and test if it pass the filter.
--
-- The filtered words are **swear words**.
runInTerminal :: IO ()
runInTerminal = do
  content <- readFile "swear_words.txt"
  filt <- pure $ BF.create 0.01 (words content)
  putStrLn ">>> Swear Words Filter (write \"quit\" to exit)"
  loop filt
    where
      loop !filt = do
        putStrLn ">>> Write a wear (don't swear!): "
        w <- getLine
        if w == "quit" || w == "q"
          then return ()
          else do
            if BF.elem filt w
              then putStrLn ("Swearing is not allowed!")
              else putStrLn ("The word " ++ w ++ " passes the filter.")
            loop filt
