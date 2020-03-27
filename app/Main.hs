{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.BloomFilter as BF

main :: IO ()
main = do
  content <- readFile "words.txt"
  filt <- pure $ BF.create 0.01 (words content)
  loop filt

loop :: (BF.Bloom String) -> IO ()
loop !filt = do
  putStrLn "Write a word (q to exit): "
  w <- getLine
  if w == "quit" || w == "q"
    then return ()
    else do
      if BF.elem filt w
        then putStrLn ("The word " ++ w ++ " does not pass the filter.")
        else putStrLn ("The word " ++ w ++ " passes the filter.")
      loop filt
