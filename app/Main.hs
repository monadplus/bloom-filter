module Main where

import qualified Data.BloomFilter as BF

main :: IO ()
main = do
  content <- readFile "words.txt"
  filt <- BF.easyList 0.01 (words content)
  loop filt
    where
      loop filt = do
        putStrLn "Write a word: "
        w <- getLine
        if w == "quit" then return ()
          else $ do
            if BF.elem w filt
              then putStrLn ("The word " ++ w ++ " does not pass the filter.")
              else putStrLn ("The word " ++ w ++ " passes the filter.")
            loop filt
