{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Test (
    errorRateTest
  ) where

import           Control.Monad
import qualified Data.BloomFilter as BF
import           Data.Coerce
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           System.IO
import           Test.QuickCheck


-- | Runs an error rate test
-- and writes the result at the given file path.
errorRateTest :: FilePath -> IO ()
errorRateTest file =
  withFile file WriteMode $ \h -> do
    hSetBuffering h LineBuffering
    forM_ [100, 1000] $ \n -> do
      forM_ [0.05, 0.01] $ \e -> do
        hPutStrLn h ("----------------------------")
        hPutStrLn h ("N = " ++ show n)
        hPutStrLn h ("Expected ε = " ++ show e)
        (fp, fn) <- fmap unzip $ forM [0..9] $
            \(_ :: Int) -> do
              samples' <- generate (genSamples n)
              let (trainingSet, testingSet) = coerce @_ @([String], [String]) samples'
              let !filt = BF.create e trainingSet
              let wrds = Set.fromList trainingSet
              let (l, r) = foldr (testFilt filt wrds) (0,0) testingSet
              let overN x = (fromIntegral x) / (fromIntegral n) :: Double
              return (overN l, overN r)
        let (fpr, fnr) = (mean fp, mean fn)
        let (sfp, sfn) = (stdDev fp, stdDev fn)
        _ <- traverse (hPutStrLn h) [ "False Positive Rate"
                                    , "\tμ = " ++ show fpr
                                    , "\tσ = " ++ show sfp
                                    , "False Negative Rate"
                                    , "\tμ = " ++ show fnr
                                    , "\tσ = " ++ show sfn
                                    ]
        hPutStrLn h ("----------------------------")



-- | Checks if the filter classifies properly or not a word.
--
-- False Negative: "Is in the filter" says 'No' but the answer should be 'Yes'.
testFilt
  :: BF.Bloom String
  -> Set String
  -> String        -- ^ Word to test
  -> (Int, Int)    -- ^ (False Positive, False Negative)
  -> (Int, Int)    -- ^ New error
testFilt filt filteredWords w (fp, fn) =
  let predicted = BF.elem filt w
      labelled = Set.member w filteredWords
  in case (predicted, labelled) of
      (True, False) -> (fp + 1, fn)
      (False, True) -> (fp, fn + 1)
      _             -> (fp, fn)


genSamples :: Int
           -> Gen ([ASCIIString], [ASCIIString]) -- ^ (Training, Testing)
genSamples n = do
  xs <- vectorOf n (arbitrary :: Gen ASCIIString)
  return $ splitAt (truncate $ fromIntegral n * (0.75 :: Double)) xs

-- | Average
mean :: Fractional d => [d] -> d
mean ds = sum ds / fromIntegral (length ds)

-- | Standard Deviation
stdDev :: Floating d => [d] -> d
stdDev xs =
  let x = mean xs
  in sqrt $ (sum (fmap (\x' -> (x' - x)^(2 :: Integer)) xs)) / fromIntegral (length xs) - 1
