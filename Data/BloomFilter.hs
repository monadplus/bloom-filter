{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE RecordWilCards #-}
module Data.BloomFilter (
  , Bloom(..)
  , Hash
  , easyList
  , elem
  ) where

-----------------------------------------------------------------

import           Data.BloomFilter.Hash (cheapHashes)
import           Data.BloomFilter.Util (nextPowerOfTwo)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import           Data.Word

import           Data.BitArray.Extras  (BitArray)
import qualified Data.BitArray.Extras  as BA

-----------------------------------------------------------------

type Hash = Word32

data Bloom a = Bloom
  { hashes   :: !(a -> [Hash])
  , bitArray :: {-# UNPACK #-} !BitArray
  }

easyList :: Hashable a
         => Double  -- ^ desired false positive rate (0 < /e/ < 1)
         -> [a]     -- ^ values to populate with
         -> Bloom a
easyList errRate xs = B.fromList (cheapHashes numHashes) numBits xs
   where capacity = length xs
         (numBits, numHashes)
             | capacity > 0 = suggestSizing capacity errRate
             | otherwise    = (1, 1)
{-# SPECIALIZE easyList :: Double -> [String] -> Bloom String #-}
{-# SPECIALIZE easyList :: Double -> [BS.ByteString] -> Bloom BS.ByteString #-}
{-# SPECIALIZE easyList :: Double -> [LSB.ByteString] -> Bloom LSB.ByteString #-}

elem :: a
     -> Bloom a
     -> Bool
elem a = \Bloom{..} ->
  contains (toArr hashes a) bitArray
{-# INLINEABLE elem #-}


-- TODO insert, inserList
-- TODO insert, inserList
-- TODO insert, inserList
-- TODO insert, inserList



----------------------------------------------------------------------------------------

fromList :: (a -> [Hash])  -- ^ hashing functions
         -> Int            -- ^ length of the bloom filter inner array.
         -> [a]
         -> BloomFilter a
fromList h m =
  Bloom h . foldr go (BA.new m)
    where
      go !arr x = arr <> (BA.toArr h x)
{-# INLINEABLE fromList #-}

toArr :: (a -> [Hash]) -- ^ Multi-hashing function
      -> a
      -> BA.BitArray
toArr h = BA.fromIndexes . fmap h
{-# INLINABLE toArr #-}


suggestSizing :: Int            -- ^ n: expected maximum capacity
              -> Double         -- ^ e: desired false positive rate (0 < /e/ < 1)
              -> (Int, Int)     -- ^ (m, k)
suggestSizing n e = either fatal id (safeSuggestSizing n e)
  where fatal = error . ("Data.BloomFilter.suggestSizing: " ++)

-- | Suggest a good combination of filter size and number of hash
-- functions for a Bloom filter, based on its expected maximum
-- capacity and a desired false positive rate.
--
-- The false positive rate is the rate at which queries against the
-- filter should return @True@ when an element is not actually
-- present.  It should be a fraction between 0 and 1, so a 1% false
-- positive rate is represented by 0.01.
safeSuggestSizing
    :: Int              -- ^ expected maximum capacity
    -> Double           -- ^ desired false positive rate (0 < /e/ < 1)
    -> Either String (Int, Int)
safeSuggestSizing capacity errRate
    | capacity <= 0                = Left "invalid capacity"
    | errRate <= 0 || errRate >= 1 = Left "invalid error rate"
    | otherwise =
    let cap = fromIntegral capacity
        (bits :: Double, hashes :: Double) =
            minimum [((-k) * cap / log (1 - (errRate ** (1 / k))), k)
                    | k <- [1..100]
                    ]
        roundedBits = nextPowerOfTwo (ceiling bits)
    in if roundedBits <= 0 || roundedBits > 0xffffffff
       then Left  "capacity too large to represent"
       else Right (roundedBits, truncate hashes)
