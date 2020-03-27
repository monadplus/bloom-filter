{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.BloomFilter (
    Bloom(..)
  , create
  , elem
  , length
  ) where

-----------------------------------------------------------------

import           Data.BitArray.Extras  (BitArray)
import qualified Data.BitArray.Extras  as BA
import           Data.BloomFilter.Hash (Hashable (..), cheapHashes)
import           Data.BloomFilter.Util (nextPowerOfTwo)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.List             as List
import           Data.Word
import           Prelude               hiding (elem, length)

-----------------------------------------------------------------

type Hash = Word32

data Bloom a = Bloom
  { hashes   :: !(a -> [Hash])
  , bitArray :: {-# UNPACK #-} !BitArray
  }

create :: Hashable a
         => Double  -- ^ desired false positive rate (0 < /e/ < 1)
         -> [a]     -- ^ values to populate with
         -> Bloom a
create errRate xs = fromList (cheapHashes numHashes) numBits xs
   where capacity = List.length xs
         (numBits, numHashes)
             | capacity > 0 = suggestSizing capacity errRate
             | otherwise    = (1, 1)
{-# SPECIALIZE create :: Double -> [String] -> Bloom String #-}
{-# SPECIALIZE create :: Double -> [BS.ByteString] -> Bloom BS.ByteString #-}
{-# SPECIALIZE create :: Double -> [LBS.ByteString] -> Bloom LBS.ByteString #-}

elem :: Bloom a
     -> a
     -> Bool
elem b@Bloom{..} = \a ->
  BA.contains (toArr hashes m a) bitArray
    where
      m = length b
{-# INLINEABLE elem #-}

length :: Bloom a
       -> Int
length Bloom{..} =
  BA.length bitArray
{-# INLINEABLE length #-}


-- TODO insert, inserList



----------------------------------------------------------------------------------------

fromList :: (a -> [Hash])  -- ^ hashing functions
         -> Int            -- ^ length of the bloom filter inner array.
         -> [a]
         -> Bloom a
fromList h m =
  Bloom h . foldr go (BA.new m)
    where
      go x !arr = arr <> (toArr h m x)
{-# INLINEABLE fromList #-}

toArr :: (a -> [Hash]) -- ^ Multi-hashing function
      -> Int           -- BitArray size
      -> a
      -> BitArray
toArr h m = BA.fromIndexes m . fmap (congruent . fromIntegral) . h
  where
    congruent = (flip mod) m
{-# INLINABLE toArr #-}


suggestSizing :: Int            -- ^ n: expected maximum capacity
              -> Double         -- ^ e: desired false positive rate (0 < /e/ < 1)
              -> (Int, Int)     -- ^ (m, k)
suggestSizing n e = either fatal id (safeSuggestSizing n e)
  where fatal = error . ("Data.BloomFilter.suggestSizing: " ++)
{-# INLINABLE suggestSizing #-}

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
{-# INLINABLE safeSuggestSizing #-}
