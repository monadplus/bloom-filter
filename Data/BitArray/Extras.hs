{-# LANGUAGE BangPatterns #-}
module Data.BitArray.Extras (
    module Data.BitArray
  , module Data.BitArray.Extras
  ) where

-------------------------------------------------------------------------------

import  Data.BitArray (BitArray)
import qualified Data.BitArray as BitArray
import Data.Foldable

-------------------------------------------------------------------------------

-- | Returns a bit array of size m.
new :: Int -> BitArray
new m = BA.bitArray (0, m) []
{-# INLINABLE new #-}

fromIndexes :: [Int] -> BitArray
fromIndexes xs = BA.bitArray (0, maximum xs) (zip xs (repeat True))
{-# INLINABLE fromIndexes #-}

instance Semigroup BitArray where
  a1 <> a2 =
    let m = snd (BitArray.bitArrayBounds a1)
        bounds = (0, m)
        values = fmap (||) (zip (BA.bits a1) (BA.bits a2))
     in BA.listBitArray bounds values
  {-# INLINABLE (<>) #-}

-- | Checks if the first array is contained in the second one.
-- I.e. all values that are set to true in the first one are also
-- set to true in the second.
contains :: BitArray
         -> BitArray
         -> Bool
contains arr1 arr2 =
  all cmp $ zip (BA.bits arr1) [0..]
    where
      go !(i, b) =
        if b then BA.lookupBit arr2 i
             else True
{-# INLINABLE contains #-}
