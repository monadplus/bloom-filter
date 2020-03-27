{-# LANGUAGE BangPatterns #-}
module Data.BitArray.Extras (
  -- * Newtype
    BitArray(..)
  -- * Constructors
  , new
  -- ** From a list of indexes that points to the bits sets to one
  , fromIndexes
  -- * operations
  , contains
  , length
  ) where

-------------------------------------------------------------------------------

import qualified Data.BitArray as BA
import           Data.Coerce
import           Prelude       hiding (length)

-------------------------------------------------------------------------------

newtype BitArray = BitArray { unBitArray :: BA.BitArray }
  deriving (Eq, Ord, Show)

-- | Returns a bit array of size m.
new :: Int -> BitArray
new m = coerce $ BA.bitArray (0, m) []
{-# INLINABLE new #-}

-- | Each index points to a bit in the array that should be set to 1.
--
-- Notice that the size if the array is equal to the highest index.
fromIndexes
    :: Int          -- ^ BitArray size.
    -> [Int]
    -> BitArray
fromIndexes m xs = coerce $
  BA.bitArray (0, m) (zip xs (repeat True))
{-# INLINABLE fromIndexes #-}

instance Semigroup BitArray where
  (BitArray a1) <> (BitArray a2) =
    let m = snd (BA.bitArrayBounds a1)
        bounds = (0, m)
        values = fmap (uncurry (||)) (zip (BA.bits a1) (BA.bits a2))
     in coerce $ BA.listBitArray bounds values
  {-# INLINABLE (<>) #-}

-- | Checks if the first array is contained in the second one.
-- I.e. all values that are set to true in the first one are also
-- set to true in the second.
contains :: BitArray
         -> BitArray
         -> Bool
contains (BitArray arr1) (BitArray arr2) =
  all go $ zip [0..] (BA.bits arr1)
    where
      go !(i, b) =
        if b then BA.lookupBit arr2 i
             else True
{-# INLINABLE contains #-}

length :: BitArray
       -> Int
length (BitArray arr1) =
  let !(!l, !r) = BA.bitArrayBounds arr1 in r - l
{-# INLINABLE length #-}
