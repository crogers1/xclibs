{-# LANGUAGE MagicHash,FlexibleContexts #-}
-- |
-- Module      : Network.DBus.IEEE754
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Network.DBus.IEEE754 (encode, decode) where

--import GHC.Prim
--import GHC.Types
--import GHC.Word


import Data.Word (Word32, Word64)
import Data.Array.ST (newArray, castSTUArray, readArray, MArray, STUArray)
import GHC.ST (runST, ST)


decode :: Word64 -> Double
decode x = runST (cast x)

encode :: Double -> Word64
encode x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

-- | encode a double to a IEEE754 format
--encode :: Double -> Word64
--encode (D# x) = W64# (unsafeCoerce# x)

-- | decode a double from a IEEE754 format
--decode :: Word64 -> Double
--decode (W64# x) = D# (unsafeCoerce# x)
