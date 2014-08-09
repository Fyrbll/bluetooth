{-# LANGUAGE CPP, MagicHash, TupleSections #-}
module Network.Bluetooth.Utils
  ( cToEnum
  , cFromEnum
  , getFromIntegral
  , getRealToFrac
  , setFromIntegral
  , setRealToFrac
  , peekFromIntegral
  , peekRealToFrac
  , throwErrnoIfNegative
  , throwErrnoIfNegative_
  , throwErrnoIfNull_
  , unsafePeek
  , unsafePeekFromIntegral
  , unsafePeekRealToFrac
  , with'
  , withCast
  , withCastArray
  , withCastArray0
  , withCastArrayLen
  , withCastArrayLen0
  , withCastLen
  , withCastLenConv
  , withLen
  , withLenConv
  , byteSwap32
  ) where

import           Control.Monad

import qualified Data.Word as W
import           Data.Word (Word32)

import           Foreign.C.Error
import           Foreign.C.Types hiding (CSize)
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable

#if __GLASGOW_HASKELL__ < 708
import           GHC.Prim
import           GHC.Word
#endif

import           System.IO.Unsafe

#include <stddef.h>

-- | Remove this when <https://github.com/haskell/c2hs/issues/20 this issue> is resolved
type CSize = {#type size_t #}

cToEnum :: (Integral i, Enum e) => i -> e
cToEnum = toEnum . fromIntegral

cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum = fromIntegral . fromEnum

getFromIntegral :: (Integral i, Num n) => (Ptr a -> IO i) -> Ptr a -> IO n
getFromIntegral getter = fmap fromIntegral . getter

getRealToFrac :: (Real r, Fractional f) => (Ptr a -> IO r) -> Ptr a -> IO f
getRealToFrac getter = fmap realToFrac . getter

setFromIntegral :: (Integral i, Num n) => (Ptr a -> n -> IO ()) -> Ptr a -> i -> IO ()
setFromIntegral setter ptr = setter ptr . fromIntegral

setRealToFrac :: (Real r, Fractional f) => (Ptr a -> f -> IO ()) -> Ptr a -> r -> IO ()
setRealToFrac setter ptr = setter ptr . realToFrac

peekFromIntegral :: (Integral a, Storable a, Num b) => Ptr a -> IO b
peekFromIntegral = fmap fromIntegral . peek

peekRealToFrac :: (Real a, Storable a, Fractional b) => Ptr a -> IO b
peekRealToFrac = fmap realToFrac . peek

throwErrnoIfNegative :: (Num a, Ord a) => String -> IO a -> IO a
throwErrnoIfNegative = throwErrnoIf (< 0)

throwErrnoIfNegative_ :: (Num a, Ord a) => String -> IO a -> IO ()
throwErrnoIfNegative_ s = void . throwErrnoIfNegative s

throwErrnoIfNull_ ::  String -> IO (Ptr a) -> IO ()
throwErrnoIfNull_ s = void . throwErrnoIfNull s

unsafePeek :: Storable a => Ptr a -> a
unsafePeek = unsafePerformIO . peek

unsafePeekFromIntegral :: (Integral a, Storable a, Num b) => Ptr a -> b
unsafePeekFromIntegral = fromIntegral . unsafePeek

unsafePeekRealToFrac :: (Real a, Storable a, Fractional b) => Ptr a -> b
unsafePeekRealToFrac = realToFrac . unsafePeek

-- | Synonym for 'with' to avoid c2hs issues.
--   See <https://github.com/haskell/c2hs/issues/93 this issue>.
with' :: Storable s => s -> (Ptr s -> IO a) -> IO a
with' = with

withCast :: Storable s1 => s1 -> (Ptr s2 -> IO a) -> IO a
withCast val f = with val $ f . castPtr

withCastArray :: Storable s1 => [s1] -> (Ptr s2 -> IO a) -> IO a
withCastArray vals = withCastArrayLen vals . const

withCastArrayLen :: Storable s1 => [s1] -> (Int -> Ptr s2 -> IO a) -> IO a
withCastArrayLen vals f = withArrayLen vals $ \len -> f len . castPtr

withCastArray0 :: Storable s1 => s1 -> [s1] -> (Ptr s2 -> IO a) -> IO a
withCastArray0 marker vals = withCastArrayLen0 marker vals . const

withCastArrayLen0 :: Storable s1 => s1 -> [s1] -> (Int -> Ptr s2 -> IO a) -> IO a
withCastArrayLen0 marker vals f = withArrayLen0 marker vals $ \len -> f len . castPtr

withCastLen :: Storable s1 => s1 -> ((Ptr s2, CSize) -> IO a) -> IO a
withCastLen val f = withLen val $ f . mapFst castPtr

withCastLenConv :: (Num n, Storable s1) => s1 -> ((Ptr s2, n) -> IO a) -> IO a
withCastLenConv val f = withCastLen val $ f . mapSnd fromIntegral

withLen :: Storable a => a -> ((Ptr a, CSize) -> IO b) -> IO b
withLen val f = with val $ f . (, fromIntegral $ sizeOf val)

withLenConv :: (Num n, Storable s) => s -> ((Ptr s, n) -> IO a) -> IO a
withLenConv val f = withLen val $ f . mapSnd fromIntegral

byteSwap32 :: Word32 -> Word32
#if __GLASGOW_HASKELL__ >= 708
byteSwap32 = W.byteSwap32
#else
byteSwap32 (W32# w#) = W32# (narrow32Word# (byteSwap32# w#))
#endif

-------------------------------------------------------------------------------

-- allocaLenPtrConv :: (Integral i, Num n, Storable n) => ((p, Ptr n) -> IO a) -> (p, i) -> IO a
-- allocaLenPtrConv f (valPtr, len) = allocaBytes (sizeOf (undefined :: CLLong)) -- The largest C integral size
--   $ \lenPtr -> do
--     poke lenPtr $ fromIntegral len
--     f (valPtr, lenPtr)

mapFst :: (a1 -> a2) -> (a1, b) -> (a2, b)
mapFst f (x, y) = (f x, y)

mapSnd :: (b1 -> b2) -> (a, b1) -> (a, b2)
mapSnd = fmap