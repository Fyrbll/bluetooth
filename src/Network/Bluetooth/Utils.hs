{-# LANGUAGE CPP, MagicHash #-}
module Network.Bluetooth.Utils
  ( cToEnum
  , cFromEnum
  , getFromIntegral
  , getRealToFrac
  , setFromIntegral
  , setRealToFrac
  , withCStringLenIntConv
  , peekCStringLenIntConv
  , peekFromIntegral
  , peekRealToFrac
  , throwErrnoIfNegative
  , throwErrnoIfNegative_
  , throwErrnoIfNull_
  , unsafePeek
  , unsafePeekFromIntegral
  , unsafePeekRealToFrac
  , with'
  , byteSwap32
  ) where

import           Control.Monad

import qualified Data.Word as W

import           Foreign.C.Error
import           Foreign.C.String
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable

#if __GLASGOW_HASKELL__ < 708
import           GHC.Prim
import           GHC.Word
#endif

import           System.IO.Unsafe

cToEnum :: (Integral i, Enum e) => i -> e
cToEnum = toEnum . fromIntegral

cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum = fromIntegral . fromEnum

getFromIntegral :: (Integral i, Num n) => (Ptr a -> IO i) -> Ptr a -> IO n
getFromIntegral getter = fmap fromIntegral . getter

getRealToFrac :: (Real r, Fractional f) => (Ptr a -> IO r) -> Ptr a -> IO f
getRealToFrac getter = fmap realToFrac . getter

setFromIntegral :: (Integral i, Num n, Storable n) => (Ptr a -> n -> IO ()) -> Ptr a -> i -> IO ()
setFromIntegral setter ptr = setter ptr . fromIntegral

setRealToFrac :: (Real r, Fractional f, Storable f) => (Ptr a -> f -> IO ()) -> Ptr a -> r -> IO ()
setRealToFrac setter ptr = setter ptr . realToFrac

withCStringLenIntConv :: Num n => String -> ((CString, n) -> IO a) -> IO a
withCStringLenIntConv s f = withCStringLen s $ \(p, n) -> f (p, fromIntegral n)

peekFromIntegral :: (Integral a, Storable a, Num b) => Ptr a -> IO b
peekFromIntegral = fmap fromIntegral . peek

peekRealToFrac :: (Real a, Storable a, Fractional b) => Ptr a -> IO b
peekRealToFrac = fmap realToFrac . peek

peekCStringLenIntConv :: Integral n => CString -> n -> IO String
peekCStringLenIntConv s n = peekCStringLen (s, fromIntegral n)

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
with' :: Storable a => a -> (Ptr a -> IO b) -> IO b
with' = with

byteSwap32 :: W.Word32 -> W.Word32
#if __GLASGOW_HASKELL__ >= 708
byteSwap32 = W.byteSwap32
#else
byteSwap32 (W32# w#) = W32# (narrow32Word# (byteSwap32# w#))
#endif