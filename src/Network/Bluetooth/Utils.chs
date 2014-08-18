{-# LANGUAGE CPP, MagicHash, TupleSections #-}
module Network.Bluetooth.Utils
  ( byteSwap32
  , cToEnum
  , cFromEnum
  , getFromIntegral
  , getRealToFrac
  , setFromIntegral
  , setRealToFrac
  , peekFromIntegral
  , peekRealToFrac
  , throwSocketErrorIf
  , throwSocketErrorIfMinus1Retry_
  , throwSocketErrorIfNull
  , throwSocketErrorIfNull_
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
  ) where

import           Control.Monad

import qualified Data.Word as W
import           Data.Word (Word32)

import           Foreign.C.Types hiding (CSize)
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable

#if __GLASGOW_HASKELL__ < 708
import           GHC.Prim
import           GHC.Word
#endif

import           Network.Socket.Internal

import           System.IO.Unsafe

#include <stddef.h>

-- | Remove this when <https://github.com/haskell/c2hs/issues/20 this issue> is resolved
type CSize = {#type size_t #}

byteSwap32 :: Word32 -> Word32
#if __GLASGOW_HASKELL__ >= 708
byteSwap32 = W.byteSwap32
#else
byteSwap32 (W32# w#) = W32# (narrow32Word# (byteSwap32# w#))
#endif

-- TODO: Rename this to something more sensible.
cToEnum :: (Integral i, Enum e) => i -> e
cToEnum = toEnum . fromIntegral

-- TODO: Rename this to something more sensible.
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

throwSocketErrorIf :: (a -> Bool) -> String -> IO a -> IO a
throwSocketErrorIf p name act = do
    r <- act
    if p r
       then throwSocketError name
       else return r

throwSocketErrorIfMinus1Retry_ :: (Eq a, Num a) => String -> IO a -> IO ()
throwSocketErrorIfMinus1Retry_ name = void . throwSocketErrorIfMinus1Retry name

throwSocketErrorIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
throwSocketErrorIfNull = throwSocketErrorIf (== nullPtr)

throwSocketErrorIfNull_ :: String -> IO (Ptr a) -> IO ()
throwSocketErrorIfNull_ name = void . throwSocketErrorIfNull name

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

-------------------------------------------------------------------------------

mapFst :: (a1 -> a2) -> (a1, b) -> (a2, b)
mapFst f (x, y) = (f x, y)

mapSnd :: (b1 -> b2) -> (a, b1) -> (a, b2)
mapSnd = fmap