{-# LANGUAGE CPP, MagicHash, TupleSections #-}
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
  , withCast
  , withCastArray
  , withCastArray0
  , withCastArrayLen
  , withCastArrayLen0
  , withCastLen
  , withCastLenPtr
  , withCastLenIntConv
  , withCastLenPtrIntConv
  , withLen
  , withLenPtr
  , withLenIntConv
  , withLenPtrIntConv
  , byteSwap32
  ) where

import           Control.Monad

import qualified Data.Word as W
import           Data.Word (Word32)

import           Foreign.C.Error
import           Foreign.C.String
import           Foreign.C.Types
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

withCastLen :: Storable s1 => s1 -> ((Ptr s2, CUInt) -> IO a) -> IO a
withCastLen val f = withLen val $ f . mapFst castPtr

withCastLenPtr :: Storable s1 => s1 -> ((Ptr s2, Ptr CUInt) -> IO a) -> IO a
withCastLenPtr val f = withLenPtr val $ f . mapFst castPtr

withCastLenIntConv :: (Num n, Storable s1) => s1 -> ((Ptr s2, n) -> IO a) -> IO a
withCastLenIntConv val f = withCastLen val $ f . mapSnd fromIntegral

withCastLenPtrIntConv :: (Num n, Storable n, Storable s1) => s1 -> ((Ptr s2, Ptr n) -> IO a) -> IO a
withCastLenPtrIntConv val = withCastLen val . allocaLenPtrIntConv

withLen :: Storable a => a -> ((Ptr a, CUInt) -> IO b) -> IO b
withLen val f = with val $ f . (, fromIntegral $ sizeOf val)

withLenPtr :: Storable s => s -> ((Ptr s, Ptr CUInt) -> IO a) -> IO a
withLenPtr val f = withLen val $ \(valPtr, len) -> with len $ \lenPtr -> f (valPtr, lenPtr)

withLenIntConv :: (Num n, Storable s) => s -> ((Ptr s, n) -> IO a) -> IO a
withLenIntConv val f = withLen val $ f . mapSnd fromIntegral

withLenPtrIntConv :: (Num n, Storable n, Storable s) => s -> ((Ptr s, Ptr n) -> IO a) -> IO a
withLenPtrIntConv val = withLen val . allocaLenPtrIntConv

byteSwap32 :: Word32 -> Word32
#if __GLASGOW_HASKELL__ >= 708
byteSwap32 = W.byteSwap32
#else
byteSwap32 (W32# w#) = W32# (narrow32Word# (byteSwap32# w#))
#endif

-------------------------------------------------------------------------------

allocaLenPtrIntConv :: (Integral i, Num n, Storable n) => ((p, Ptr n) -> IO a) -> (p, i) -> IO a
allocaLenPtrIntConv f (valPtr, len) = allocaBytes (sizeOf (undefined :: CLLong)) -- The largest C integral size
  $ \lenPtr -> do
    poke lenPtr $ fromIntegral len
    f (valPtr, lenPtr)

mapFst :: (a1 -> a2) -> (a1, b) -> (a2, b)
mapFst f (x, y) = (f x, y)

mapSnd :: (b1 -> b2) -> (a, b1) -> (a, b2)
mapSnd = fmap