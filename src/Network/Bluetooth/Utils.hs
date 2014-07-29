module Network.Bluetooth.Utils
  ( cToEnum
  , cFromEnum
  , withCStringLenIntConv
  , peekCStringLenIntConv
  , throwErrnoIfNegative
  , throwErrnoIfNegative_
  , with'
  ) where

import Control.Monad

import Foreign.C.Error
import Foreign.C.String
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

cToEnum :: (Integral i, Enum e) => i -> e
cToEnum = toEnum . fromIntegral

cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum = fromIntegral . fromEnum

withCStringLenIntConv :: Num n => String -> ((CString, n) -> IO a) -> IO a
withCStringLenIntConv s f = withCStringLen s $ \(p, n) -> f (p, fromIntegral n)

peekCStringLenIntConv :: Integral n => CString -> n -> IO String
peekCStringLenIntConv s n = peekCStringLen (s, fromIntegral n)

throwErrnoIfNegative :: (Num a, Ord a) => String -> IO a -> IO a
throwErrnoIfNegative = throwErrnoIf (< 0)

throwErrnoIfNegative_ :: (Num a, Ord a) => String -> IO a -> IO ()
throwErrnoIfNegative_ s = void . throwErrnoIfNegative s

-- | Synonym for 'with' to avoid c2hs issues.
--   See <https://github.com/haskell/c2hs/issues/93 this issue>.
with' :: Storable a => a -> (Ptr a -> IO b) -> IO b
with' = with