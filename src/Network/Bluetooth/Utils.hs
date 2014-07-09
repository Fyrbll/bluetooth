module Network.Bluetooth.Utils
  ( cToEnum
  , cFromEnum
  ) where

cToEnum :: (Integral i, Enum e) => i -> e
cToEnum = toEnum . fromIntegral

cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum = fromIntegral . fromEnum