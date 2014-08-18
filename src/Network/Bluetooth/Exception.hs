{-# LANGUAGE DeriveDataTypeable #-}
module Network.Bluetooth.Exception (BluetoothException(..)) where

import Control.Exception
import Data.Typeable
import Network.Bluetooth.Protocol

data BluetoothException = BluetoothPortException BluetoothProtocol BluetoothPort
  deriving Typeable

instance Show BluetoothException where
    showsPrec _ (BluetoothPortException L2CAP  port) = showString "L2CAP port " . shows port . showString " is not an odd number between 4,097 and 32,767."
    showsPrec _ (BluetoothPortException RFCOMM port) = showString "RFCOMM port " . shows port . showString " is not between 1 and 30."

instance Exception BluetoothException