{-# LANGUAGE DeriveDataTypeable #-}
module Network.Bluetooth.Exception where

import Control.Exception
import Data.Typeable
import Network.Bluetooth.Protocol

data BluetoothException = BluetoothPortException BluetoothProtocol BluetoothPort
  deriving Typeable

instance Show BluetoothException where
    show (BluetoothPortException L2CAP  port) = "L2CAP port " ++ show port ++ " is not an odd number between 4,097 and 32,767."
    show (BluetoothPortException RFCOMM port) = "RFCOMM port " ++ show port ++ " is not between 1 and 30."

instance Exception BluetoothException