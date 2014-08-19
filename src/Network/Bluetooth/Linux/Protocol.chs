{-# LANGUAGE ForeignFunctionInterface #-}
module Network.Bluetooth.Linux.Protocol (
      BluetoothProtocol(..)
    , BluetoothPort
    , isBluetoothPortValid
    ) where

import Data.Ix
import Data.Word

#include <bluetooth/bluetooth.h>

{#enum define BluetoothProtocol {
    BTPROTO_L2CAP  as L2CAP
  , BTPROTO_RFCOMM as RFCOMM
  } deriving (Ix, Show, Eq, Read, Ord, Bounded) #}

type BluetoothPort = Word16

isBluetoothPortValid :: BluetoothProtocol -> BluetoothPort -> Bool
isBluetoothPortValid RFCOMM port = inRange (1, 30) port
isBluetoothPortValid L2CAP  port = odd port && inRange (4097, 32767) port