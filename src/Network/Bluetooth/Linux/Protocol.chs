{-# LANGUAGE ForeignFunctionInterface #-}
module Network.Bluetooth.Linux.Protocol where

import Data.Ix
import Data.Word

#include <bluetooth/bluetooth.h>

{#enum define BluetoothProtocol {
    BTPROTO_L2CAP  as L2CAP
  , BTPROTO_RFCOMM as RFCOMM
  } deriving (Ix, Show, Eq, Read, Ord, Bounded) #}

type BluetoothPort = Word16

isBluetoothPortValid :: BluetoothProtocol -> BluetoothPort -> Bool
isBluetoothPortValid RFCOMM port = 1 <= port && port <= 30
--                    = fromIntegral port
-- mkPort RFCOMM _    = error "RFCOMM ports must be between 1 and 30."
isBluetoothPortValid L2CAP  port = odd port && 4097 <= port && port <= 32767
--                    = fromIntegral port
-- mkPort L2CAP  _    = error "L2CAP ports must be odd numbers between 4097 and 32,767."