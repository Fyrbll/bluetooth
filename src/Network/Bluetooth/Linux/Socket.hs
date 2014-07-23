module Network.Bluetooth.Linux.Socket (
      bluetoothSocket
    ) where

import Control.Concurrent.MVar

import Foreign.C.Error

import Network.Bluetooth.Linux.Internal
import Network.Bluetooth.Utils
import Network.Socket

bluetoothSocket :: BluetoothProtocol -> IO Socket
bluetoothSocket proto = do
    let family  = AF_BLUETOOTH
        sockType = case proto of
            L2CAP  -> SeqPacket
            RFCOMM -> Stream
    fd <- throwErrnoIfMinus1 "socket" $ c_socket family sockType proto
    status <- newMVar NotConnected
    return $ MkSocket fd family sockType (cFromEnum proto) status

