module Network.Bluetooth.Linux.Socket (
      bluetoothSocket
    ) where

import Control.Concurrent.MVar

import Foreign.C.Error
-- import Foreign.Marshal.Alloc

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
    return $ MkSocket (fromIntegral fd) family sockType (cFromEnum proto) status

-- bluetoothBind :: Socket -> BluetoothAddr -> Int -> IO ()
-- bluetoothBind (MkSocket fd family sockType proto sockStatus) = do
--     modifyMVar_ sockStatus $ \status -> do
--         when (status /= NotConnected) . ioError . userError $
--           "bind: can't peform bind on socket in status " ++ show status
--         case cToEnum proto of
--              L2CAP  -> allocaBytes {#sizeof sockaddr_l2 #} $ \sockaddr -> do
--                  
--              RFCOMM ->