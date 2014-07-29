module Network.Bluetooth.Linux.Socket (
      bluetoothSocket
    , bluetoothBind
    ) where

import Control.Concurrent.MVar
import Control.Monad

import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import Network.Bluetooth.Linux.Internal
import Network.Bluetooth.Linux.Types
import Network.Bluetooth.Utils
import Network.Socket

#include "wr_l2cap.h"
#include "wr_rfcomm.h"

bluetoothSocket :: BluetoothProtocol -> IO Socket
bluetoothSocket proto = do
    let family  = AF_BLUETOOTH
        sockType = case proto of
            L2CAP  -> SeqPacket
            RFCOMM -> Stream
    fd <- throwErrnoIfMinus1 "socket" $ c_socket family sockType proto
    status <- newMVar NotConnected
    return $ MkSocket (fromIntegral fd) family sockType (cFromEnum proto) status

bluetoothBind :: Socket -> BluetoothAddr -> Int -> IO ()
bluetoothBind (MkSocket fd _ _ proto sockStatus) bdaddr port = do
    modifyMVar_ sockStatus $ \status -> do
        when (status /= NotConnected) . ioError . userError $
          "bind: can't peform bind on socket in status " ++ show status
        case cToEnum proto of
             L2CAP  -> callBind {#sizeof sockaddr_l2_t #} {#set sockaddr_l2_t.l2_family #} c_sockaddr_l2_set_l2_bdaddr {#set sockaddr_l2_t.l2_psm #} $ c_htobs port
             RFCOMM -> callBind {#sizeof sockaddr_rc_t #} {#set sockaddr_rc_t.rc_family #} c_sockaddr_rc_set_rc_bdaddr {#set sockaddr_rc_t.rc_channel #} port
        return Bound
   where
     callBind :: (Num s1, Num s2, SockAddrPtr p, Integral i) =>
                    Int
                 -> (Ptr p -> s1 -> IO ())
                 -> (Ptr p -> BluetoothAddr -> IO ())
                 -> (Ptr p -> s2 -> IO ())
                 -> i
                 -> IO ()
     callBind size poker1 setter poker2 port' = allocaBytes size $ \sockaddr -> do
         poker1 sockaddr . fromIntegral $ packFamily AF_BLUETOOTH
         setter sockaddr bdaddr
         poker2 sockaddr $ fromIntegral port'
         throwErrnoIfMinus1_ "bind" $ c_bind (fromIntegral fd) sockaddr size

bluetoothListen :: Socket -> Int -> IO ()
bluetoothListen = listen

-- bluetoothAccept :: Socket -> IO (Socket, BluetoothAddr)
-- bluetoothAccept (MkSocket fd family sockType proto sockStatus) = do
--     currentStatus <- readMVar status
--     when (value /= Connected && value /= Listening) . ioError . userError $
--       "accept: can't perform accept on socket (" ++ show (family,sockType,proto) ++ ") in status " ++ show currentStatus
--     case cToEnum proto of
--          L2CAP  -> callAccept {#sizeof sockaddr_l2_t #} GETTER
--          RFCOMM -> 
--   where callAccept :: Int -> A
--         callAccept size peeker = allocaBytes size $ \sockaddr -> do
--             newFd <- throwErrnoIfMinus1 "accept" $ c_accept (fromIntegral fd) sockaddr size
--             bdaddr <- peeker sockaddr
--             newStatus <- newMVar Connected
--             return (MkSocket newFd family sockType proto newStatus, bdaddr)