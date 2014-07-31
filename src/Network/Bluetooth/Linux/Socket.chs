module Network.Bluetooth.Linux.Socket where

import Control.Concurrent.MVar
import Control.Monad

import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import Network.Bluetooth.Linux.Addr
import Network.Bluetooth.Linux.Internal
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
             L2CAP  -> callBind {#sizeof sockaddr_l2_t #}
                                {#set sockaddr_l2_t.l2_family #}
                                ({#set sockaddr_l2_t.l2_bdaddr.b #} :: SockAddrL2CAPPtr -> BluetoothAddrArray -> IO ())
                                {#set sockaddr_l2_t.l2_psm #}
                                (c_htobs port)
             RFCOMM -> callBind {#sizeof sockaddr_rc_t #}
                                {#set sockaddr_rc_t.rc_family #}
                                ({#set sockaddr_rc_t.rc_bdaddr.b #} :: SockAddrRFCOMMPtr -> BluetoothAddrArray -> IO ())
                                {#set sockaddr_rc_t.rc_channel #}
                                port
        return Bound
  where
    callBind :: (Num s1, Num s2, SockAddrPtr p, Integral i) =>
                Int
             -> (Ptr p -> s1 -> IO ())
             -> (Ptr p -> BluetoothAddrArray -> IO ())
             -> (Ptr p -> s2 -> IO ())
             -> i
             -> IO ()
    callBind size poker1 setter poker2 port' = allocaBytes size $ \sockaddr -> do
         poker1 sockaddr . fromIntegral $ packFamily AF_BLUETOOTH
         asArray bdaddr $ setter sockaddr
         poker2 sockaddr $ fromIntegral port'
         throwErrnoIfMinus1_ "bind" $ c_bind (fromIntegral fd) sockaddr size

bluetoothListen :: Socket -> Int -> IO ()
bluetoothListen = listen -- TODO: tweak exception handling

bluetoothAccept :: Socket -> IO (Socket, BluetoothAddr)
bluetoothAccept (MkSocket fd family sockType proto sockStatus) = do
    currentStatus <- readMVar sockStatus
    when (currentStatus /= Connected && currentStatus /= Listening) . ioError . userError $
      "accept: can't perform accept on socket (" ++ show (family,sockType,proto) ++ ") in status " ++ show currentStatus
    case cToEnum proto of
         L2CAP  -> callAccept {#sizeof sockaddr_l2_t #}
                              ({#get sockaddr_l2_t.l2_bdaddr.b #} :: SockAddrL2CAPPtr -> IO BluetoothAddrArray)
         RFCOMM -> callAccept {#sizeof sockaddr_rc_t #}
                              ({#get sockaddr_rc_t.rc_bdaddr.b #} :: SockAddrRFCOMMPtr -> IO BluetoothAddrArray)
  where
    callAccept :: SockAddrPtr p => Int -> (Ptr p -> IO BluetoothAddrArray) -> IO (Socket, BluetoothAddr)
    callAccept size peeker = allocaBytes size $ \sockaddr -> do
        newFd <- throwErrnoIfMinus1 "accept" $ c_accept (fromIntegral fd) sockaddr
        arr <- peeker sockaddr
        bdaddr <- fromArray arr
        newStatus <- newMVar Connected
        return (MkSocket (fromIntegral newFd) family sockType proto newStatus, bdaddr)