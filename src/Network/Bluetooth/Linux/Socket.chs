module Network.Bluetooth.Linux.Socket where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad

import Data.Either
import Data.Word

import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Network.Bluetooth.Linux.Addr
import Network.Bluetooth.Linux.Internal
import Network.Bluetooth.Utils
import Network.Socket

#include <bluetooth/bluetooth.h>
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
    return $ MkSocket fd family sockType (cFromEnum proto) status

bluetoothBind :: Socket -> BluetoothAddr -> Int -> IO ()
bluetoothBind (MkSocket fd _ _ proto sockStatus) bdaddr portNum = do
    modifyMVar_ sockStatus $ \status -> do
        when (status /= NotConnected) . ioError . userError $
          "bind: can't peform bind on socket in status " ++ show status
        btPort <- mkPort (cToEnum proto) portNum
        case cToEnum proto of
             L2CAP  -> callBind {#sizeof sockaddr_l2_t #}
                                {#set sockaddr_l2_t.l2_family #}
                                c_sockaddr_l2_set_bdaddr
                                {#set sockaddr_l2_t.l2_psm #}
                                (c_htobs $ getPort btPort)
             RFCOMM -> callBind {#sizeof sockaddr_rc_t #}
                                {#set sockaddr_rc_t.rc_family #}
                                c_sockaddr_rc_set_bdaddr
                                {#set sockaddr_rc_t.rc_channel #}
                                (getPort btPort)
        return Bound
  where
    callBind :: (Num s1, Num s2, SockAddrPtr p, Integral i) =>
                Int
             -> (Ptr p -> s1 -> IO ())
             -> (Ptr p -> Ptr BluetoothAddr -> IO ())
             -> (Ptr p -> s2 -> IO ())
             -> i
             -> IO ()
    callBind size setter1 setter2 setter3 port' =
      allocaBytes size $ \sockaddrPtr ->
      with bdaddr      $ \bdaddrPtr   -> do
          setFromIntegral setter1 sockaddrPtr $ packFamily AF_BLUETOOTH
          setter2 sockaddrPtr bdaddrPtr
          setFromIntegral setter3 sockaddrPtr port'
          throwErrnoIfMinus1_ "bind" $ c_bind fd sockaddrPtr size

bluetoothBindAnyPort :: Socket -> BluetoothAddr -> IO BluetoothPort
bluetoothBindAnyPort sock@(MkSocket _ _ _ proto _) bdaddr = do
    port <- bindAnyPort (cToEnum proto) bdaddr
    bluetoothBind sock bdaddr $ getPort port
    return port

data BluetoothPort = RFCOMMPort Word8
                   | L2CAPPort Word16
                   deriving Eq

instance Show BluetoothPort where
    show = show . getPort

getPort :: BluetoothPort -> Int
getPort (L2CAPPort p)  = fromIntegral p
getPort (RFCOMMPort p) = fromIntegral p

mkPort :: BluetoothProtocol -> Int -> IO BluetoothPort
mkPort RFCOMM port | 1 <= port && port <= 30
                   = return . RFCOMMPort $ fromIntegral port
mkPort RFCOMM _    = ioError $ userError "RFCOMM ports must be between 1 and 30."
mkPort L2CAP  port | odd port && 4097 <= port && port <= 32767
                   = return . L2CAPPort $ fromIntegral port
mkPort L2CAP  _    = ioError $ userError "L2CAP ports must be odd numbers between 4097 and 32,767."

bindAnyPort :: BluetoothProtocol -> BluetoothAddr -> IO BluetoothPort
bindAnyPort proto addr = do
    avails <- flip filterM (portRange proto) $ \portNum -> do
        sock <- bluetoothSocket proto
        res  <- try $ bluetoothBind sock addr portNum
        close sock
        return $ isRight (res :: Either IOError ())
    case avails of
        portNum:_ -> mkPort proto portNum
        _         -> ioError $ userError "Unable to find any available port"
  where
    portRange L2CAP  = [4097, 4099 .. 32767]
    portRange RFCOMM = [1 .. 30]

bluetoothListen :: Socket -> Int -> IO ()
bluetoothListen = listen -- TODO: tweak exception handling

bluetoothAccept :: Socket -> IO (Socket, BluetoothAddr)
bluetoothAccept (MkSocket fd family sockType proto sockStatus) = do
    currentStatus <- readMVar sockStatus
    when (currentStatus /= Connected && currentStatus /= Listening) . ioError . userError $
      "accept: can't perform accept on socket (" ++ show (family,sockType,proto) ++ ") in status " ++ show currentStatus
    case cToEnum proto of
         L2CAP  -> callAccept {#sizeof sockaddr_l2_t #} c_sockaddr_l2_get_bdaddr
         RFCOMM -> callAccept {#sizeof sockaddr_rc_t #} c_sockaddr_rc_get_bdaddr
  where
    callAccept :: SockAddrPtr p =>
                  Int
               -> (Ptr BluetoothAddr -> Ptr p -> IO (Ptr BluetoothAddr))
               -> IO (Socket, BluetoothAddr)
    callAccept size getter =
      allocaBytes size                                  $ \sockaddrPtr ->
      allocaBytes (sizeOf (undefined :: BluetoothAddr)) $ \bdaddrPtr   -> do
          (newFd,_) <- throwErrnoIf ((== -1) . fst) "accept" $ c_accept fd sockaddrPtr size
          _ <- getter bdaddrPtr sockaddrPtr
          bdaddr <- peek bdaddrPtr
          newStatus <- newMVar Connected
          return (MkSocket newFd family sockType proto newStatus, bdaddr)

-- bluetoothAccept :: Socket -> BluetoothAddr -> Int -> IO ()
-- bluetoothAccept (MkSocket fd _ _ proto sockStatus) bdaddr portNum = do
--     modifyMVar sockStatus $ \status -> do
--         when (status /= NotConnected) . ioError . userError $
--           "accept: can't peform connect on socket in status " ++ show status
--         btPort <- mkPort (cToEnum proto) bdaddr portNum

getSockPort :: Socket -> IO BluetoothPort
getSockPort (MkSocket fd _ _ proto status) = do
    currentStatus <- readMVar status
    when (currentStatus == NotConnected || currentStatus == Closed) . ioError . userError $
      "getsockname: can't get name of socket in status " ++ show currentStatus
    case cToEnum proto of
         L2CAP  -> callGetSockName {#sizeof sockaddr_l2_t #}
                                   ({#get sockaddr_l2_t.l2_psm #} :: SockAddrL2CAPPtr -> IO CUShort)
                                   L2CAPPort
         RFCOMM -> callGetSockName {#sizeof sockaddr_rc_t #}
                                   ({#get sockaddr_rc_t.rc_channel #} :: SockAddrRFCOMMPtr -> IO CUInt8)
                                   RFCOMMPort
  where
    callGetSockName :: (Integral i, Integral j, SockAddrPtr p) =>
                       Int
                    -> (Ptr p -> IO i)
                    -> (j -> BluetoothPort)
                    -> IO BluetoothPort
    callGetSockName size getter portCon = allocaBytes size $ \sockaddrPtr -> do
        throwErrnoIf_ ((== -1) . fst) "getsockname" $ c_getsockname fd sockaddrPtr size
        fmap portCon $ getFromIntegral getter sockaddrPtr 
        