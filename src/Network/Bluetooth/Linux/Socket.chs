{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Network.Bluetooth.Linux.Socket where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad

import Data.Either

import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Network.Bluetooth.Exception
import Network.Bluetooth.Linux.Addr
import Network.Bluetooth.Linux.Protocol
import Network.Bluetooth.Utils
import Network.Socket

#include <bluetooth/bluetooth.h>
#include <sys/socket.h>
#include "wr_l2cap.h"
#include "wr_rfcomm.h"

bluetoothSocket :: BluetoothProtocol -> IO Socket
bluetoothSocket proto = do
    let family   = AF_BLUETOOTH
        sockType = case proto of
            L2CAP  -> SeqPacket
            RFCOMM -> Stream
    fd <- throwErrnoIfMinus1 "socket" $ c_socket family sockType proto
    status <- newMVar NotConnected
    return $ MkSocket fd family sockType (cFromEnum proto) status

assignSocket :: (forall a. SockAddrBluetooth a => CInt -> Ptr a -> Int -> IO Int)
             -> String
             -> Socket -> BluetoothAddr -> BluetoothPort -> IO ()
assignSocket c_assigner name (MkSocket fd _ _ proto sockStatus) bdaddr portNum =
    modifyMVar_ sockStatus $ \status -> do
        when (status /= NotConnected) . ioError . userError $
          name ++ ": can't peform " ++ name ++ " on socket in status " ++ show status
        let protoEnum = cToEnum proto
--             btPort    = mkPort protoEnum portNum
        unless (isBluetoothPortValid protoEnum portNum) . throwIO $ BluetoothPortException protoEnum portNum
--             sockaddr = case protoEnum of
--                             L2CAP ->  SockAddrL2CAP family (c_htobs btPort) bdaddr
--                             RFCOMM -> SockAddrRFCOMM family bdaddr btPort
        case protoEnum of
             L2CAP  -> with (SockAddrL2CAP AF_BLUETOOTH (c_htobs portNum) bdaddr) $ \sockaddrPtr ->
               throwErrnoIfMinus1_ name $ c_assigner fd sockaddrPtr $ sizeOf (undefined :: SockAddrL2CAP)
             RFCOMM -> with (SockAddrRFCOMM AF_BLUETOOTH bdaddr $ fromIntegral portNum) $ \sockaddrPtr ->
               throwErrnoIfMinus1_ name . c_assigner fd sockaddrPtr $ sizeOf (undefined :: SockAddrRFCOMM)
--         let portNum' = (case protoEnum of
--                            L2CAP -> c_htobs
--                            RFCOMM -> id) $ getPort btPort
--         allocaBytes sockaddrSize $ \sockaddrPtr -> with bdaddr $ \bdaddrPtr -> do
--             setFromIntegral familySetter sockaddrPtr $ packFamily AF_BLUETOOTH
--             bdaddrSetter sockaddrPtr bdaddrPtr
--             setFromIntegral portSetter sockaddrPtr portNum'
--             throwErrnoIfMinus1_ name $ c_assigner fd sockaddrPtr sockaddrSize
        return Bound
--   where
--     callAssigner :: Socket -> a -> Ptr a -> Int -> IO ()
--     callAssigner (MkSocket fd _ _ _ _) sockaddr sockaddrPtr size = do
--         poke sockaddrPtr sockaddr
--         throwErrnoIfMinus1_ name $ c_assigner fd sockaddrPtr size

bluetoothBind :: Socket -> BluetoothAddr -> BluetoothPort -> IO ()
bluetoothBind = assignSocket c_bind "bind"
-- bluetoothBind sock@(MkSocket _ _ _ proto _) =
--   let bind' = case cToEnum proto of
--                    L2CAP  -> assignSocket {#sizeof sockaddr_l2_t #}
--                                           {#set sockaddr_l2_t.l2_family #}
--                                           c_sockaddr_l2_set_bdaddr
--                                           {#set sockaddr_l2_t.l2_psm #}
--                                           c_bind
--                    RFCOMM -> assignSocket {#sizeof sockaddr_rc_t #}
--                                           {#set sockaddr_rc_t.rc_family #}
--                                           c_sockaddr_rc_set_bdaddr
--                                           {#set sockaddr_rc_t.rc_channel #}
--                                           c_bind
--   in bind' "bind" sock
-- bluetoothBind (MkSocket fd _ _ proto sockStatus) bdaddr portNum = do
--     modifyMVar_ sockStatus $ \status -> do
--         when (status /= NotConnected) . ioError . userError $
--           "bind: can't peform bind on socket in status " ++ show status
--         let protoEnum = cToEnum proto
--         btPort <- mkPort protoEnum portNum
--         case protoEnum of
--              L2CAP  -> callBind {#sizeof sockaddr_l2_t #}
--                                 {#set sockaddr_l2_t.l2_family #}
--                                 c_sockaddr_l2_set_bdaddr
--                                 {#set sockaddr_l2_t.l2_psm #}
--                                 (c_htobs $ getPort btPort)
--              RFCOMM -> callBind {#sizeof sockaddr_rc_t #}
--                                 {#set sockaddr_rc_t.rc_family #}
--                                 c_sockaddr_rc_set_bdaddr
--                                 {#set sockaddr_rc_t.rc_channel #}
--                                 (getPort btPort)
--         return Bound
--   where
--     callBind :: (Num s1, Num s2, SockAddrPtr p, Integral i) =>
--                 Int
--              -> (Ptr p -> s1 -> IO ())
--              -> (Ptr p -> Ptr BluetoothAddr -> IO ())
--              -> (Ptr p -> s2 -> IO ())
--              -> i
--              -> IO ()
--     callBind size setter1 setter2 setter3 port' =
--       allocaBytes size $ \sockaddrPtr ->
--       with bdaddr      $ \bdaddrPtr   -> do
--           setFromIntegral setter1 sockaddrPtr $ packFamily AF_BLUETOOTH
--           setter2 sockaddrPtr bdaddrPtr
--           setFromIntegral setter3 sockaddrPtr port'
--           throwErrnoIfMinus1_ "bind" $ c_bind fd sockaddrPtr size

bluetoothBindAnyPort :: Socket -> BluetoothAddr -> IO BluetoothPort
bluetoothBindAnyPort sock@(MkSocket _ _ _ proto _) bdaddr = do
    port <- bindAnyPort (cToEnum proto) bdaddr
    bluetoothBind sock bdaddr $ fromIntegral port
    return port

-- data BluetoothPort = RFCOMMPort { rfcommPort :: Word8 }
--                    | L2CAPPort { l2capPort :: Word16 }
--                    deriving Eq
-- 
-- instance Show BluetoothPort where
--     show = show . getPort
-- 
-- getPort :: BluetoothPort -> Int
-- getPort (L2CAPPort p)  = fromIntegral p
-- getPort (RFCOMMPort p) = fromIntegral p

bindAnyPort :: BluetoothProtocol -> BluetoothAddr -> IO BluetoothPort
bindAnyPort proto addr = do
    avails <- flip filterM (portRange proto) $ \portNum -> do
        sock <- bluetoothSocket proto
        res  <- try $ bluetoothBind sock addr portNum
        close sock
        return $ isRight (res :: Either IOError ())
    case avails of
        portNum:_ -> return portNum
        _         -> ioError $ userError "Unable to find any available port"
  where
    portRange L2CAP  = [4097, 4099 .. 32767]
    portRange RFCOMM = [1 .. 30]

bluetoothListen :: Socket -> Int -> IO ()
bluetoothListen = listen -- TODO: tweak exception handling

bluetoothAccept :: Socket -> IO (Socket, BluetoothAddr)
bluetoothAccept sock@(MkSocket _ family sockType proto sockStatus) = do
    currentStatus <- readMVar sockStatus
    when (currentStatus /= Connected && currentStatus /= Listening) . ioError . userError $
      "accept: can't perform accept on socket (" ++ show (family,sockType,proto) ++ ") in status " ++ show currentStatus
    case cToEnum proto of
         L2CAP  -> alloca $ \(sockaddrPtr :: Ptr SockAddrL2CAP) ->
           callAccept sock sockaddrPtr $ sizeOf (undefined :: SockAddrL2CAP)
         RFCOMM -> alloca $ \(sockaddrPtr :: Ptr SockAddrRFCOMM) ->
           callAccept sock sockaddrPtr $ sizeOf (undefined :: SockAddrRFCOMM)
  where
    callAccept :: SockAddrBluetooth a => Socket -> Ptr a -> Int -> IO (Socket, BluetoothAddr)
    callAccept (MkSocket fd _ _ _ _) sockaddrPtr size = do
          (newFd,_) <- throwErrnoIf ((== -1) . fst) "accept" $ c_accept fd sockaddrPtr size
          bdaddr <- fmap sockAddrBluetoothAddr $ peek sockaddrPtr
          newStatus <- newMVar Connected
          return (MkSocket newFd family sockType proto newStatus, bdaddr)

-- bluetoothConnect :: Socket -> BluetoothAddr -> Int -> IO ()
-- bluetoothConnect (MkSocket fd _ _ proto sockStatus) bdaddr portNum = do
--     modifyMVar sockStatus $ \status -> do
--         when (status /= NotConnected) . ioError . userError $
--           "accept: can't peform connect on socket in status " ++ show status
--         let protoEnum = cToEnum proto
--         btPort <- mkPort protoEnum bdaddr portNum
--         case protoEnum of
--              L2CAP  -> 
--              RFCOMM -> 
--   where
--     callConnect :: SockAddrPtr p => Int

bluetoothConnect :: Socket -> BluetoothAddr -> BluetoothPort -> IO ()
bluetoothConnect = assignSocket c_connect "connect"
-- bluetoothConnect sock@(MkSocket _ _ _ proto _) =
--   let connect' = case cToEnum proto of
--                       L2CAP  -> assignSocket {#sizeof sockaddr_l2_t #}
--                                              {#set sockaddr_l2_t.l2_family #}
--                                              c_sockaddr_l2_set_bdaddr
--                                              {#set sockaddr_l2_t.l2_psm #}
--                                              c_connect
--                       RFCOMM -> assignSocket {#sizeof sockaddr_rc_t #}
--                                              {#set sockaddr_rc_t.rc_family #}
--                                              c_sockaddr_rc_set_bdaddr
--                                              {#set sockaddr_rc_t.rc_channel #}
--                                              c_connect
--   in connect' "connect" sock     

bluetoothSocketPort :: Socket -> IO BluetoothPort
bluetoothSocketPort sock@(MkSocket _ _ _ proto status) = do
    currentStatus <- readMVar status
    when (currentStatus == NotConnected || currentStatus == Closed) . ioError . userError $
      "getsockname: can't get name of socket in status " ++ show currentStatus
    case cToEnum proto of
         L2CAP -> alloca $ \(sockaddrPtr :: Ptr SockAddrL2CAP) ->
           callGetSockName sock sockaddrPtr $ sizeOf (undefined :: SockAddrL2CAP)
         RFCOMM -> alloca $ \(sockaddrPtr :: Ptr SockAddrRFCOMM) ->
           callGetSockName sock sockaddrPtr $ sizeOf (undefined :: SockAddrRFCOMM)
  where
    callGetSockName :: SockAddrBluetooth a => Socket -> Ptr a -> Int -> IO BluetoothPort
    callGetSockName (MkSocket fd _ _ _ _) sockaddrPtr size = do
        throwErrnoIf_ ((== -1) . fst) "getsockname" $ c_getsockname fd sockaddrPtr size
        fmap sockAddrPort $ peek sockaddrPtr
--          L2CAP  -> callGetSockName {#sizeof sockaddr_l2_t #}
--                                    ({#get sockaddr_l2_t.l2_psm #} :: SockAddrL2CAPPtr -> IO CUShort)
--                                    L2CAPPort
--          RFCOMM -> callGetSockName {#sizeof sockaddr_rc_t #}
--                                    ({#get sockaddr_rc_t.rc_channel #} :: SockAddrRFCOMMPtr -> IO {#type uint8_t #})
--                                    RFCOMMPort
--   where
--     callGetSockName :: (Integral i, Integral j, SockAddrBluetooth p) =>
--                        Int
--                     -> (Ptr p -> IO i)
--                     -> (j -> BluetoothPort)
--                     -> IO BluetoothPort
--     callGetSockName size getter portCon = allocaBytes size $ \sockaddrPtr -> do
--         throwErrnoIf_ ((== -1) . fst) "getsockname" $ c_getsockname fd sockaddrPtr size
--         fmap portCon $ getFromIntegral getter sockaddrPtr 

{#fun unsafe socket as c_socket
  { packFamily     `Family'
  , packSocketType `SocketType'
  , cFromEnum      `BluetoothProtocol'
  }             -> `CInt' id #}

{#fun unsafe bind as c_bind
  `SockAddrBluetooth a' =>
  { id      `CInt'
  , castPtr `Ptr a'
  ,         `Int'
  }      -> `Int' #}

{#fun unsafe accept as c_accept
  `SockAddrBluetooth a' =>
  { id      `CInt'
  , castPtr `Ptr a'
  ,         `Int'  peekFromIntegral*
  }      -> `CInt' id #}

{#fun unsafe connect as c_connect
  `SockAddrBluetooth a' =>
  { id      `CInt'
  , castPtr `Ptr a'
  ,         `Int'
  }      -> `Int' #}

{#fun unsafe getsockname as c_getsockname
  `SockAddrBluetooth a' =>
  { id      `CInt'
  , castPtr `Ptr a'
  ,         `Int'  peekFromIntegral*
  }      -> `CInt' id #}

{#fun pure wr_htobs as c_htobs
  `Integral i' =>
  { fromIntegral `i'
  }           -> `i' fromIntegral #}