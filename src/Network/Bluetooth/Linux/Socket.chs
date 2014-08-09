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

bluetoothBind :: Socket -> BluetoothAddr -> BluetoothPort -> IO ()
bluetoothBind = assignSocket c_bind "bind"

bluetoothBindAnyPort :: Socket -> BluetoothAddr -> IO BluetoothPort
bluetoothBindAnyPort sock@(MkSocket _ _ _ proto _) bdaddr = do
    port <- bindAnyPort (cToEnum proto) bdaddr
    bluetoothBind sock bdaddr $ fromIntegral port
    return port

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

bluetoothConnect :: Socket -> BluetoothAddr -> BluetoothPort -> IO ()
bluetoothConnect = assignSocket c_connect "connect"

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

-------------------------------------------------------------------------------

type SockLen = {#type socklen_t #}

assignSocket :: (forall a. SockAddrBluetooth a => CInt -> a -> IO Int)
             -> String
             -> Socket -> BluetoothAddr -> BluetoothPort -> IO ()
assignSocket c_assigner name (MkSocket fd _ _ proto sockStatus) bdaddr portNum =
    modifyMVar_ sockStatus $ \status -> do
        when (status /= NotConnected) . ioError . userError $
          name ++ ": can't peform " ++ name ++ " on socket in status " ++ show status
        let protoEnum = cToEnum proto
        unless (isBluetoothPortValid protoEnum portNum) . throwIO $ BluetoothPortException protoEnum portNum
        case protoEnum of
             L2CAP  -> callAssign $ SockAddrL2CAP AF_BLUETOOTH (c_htobs portNum) bdaddr
             RFCOMM -> callAssign . SockAddrRFCOMM AF_BLUETOOTH bdaddr $ fromIntegral portNum
        return Bound
  where
    callAssign :: SockAddrBluetooth a => a -> IO ()
    callAssign sockaddr = throwErrnoIfMinus1_ name $ c_assigner fd sockaddr

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

{#fun unsafe socket as c_socket
  { packFamily     `Family'
  , packSocketType `SocketType'
  , cFromEnum      `BluetoothProtocol'
  }             -> `CInt' id #}

{#fun unsafe bind as c_bind
  `SockAddrBluetooth a' =>
  { id               `CInt'
  , withCastLenConv* `a'&
  }               -> `Int' #}

{#fun unsafe accept as c_accept
  `SockAddrBluetooth a' =>
  { id      `CInt'
  , castPtr `Ptr a'
  ,         `Int'  peekFromIntegral*
  }      -> `CInt' id #}

{#fun unsafe connect as c_connect
  `SockAddrBluetooth a' =>
  { id               `CInt'
  , withCastLenConv* `a'&
  }               -> `Int' #}

{#fun unsafe getsockname as c_getsockname
  `SockAddrBluetooth a' =>
  { id      `CInt'
  , castPtr `Ptr a'
  ,         `Int'  peekFromIntegral*
  }          -> `CInt' id #}

{#fun pure wr_htobs as c_htobs
  `Integral i' =>
  { fromIntegral `i'
  }           -> `i' fromIntegral #}