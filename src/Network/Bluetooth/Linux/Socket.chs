{-# LANGUAGE ScopedTypeVariables #-}
module Network.Bluetooth.Linux.Socket (
      btSocket
    , btBind
    , btBindAnyPort
    , btListen
    , btAccept
    , btConnect
    , btSocketInfo
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad

import Data.Either

import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Network.Bluetooth.Exception
import Network.Bluetooth.Linux.Addr
import Network.Bluetooth.Linux.Protocol
import Network.Bluetooth.Utils
import Network.Socket
import Network.Socket.Internal

import System.Posix.Internals (setNonBlockingFD)

#include <bluetooth/bluetooth.h>
#include <sys/socket.h>
#include "wr_l2cap.h"
#include "wr_rfcomm.h"

btSocket :: BluetoothProtocol -> IO Socket
btSocket proto = socket AF_BLUETOOTH sockType protoNum
  where
    sockType = case proto of
        L2CAP  -> SeqPacket
        RFCOMM -> Stream
    protoNum = cFromEnum proto

btBind :: Socket -> BluetoothAddr -> BluetoothPort -> IO ()
btBind (MkSocket fd _ _ proto sockStatus) bdaddr portNum =
    modifyMVar_ sockStatus $ \status -> do
        when (status /= NotConnected) . ioError . userError $
          "bind: can't peform bind on socket in status " ++ show status
        let protoEnum = cToEnum proto
        unless (isBluetoothPortValid protoEnum portNum) . throwIO $ BluetoothPortException protoEnum portNum
        case protoEnum of
             L2CAP  -> callBind $ SockAddrL2CAP AF_BLUETOOTH (c_htobs portNum) bdaddr
             RFCOMM -> callBind . SockAddrRFCOMM AF_BLUETOOTH bdaddr $ fromIntegral portNum
        return Bound
  where
    callBind :: SockAddrBluetooth a => a -> IO ()
    callBind sockAddr = throwSocketErrorIfMinus1Retry_ "bind" $ c_bind fd sockAddr

btBindAnyPort :: Socket -> BluetoothAddr -> IO BluetoothPort
btBindAnyPort sock@(MkSocket _ _ _ proto _) bdaddr = do
    port <- bindAnyPort (cToEnum proto) bdaddr
    btBind sock bdaddr $ fromIntegral port
    return port

btListen :: Socket -> Int -> IO ()
btListen = listen

btAccept :: Socket -> IO (Socket, BluetoothAddr)
btAccept sock@(MkSocket _ family sockType proto sockStatus) = do
    currentStatus <- readMVar sockStatus
    when (currentStatus /= Connected && currentStatus /= Listening) . ioError . userError $
      "accept: can't perform accept on socket (" ++ show (family,sockType,proto) ++ ") in status " ++ show currentStatus
    case cToEnum proto of
         L2CAP  -> allocaLen $ \sockLen (sockAddrPtr :: Ptr SockAddrL2CAP) ->
           callAccept sock sockAddrPtr sockLen
         RFCOMM -> allocaLen $ \sockLen (sockAddrPtr :: Ptr SockAddrRFCOMM) ->
           callAccept sock sockAddrPtr sockLen
  where
    callAccept :: SockAddrBluetooth a => Socket -> Ptr a -> Int -> IO (Socket, BluetoothAddr)
    callAccept sock'@(MkSocket fd _ _ _ _) sockAddrPtr size = do
          newFd <- throwSocketErrorWaitRead sock' "accept" . fmap fst $ c_accept fd sockAddrPtr size
          setNonBlockingFD newFd True
          bdaddr <- fmap sockAddrBluetoothAddr $ peek sockAddrPtr
          newStatus <- newMVar Connected
          return (MkSocket newFd family sockType proto newStatus, bdaddr)

btConnect :: Socket -> BluetoothAddr -> BluetoothPort -> IO ()
btConnect sock@(MkSocket fd _ _ proto sockStatus) bdaddr portNum =
    modifyMVar_ sockStatus $ \status -> do
        when (status /= NotConnected && status /= Bound) . ioError . userError $
            "connect: can't peform connect on socket in status " ++ show status
        let protoEnum = cToEnum proto
        unless (isBluetoothPortValid protoEnum portNum) . throwIO $ BluetoothPortException protoEnum portNum
        
        let connectLoop :: IO ()
            connectLoop = do
                r <- case protoEnum of
                    L2CAP  -> c_connect fd $ SockAddrL2CAP AF_BLUETOOTH (c_htobs portNum) bdaddr
                    RFCOMM -> c_connect fd . SockAddrRFCOMM AF_BLUETOOTH bdaddr $ fromIntegral portNum
                when (r == -1) $ do
                    err <- getErrno
                    case () of
                        _ | err == eINTR       -> connectLoop
                        _ | err == eINPROGRESS -> connectBlocked
                        _otherwise             -> throwSocketError "connect"
            
            connectBlocked :: IO ()
            connectBlocked = do
                threadWaitWrite $ fromIntegral fd
                err <- getSocketOption sock SoError
                unless (err == 0) . throwSocketErrorCode "connect" $ fromIntegral err
        
        connectLoop
        return Connected

btSocketInfo :: Socket -> IO (BluetoothAddr, BluetoothPort)
btSocketInfo sock@(MkSocket _ _ _ proto _) = do
    case cToEnum proto of
         L2CAP -> allocaLen $ \sockLen (sockAddrPtr :: Ptr SockAddrL2CAP) ->
           callGetSockName sock sockAddrPtr sockLen
         RFCOMM -> allocaLen $ \sockLen (sockAddrPtr :: Ptr SockAddrRFCOMM) ->
           callGetSockName sock sockAddrPtr sockLen
  where
    callGetSockName :: SockAddrBluetooth a => Socket -> Ptr a -> Int -> IO (BluetoothAddr, BluetoothPort)
    callGetSockName (MkSocket fd _ _ _ _) sockAddrPtr size = do
        throwSocketErrorIfMinus1Retry_ "getsockname" . fmap fst $ c_getsockname fd sockAddrPtr size
        sockAddr <- peek sockAddrPtr
        return (sockAddrBluetoothAddr sockAddr, sockAddrPort sockAddr)

-------------------------------------------------------------------------------

bindAnyPort :: BluetoothProtocol -> BluetoothAddr -> IO BluetoothPort
bindAnyPort proto addr = do
    avails <- flip filterM (portRange proto) $ \portNum -> do
        sock <- btSocket proto
        res  <- try $ btBind sock addr portNum
        close sock
        return $ isRight (res :: Either IOError ())
    case avails of
        portNum:_ -> return portNum
        _         -> ioError $ userError "Unable to find any available port"
  where
    portRange L2CAP  = [4097, 4099 .. 32767]
    portRange RFCOMM = [1 .. 30]

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