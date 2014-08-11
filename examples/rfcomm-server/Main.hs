module Main where

import Control.Exception

import Data.Set

import Network.Bluetooth
import Network.Socket

import Utils

main :: IO ()
main = withSocketsDo $ do
    let backlog  = 1
        proto    = RFCOMM
        uuid     = serviceClassToUUID SerialPort -- TODO: Change to better UUID
        settings = defaultSDPInfo {
            sdpServiceName    = Just "Roto-Rooter Data Router"
          , sdpProviderName   = Just "Roto-Rooter"
          , sdpDescription    = Just "An experimental plumbing router"
          , sdpServiceClasses = singleton SerialPort
          , sdpProfiles       = singleton SerialPort
        }
        messLen  = 4096
    handshakeSock <- commentate "Calling socket" $ bluetoothSocket proto
    btPort <- commentate "Calling bind" $ bluetoothBindAnyPort handshakeSock anyAddr
    
    putStrLn $ "Bound on port " ++ show btPort
    btPort2 <- bluetoothSocketPort handshakeSock
    return $ assert (btPort == btPort2) ()
    
    commentate ("Calling listen with backlog " ++ show backlog) $
      bluetoothListen handshakeSock backlog
    service <- commentate ("Registering SDP service " ++ show uuid) $
      registerSDPService uuid settings proto btPort
    (connSock, connAddr) <- commentate "Calling accept" $
      bluetoothAccept handshakeSock
      
    putStrLn $ "Established connection with address " ++ show connAddr
    
    message <- commentate ("Calling recv with " ++ show messLen ++ " bytes") $
      recv connSock messLen
      
    putStrLn $ "Received message! [" ++ message ++ "]"
    let response = reverse message
    
    respBytes <- commentate ("Calling send with response [" ++ response ++ "]") $
      send connSock response
    
    putStrLn $ "Sent response! " ++ show respBytes ++ " bytes."
    
    close connSock
    close handshakeSock
    closeSDPService service