module Main where

import Control.Exception

import Network.Bluetooth
import Network.Bluetooth.UUID
import Network.Socket

import System.IO

commentate :: String -> IO a -> IO a
commentate str io = do
    putStr $ str ++ "... "
    hFlush stdout
    a <- io
    putStrLn "done."
    return a

main :: IO ()
main = withSocketsDo $ do
    let port     = 0 -- any port
        backlog  = 1
        uuid     = serialPortServiceClassUUID
        settings = SDPAttributes "Roto-Rooter Data Router" "Roto-Rooter" "An experimental plumbing router"
        messLen  = 4096
    handshakeSock <- commentate "Calling socket" $ bluetoothSocket RFCOMM
    btPort <- commentate "Calling bind" $ bluetoothBind handshakeSock anyAddr port
    
    putStrLn $ "Bound on port " ++ show btPort
    btPort2 <- getSockPort handshakeSock
    return $ assert (btPort == btPort2) ()
    
    commentate ("Calling listen with backlog " ++ show backlog) $
      bluetoothListen handshakeSock backlog
    service <- commentate ("Registering SDP service " ++ show uuid) $
      registerSDPService uuid settings btPort
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