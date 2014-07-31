module Main where

import Network.Bluetooth
import Network.Bluetooth.UUID
import Network.Socket

commentate :: String -> IO a -> IO a
commentate str io = do
    putStr $ str ++ "... "
    a <- io
    putStrLn "done."
    return a

main :: IO ()
main = withSocketsDo $ do
    let port     = 3
        backlog  = 1
        uuid     = serialPortServiceClassUUID
        settings = SDPAttributes "Roto-Rooter Data Router" "Roto-Rooter" "An experimental plumbing router"
        messLen  = 4096
    handshakeSock <- commentate "Calling socket" $
      bluetoothSocket RFCOMM
    commentate ("Calling bind on port " ++ show port) $
      bluetoothBind handshakeSock anyAddr port
    commentate ("Calling listen with backlog " ++ show backlog) $
      bluetoothListen handshakeSock backlog
    service <- commentate ("Registering SDP service " ++ show uuid) $
      registerSDPService uuid settings port
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