module Main where

import Network.Bluetooth
import Network.Socket

import System.Environment

import Utils

main :: IO ()
main = getArgs >>= \args -> case args of
  addr:port:_ -> withSocketsDo $ client (read addr) (read port)
  _           -> printUsage

client :: BluetoothAddr -> BluetoothPort -> IO ()
client addr port = do
    let message = "Hello, World!"
        respLen = 4096
    
    sock <- commentate "Calling socket" $ bluetoothSocket RFCOMM
    commentate ("Calling connect on address " ++ show addr ++ " and port " ++ show port)
      $ bluetoothConnect sock addr port
    
    messBytes <- commentate ("Calling send with message [" ++ message ++ "]") $
      send sock message
    
    putStrLn $ "Sent message! " ++ show messBytes ++ " bytes."
    
    response <- commentate ("Calling recv with " ++ show respLen ++ " bytes") $
      recv sock respLen
    
    putStrLn $ "Received response! [" ++ response ++ "]"
    
    close sock

printUsage :: IO ()
printUsage = getProgName >>= putStrLn . usage

usage :: String -> String
usage name = unlines
  [ "usage: " ++ name ++ " <XX:XX:XX:XX:XX:XX> <port>"
  , "       where XX:XX:XX:XX:XX:XX is the remote Bluetooth address to which to connect"
  ]