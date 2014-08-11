module Utils (commentate) where

import System.IO

commentate :: String -> IO a -> IO a
commentate str io = do
    putStr $ str ++ "... "
    hFlush stdout
    a <- io
    putStrLn "done."
    return a