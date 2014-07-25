{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Bluetooth.Linux.Types (
      BluetoothAddr
    , fromWords
    , toWords
    ) where

import           Control.Applicative

import           Data.Binary

import           Foreign.C.Types
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable

import           Network.Info

import           Text.Read
import qualified Text.Read.Lex as Rex
import qualified Text.ParserCombinators.ReadPrec as Rex
import qualified Text.ParserCombinators.ReadP as Rex

#include <bluetooth/bluetooth.h>

newtype BluetoothAddr = BluetoothAddr MAC
  deriving (Eq, Ord, Bounded, Binary)

fromWords :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> BluetoothAddr
fromWords a b c d e f = BluetoothAddr $ MAC a b c d e f

toWords :: BluetoothAddr -> (Word8, Word8, Word8, Word8, Word8, Word8)
toWords (BluetoothAddr (MAC a b c d e f)) = (a, b, c, d, e, f) 

instance Show BluetoothAddr where
    show (BluetoothAddr macAddr) = show macAddr

instance Storable BluetoothAddr where
    sizeOf _ = {#sizeof bdaddr_t #}
    alignment _ = {#alignof bdaddr_t #}
    peek p = do
        arr <- {#get bdaddr_t.b #} p
        [a,b,c,d,e,f] <- map fromIntegral <$> peekArray 6 arr
        return . BluetoothAddr $ MAC a b c d e f
    poke p (BluetoothAddr (MAC a b c d e f)) =
      withArray (map fromIntegral [a,b,c,d,e,f]) $ {#set bdaddr_t.b #} p

-------------------------------------------------------------------------------
-- The following instances come from the maccatcher package, licensed under the
-- BSD3 license.

instance Read BluetoothAddr where
    readPrec =  Rex.lift $ do
        a               <-  Rex.readHexP
        [b, c, d, e, f] <-  Rex.many $ Rex.char ':' *> Rex.readHexP
        return . BluetoothAddr $ MAC a b c d e f

instance Binary MAC where
    -- Thanks to aslatter@gmail.com for this instance.
    put (MAC a b c d e f) =
         putWord8 a
      *> putWord8 b
      *> putWord8 c
      *> putWord8 d
      *> putWord8 e
      *> putWord8 f
    get = MAC
      <$> getWord8
      <*> getWord8
      <*> getWord8
      <*> getWord8
      <*> getWord8
      <*> getWord8