{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Bluetooth.Linux.Addr (
      BluetoothAddr
    , BluetoothAddrArray
    , asArray
    , fromArray
    , fromWords
    , toWords
    ) where

import           Control.Applicative
import           Control.Monad

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

type BluetoothAddrArray = Ptr {#type uint8_t #}

asArray :: BluetoothAddr -> (BluetoothAddrArray -> IO a) -> IO a
asArray (BluetoothAddr (MAC a b c d e f)) = withArray $ map fromIntegral [a,b,c,d,e,f]

fromArray :: BluetoothAddrArray -> IO BluetoothAddr
fromArray arr = do
    [a,b,c,d,e,f] <- map fromIntegral <$> peekArray 6 arr
    return . BluetoothAddr $ MAC a b c d e f

fromWords :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> BluetoothAddr
fromWords a b c d e f = BluetoothAddr $ MAC a b c d e f

toWords :: BluetoothAddr -> (Word8, Word8, Word8, Word8, Word8, Word8)
toWords (BluetoothAddr (MAC a b c d e f)) = (a, b, c, d, e, f) 

instance Show BluetoothAddr where
    show (BluetoothAddr macAddr) = show macAddr

instance Storable BluetoothAddr where
    sizeOf = const {#sizeof bdaddr_t #}
    alignment = const {#alignof bdaddr_t #}
    peek = {#get bdaddr_t.b #} >=> fromArray
    poke = flip asArray . {#set bdaddr_t.b #}

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