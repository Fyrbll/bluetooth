{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Bluetooth.Linux.Addr where

import           Control.Applicative

import           Data.Binary

import           Foreign.Storable

import           Network.Info

import           Text.Read
import qualified Text.Read.Lex as Rex
import qualified Text.ParserCombinators.ReadPrec as Rex
import qualified Text.ParserCombinators.ReadP as Rex

#include <stdint.h>
#include "wr_bluetooth.h"

newtype BluetoothAddr = BluetoothAddr MAC
  deriving (Eq, Ord, Bounded, Binary, Storable)

fromWords :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> BluetoothAddr
fromWords a b c d e f = BluetoothAddr $ MAC a b c d e f

toWords :: BluetoothAddr -> (Word8, Word8, Word8, Word8, Word8, Word8)
toWords (BluetoothAddr (MAC a b c d e f)) = (a, b, c, d, e, f)

instance Show BluetoothAddr where
    showsPrec d (BluetoothAddr macAddr) = showsPrec d macAddr

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