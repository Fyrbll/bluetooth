{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Bluetooth.Linux.Addr (
      BluetoothAddr
    , fromWords
    , toWords
    , anyAddr
    , localAddr
    , SockAddrBluetooth(..)
    , SockAddrL2CAP(..)
    , SockAddrRFCOMM(..)
    ) where

import           Control.Applicative

import           Data.Binary

import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable

import           Network.Bluetooth.Linux.Protocol
import           Network.Bluetooth.Utils
import           Network.Info
import           Network.Socket

import           Text.Read
import qualified Text.Read.Lex as Rex
import qualified Text.ParserCombinators.ReadPrec as Rex
import qualified Text.ParserCombinators.ReadP as Rex

#include <stdint.h>
#include "wr_bluetooth.h"
#include "wr_l2cap.h"
#include "wr_rfcomm.h"

newtype BluetoothAddr = BluetoothAddr MAC
  deriving (Eq, Ord, Bounded, Binary, Storable)

fromWords :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> BluetoothAddr
fromWords a b c d e f = BluetoothAddr $ MAC a b c d e f

toWords :: BluetoothAddr -> (Word8, Word8, Word8, Word8, Word8, Word8)
toWords (BluetoothAddr (MAC a b c d e f)) = (a, b, c, d, e, f)

anyAddr :: BluetoothAddr
anyAddr = unsafePeek c_bdaddr_any

localAddr :: BluetoothAddr
localAddr = unsafePeek c_bdaddr_local

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

-------------------------------------------------------------------------------

data SockAddrL2CAP = SockAddrL2CAP {
    l2capFamily        :: Family
  , l2capPsm           :: Word16
  , l2capBluetoothAddr :: BluetoothAddr
} deriving (Read, Ord, Show, Eq)

instance Storable SockAddrL2CAP where
    sizeOf = const {#sizeof sockaddr_l2_t #}
    alignment = const {#alignof sockaddr_l2_t #}
    peek p = SockAddrL2CAP
      <$> (unpackFamily <$> getFromIntegral {#get sockaddr_l2_t.l2_family #} p)
      <*> getFromIntegral {#get sockaddr_l2_t.l2_psm #} p
      <*> c_sockaddr_l2_get_bdaddr p
    poke p (SockAddrL2CAP family psm bdaddr) =
         (setFromIntegral {#set sockaddr_l2_t.l2_family #} p $ packFamily family)
      *> setFromIntegral {#set sockaddr_l2_t.l2_psm #} p psm
      *> c_sockaddr_l2_set_bdaddr p bdaddr

data SockAddrRFCOMM = SockAddrRFCOMM {
    rfcommFamily        :: Family
  , rfcommBluetoothAddr :: BluetoothAddr
  , rfcommChannel       :: Word8
} deriving (Read, Ord, Show, Eq)

instance Storable SockAddrRFCOMM where
    sizeOf = const {#sizeof sockaddr_rc_t #}
    alignment = const {#alignof sockaddr_rc_t #}
    peek p = SockAddrRFCOMM
      <$> (unpackFamily <$> getFromIntegral {#get sockaddr_rc_t.rc_family #} p)
      <*> c_sockaddr_rc_get_bdaddr p
      <*> getFromIntegral {#get sockaddr_rc_t.rc_channel #} p
    poke p (SockAddrRFCOMM family bdaddr channel) =
         (setFromIntegral {#set sockaddr_rc_t.rc_family #} p $ packFamily family)
      *> c_sockaddr_rc_set_bdaddr p bdaddr
      *> setFromIntegral {#set sockaddr_rc_t.rc_channel #} p channel

class Storable a => SockAddrBluetooth a where
    sockAddrFamily :: a -> Family
    sockAddrBluetoothAddr :: a -> BluetoothAddr
    sockAddrPort :: a -> BluetoothPort

instance SockAddrBluetooth SockAddrL2CAP where
    sockAddrFamily = l2capFamily
    sockAddrBluetoothAddr = l2capBluetoothAddr
    sockAddrPort = l2capPsm

instance SockAddrBluetooth SockAddrRFCOMM where
    sockAddrFamily = rfcommFamily
    sockAddrBluetoothAddr = rfcommBluetoothAddr
    sockAddrPort = fromIntegral . rfcommChannel

{#pointer *wr_bdaddr_t   as BluetoothAddrPtr  -> BluetoothAddr #}
{#pointer *sockaddr_l2_t as SockAddrL2CAPPtr  -> SockAddrL2CAP #}
{#pointer *sockaddr_rc_t as SockAddrRFCOMMPtr -> SockAddrRFCOMM #}

{#fun unsafe wr_sockaddr_l2_get_bdaddr as c_sockaddr_l2_get_bdaddr
  { alloca- `BluetoothAddr'
  ,         `SockAddrL2CAPPtr'
  }      -> `BluetoothAddr' peek* #}

{#fun unsafe wr_sockaddr_l2_set_bdaddr as c_sockaddr_l2_set_bdaddr
  {        `SockAddrL2CAPPtr'
  , with'* `BluetoothAddr'
  }     -> `()' #}

{#fun unsafe wr_sockaddr_rc_get_bdaddr as c_sockaddr_rc_get_bdaddr
  { alloca- `BluetoothAddr'
  ,         `SockAddrRFCOMMPtr'
  }      -> `BluetoothAddr' peek* #}

{#fun unsafe wr_sockaddr_rc_set_bdaddr as c_sockaddr_rc_set_bdaddr
  {        `SockAddrRFCOMMPtr'
  , with'* `BluetoothAddr'
  }     -> `()' #}

{#fun pure wr_bdaddr_any as c_bdaddr_any {} -> `BluetoothAddrPtr' #}

{#fun pure wr_bdaddr_local as c_bdaddr_local {} -> `BluetoothAddrPtr' #}