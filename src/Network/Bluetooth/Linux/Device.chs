{-# LANGUAGE ForeignFunctionInterface #-}
module Network.Bluetooth.Linux.Device (
      BluetoothDevice(..)
    ) where

import Control.Applicative

import Data.Word

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Network.Bluetooth.Linux.Addr
import Network.Bluetooth.Utils

#include <bluetooth/bluetooth.h>
#include "wr_bluetooth.h"
#include "wr_hci.h"

data BluetoothDevice = BluetoothDevice {
    btDeviceId   :: Word16
  , btDeviceName :: String
  , btDeviceAddr :: BluetoothAddr
} deriving (Read, Ord, Eq, Show)

instance Storable BluetoothDevice where
    sizeOf = const {#sizeof hci_dev_info_t #}
    alignment = const {#alignof hci_dev_info_t #}
    peek p = BluetoothDevice
      <$> getFromIntegral {#get hci_dev_info_t.dev_id #} p
      <*> c_hci_dev_info_get_name p
      <*> c_hci_dev_info_get_bdaddr p
    poke p (BluetoothDevice devId devName devAddr) =
         setFromIntegral {#set hci_dev_info.dev_id #} p devId
      *> c_hci_dev_info_set_name p devName
      *> c_hci_dev_info_set_bdaddr p devAddr

{#pointer *hci_dev_info_t as BluetoothDevicePtr -> BluetoothDevice #}
{#pointer *wr_bdaddr_t    as BluetoothAddrPtr   -> BluetoothAddr   #}

{#fun unsafe wr_hci_dev_info_get_bdaddr as c_hci_dev_info_get_bdaddr
  { alloca- `BluetoothAddr'
  ,         `BluetoothDevicePtr'
  }      -> `BluetoothAddr' peek* #}

{#fun unsafe wr_hci_dev_info_set_bdaddr as c_hci_dev_info_set_bdaddr
  {           `BluetoothDevicePtr'
  , withCast* `BluetoothAddr'
  }        -> `()' #}

{#fun unsafe wr_hci_dev_info_get_name as c_hci_dev_info_get_name
  { allocaArray8- `CString'
  ,               `BluetoothDevicePtr'
  }            -> `String' peekCString* #}
  where
    allocaArray8 :: Storable a => (Ptr a -> IO b) -> IO b
    allocaArray8 = allocaArray 8

{#fun unsafe wr_hci_dev_info_set_name as c_hci_dev_info_set_name
  {              `BluetoothDevicePtr'
  , withCString* `String'
  }           -> `()' #}