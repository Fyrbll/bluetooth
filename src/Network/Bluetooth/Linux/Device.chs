{-# LANGUAGE ForeignFunctionInterface #-}
module Network.Bluetooth.Linux.Device (
      BluetoothDevice(..)
    , localDevices
    ) where

import Control.Applicative

import Data.Int
import Data.IORef
import Data.Ix
import Data.Word

import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Network.Bluetooth.Linux.Addr
import Network.Bluetooth.Utils

#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>
#include <bluetooth/hci_lib.h>
#include "wr_bluetooth.h"
#include "wr_hci.h"
#include "wr_ioctl.h"

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

localDevices :: IO [BluetoothDevice]
localDevices = do
    devsInfoRef <- newIORef []
    let visitor =  \dd devId _    ->
          alloca $ \hciDevInfoPtr -> do
              setFromIntegral {#set hci_dev_info.dev_id #} hciDevInfoPtr devId
              throwErrnoIfMinus1_ "ioctl" $ c_ioctl dd c_hci_get_dev_info hciDevInfoPtr
              hciDevInfo <- peek hciDevInfoPtr
              modifyIORef devsInfoRef (hciDevInfo:)
              return 0
    throwErrnoIfMinus1_ "hci_for_each_dev" $ c_hci_for_each_dev HCIUp visitor 0
    fmap reverse $ readIORef devsInfoRef

-------------------------------------------------------------------------------

type DeviceVisitor = CInt -> CInt -> CLong -> IO CInt

{#enum HCI_UP as HCIDeviceFlag
  { HCI_UP      as HCIUp
  , HCI_INIT    as HCIInit
  , HCI_RUNNING as HCIRunning
  , HCI_PSCAN   as HCI_PScan
  , HCI_ISCAN   as HCI_IScan
  , HCI_AUTH    as HCIAuth
  , HCI_ENCRYPT as HCIEncrypt
  , HCI_INQUIRY as HCIInquiry
  , HCI_RAW     as HCIRaw
  } deriving (Ix, Show, Eq, Read, Ord, Bounded) #}

-- {#enum define HCI_IOCtlRequest
--   { HCIDEVUP       as HCIDevUp
--   , HCIDEVDOWN     as HCIDevDown
--   , HCIDEVRESET    as HCIDevReset
--   , HCIDEVRESTAT   as HCIDevRestat
--   
--   , HCIGETDEVLIST  as HCIGetDevList
--   , HCIGETDEVINFO  as HCIGetDevInfo
--   , HCIGETCONNLIST as HCIGetConnList
--   , HCIGETCONNINFO as HCIGetConnInfo
--   , HCIGETAUTHINFO as HCIGetAuthInfo
--   
--   , HCISETRAW      as HCISetRaw
--   , HCISETSCAN     as HCISetScan
--   , HCISETAUTH     as HCISetAuth
--   , HCISETENCRYPT  as HCISetEncrypt
--   , HCISETPTYPE    as HCISetPType
--   , HCISETLINKPOL  as HCISetLinkPol
--   , HCISETLINKMODE as HCISetLinkMode
--   , HCISETACLMTU   as HCISetACL_MTU
--   , HCISETSCOMTU   as HCISetSCO_MTU
--   
--   , HCIBLOCKADDR   as HCIBlockAddr
--   , HCIUNBLOCKADDR as HCIUnblockAddr
--   
--   , HCIINQUIRY     as HCIInquiry
--   } deriving (Ix, Show, Eq, Read, Ord, Bounded) #}

{#pointer *hci_dev_info_t as BluetoothDevicePtr -> BluetoothDevice #}
{#pointer *wr_bdaddr_t    as BluetoothAddrPtr   -> BluetoothAddr   #}

foreign import ccall safe "wrapper"
  mkDeviceVisitor :: DeviceVisitor -> IO (FunPtr DeviceVisitor)

{#fun unsafe hci_for_each_dev as c_hci_for_each_dev
  {                    `HCIDeviceFlag'
  , withDeviceVisitor* `DeviceVisitor'
  ,                    `Int64'
  }                 -> `Int' #}
  where
    withDeviceVisitor :: DeviceVisitor -> (FunPtr DeviceVisitor -> IO b) -> IO b
    withDeviceVisitor = withFunWrapper mkDeviceVisitor

{#fun pure wr_hci_get_dev_info as c_hci_get_dev_info {} -> `Int' #}

{#fun unsafe wr_ioctl as c_ioctl
  { id      `CInt'
  ,         `Int'
  , castPtr `Ptr a'
  }      -> `Int' #}

{#fun unsafe wr_hci_dev_info_get_bdaddr as c_hci_dev_info_get_bdaddr
  { alloca- `BluetoothAddr'
  ,         `BluetoothDevicePtr'
  }      -> `BluetoothAddr' peek* #}

{#fun unsafe wr_hci_dev_info_set_bdaddr as c_hci_dev_info_set_bdaddr
  {        `BluetoothDevicePtr'
  , with'* `BluetoothAddr'
  }     -> `()' #}

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