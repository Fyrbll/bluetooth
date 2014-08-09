module Network.Bluetooth.Linux.SDP where

import           Control.Monad

import           Data.Ix
import qualified Data.UUID as U
import           Data.UUID (UUID)

import           Foreign.C.Error
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable

import           Network.Bluetooth.Linux.Addr
import           Network.Bluetooth.Linux.Protocol
import           Network.Bluetooth.Utils

#include "wr_bluetooth.h"
#include "wr_sdp.h"
#include "wr_sdp_lib.h"

registerSDPService :: UUID -> SDPInfo -> BluetoothPort -> IO SDPSessionPtr
registerSDPService uuid info port = do
    let uuidSize              = {#sizeof uuid_t #}
        (w1,w2,w3,w4)         = U.toWords uuid
        sdpUUID16Create uuid' = throwErrnoIfNull_ "sdp_uuid16_create" . c_sdp_uuid16_create uuid'
        sdpListAppend list    = throwErrnoIfNull "sdp_list_append" . c_sdp_list_append list
        sdpListAppend_ list   = void . sdpListAppend list
    
    allocaBytes   uuidSize                       $ \rootUuid     ->
      allocaBytes uuidSize                       $ \l2capUuid    ->
      allocaBytes uuidSize                       $ \rfcommUuid   ->
      allocaBytes uuidSize                       $ \svcUuid      ->
      allocaBytes uuidSize                       $ \svcClassUuid ->
      allocaBytes {#sizeof sdp_profile_desc_t #} $ \profile      ->
      allocaBytes {#sizeof sdp_session_t #}      $ \record       -> do
          withArray (map byteSwap32 [w1,w2,w3,w4]) $
            throwErrnoIfNull_ "sdp_uuid128_create" . c_sdp_uuid128_create svcUuid
          c_sdp_set_service_id record svcUuid
          
          sdpUUID16Create svcClassUuid SerialPortServiceClassID
          svcClassList <- sdpListAppend nullPtr svcClassUuid
          throwErrnoIfNegative_ "sdp_set_service_classes" $
            c_sdp_set_service_classes record svcClassList
          
          sdpUUID16Create (c_sdp_profile_desc_get_uuid profile) SerialPortProfileID
          {#set sdp_profile_desc_t.version #} profile 0x0100
          profileList <- sdpListAppend nullPtr profile
          throwErrnoIfNegative_ "sdp_set_profile_descs" $
            c_sdp_set_profile_descs record profileList
          
          sdpUUID16Create rootUuid PublicBrowseGroup
          rootList <- sdpListAppend nullPtr rootUuid
          throwErrnoIfNegative_ "sdp_set_browse_groups" $
            c_sdp_set_browse_groups record rootList
          
          sdpUUID16Create l2capUuid L2CAP_UUID
          l2capList <- sdpListAppend nullPtr l2capUuid
          protoList <- sdpListAppend nullPtr l2capList
          
          sdpUUID16Create rfcommUuid RFCOMM_UUID
          channel <- with port $ c_sdp_data_alloc SDPCUInt8
          rfcommList <- sdpListAppend nullPtr rfcommUuid
          sdpListAppend_ rfcommList channel
          sdpListAppend_ protoList rfcommList
          
          accessProtoList <- sdpListAppend nullPtr protoList
          throwErrnoIfNegative_ "sdp_set_access_protos" $
            c_sdp_set_access_protos record accessProtoList
          
          case info of
               (SDPAttributes sn pn d) -> c_sdp_set_info_attr record sn pn d
               SDPNoInfo               -> return ()
          
          session <- throwErrnoIfNull "sdp_connect" $
            c_sdp_connect anyAddr localAddr SDPRetryIfBusy
          throwErrnoIfMinus1_ "sdp_record_register" $
            c_sdp_record_register session record 0
          
          c_sdp_data_free channel
          c_sdp_list_free l2capList nullFunPtr
          c_sdp_list_free rfcommList nullFunPtr
          c_sdp_list_free rootList nullFunPtr
          c_sdp_list_free accessProtoList nullFunPtr
          c_sdp_list_free svcClassList nullFunPtr
          c_sdp_list_free profileList nullFunPtr
          return session

closeSDPService :: SDPSessionPtr -> IO ()
closeSDPService = throwErrnoIfMinus1_ "sdp_close" . c_sdp_close

data SDPInfo = SDPAttributes {
                 serviceName  :: String
               , providerName :: String
               , description  :: String
             }
             | SDPNoInfo
  deriving (Read, Show)

type SDPFreeFunPtr = {#type sdp_free_func_t #}

{#enum define ProtocolUUID {
    RFCOMM_UUID as RFCOMM_UUID
  , L2CAP_UUID  as L2CAP_UUID
  } deriving (Ix, Show, Eq, Read, Ord, Bounded) #}

{#enum define ServiceClassID {
    PUBLIC_BROWSE_GROUP    as PublicBrowseGroup
  , SERIAL_PORT_SVCLASS_ID as SerialPortServiceClassID
  } deriving (Ix, Show, Eq, Read, Ord, Bounded) #}

{#enum define ProfileID {
    SERIAL_PORT_PROFILE_ID as SerialPortProfileID
  } deriving (Ix, Show, Eq, Read, Ord, Bounded) #}

{#enum define SDPDataRep {
    SDP_UINT8 as SDPCUInt8
  } deriving (Ix, Show, Eq, Read, Ord, Bounded) #}

{#enum define SDPConnectFlag {
    SDP_RETRY_IF_BUSY as SDPRetryIfBusy
  } deriving (Ix, Show, Eq, Read, Ord, Bounded) #}

data C_UUID
data C_SDPProfileDesc
data C_SDPRecord
data C_SDPList
data C_SDPData
data C_SDPSession

{#pointer *uuid_t             as UUIDPtr           -> C_UUID #}
{#pointer *sdp_profile_desc_t as SDPProfileDescPtr -> C_SDPProfileDesc #}
{#pointer *sdp_record_t       as SDPRecordPtr      -> C_SDPRecord #}
{#pointer *sdp_list_t         as SDPListPtr        -> C_SDPList #}
{#pointer *sdp_data_t         as SDPDataPtr        -> C_SDPData #}
{#pointer *sdp_session_t      as SDPSessionPtr     -> C_SDPSession #}

{#fun unsafe sdp_uuid128_create as c_sdp_uuid128_create
  {         `UUIDPtr'
  , castPtr `Ptr a'
  }      -> `UUIDPtr' #}

{#fun pure wr_sdp_profile_desc_get_uuid as c_sdp_profile_desc_get_uuid
  {    `SDPProfileDescPtr'
  } -> `UUIDPtr' #}

{#fun unsafe wr_sdp_set_service_id as c_sdp_set_service_id
  {    `SDPRecordPtr'
  ,    `UUIDPtr'
  } -> `()' #}

{#fun unsafe sdp_uuid2strn as c_sdp_uuid2strn
  {    `UUIDPtr'
  ,    `String'& peekCStringLenIntConv*
  } -> `Int' #}

{#fun unsafe sdp_uuid16_create as c_sdp_uuid16_create
  `Enum e' =>
  {           `UUIDPtr'
  , cFromEnum `e'
  }        -> `UUIDPtr' #}

{#fun unsafe sdp_list_append as c_sdp_list_append
  {         `SDPListPtr'
  , castPtr `Ptr a'
  }      -> `SDPListPtr' #}

{#fun unsafe wr_sdp_set_service_classes as c_sdp_set_service_classes
  {    `SDPRecordPtr'
  ,    `SDPListPtr'
  } -> `Int' #}

{#fun unsafe sdp_set_profile_descs as c_sdp_set_profile_descs
  {    `SDPRecordPtr'
  ,    `SDPListPtr'
  } -> `Int' #}

{#fun unsafe wr_sdp_set_browse_groups as c_sdp_set_browse_groups
  {    `SDPRecordPtr'
  ,    `SDPListPtr'
  } -> `Int' #}

{#fun unsafe sdp_data_alloc as c_sdp_data_alloc
  {         `SDPDataRep'
  , castPtr `Ptr a'
  }      -> `SDPDataPtr' #}

{#fun unsafe sdp_set_access_protos as c_sdp_set_access_protos
  {    `SDPRecordPtr'
  ,    `SDPListPtr'
  } -> `Int' #}

{#fun unsafe sdp_set_info_attr as c_sdp_set_info_attr
  {    `SDPRecordPtr'
  ,    `String'
  ,    `String'
  ,    `String'
  } -> `()' #}

{#fun unsafe wr_sdp_connect as c_sdp_connect
  `Enum e' =>
  { withCast* `BluetoothAddr'
  , withCast* `BluetoothAddr'
  , cFromEnum `e'
  }        -> `SDPSessionPtr' #}

{#fun unsafe sdp_record_register as c_sdp_record_register
  {    `SDPSessionPtr'
  ,    `SDPRecordPtr'
  ,    `Int'
  } -> `Int' #}

{#fun unsafe sdp_data_free as c_sdp_data_free { `SDPDataPtr' } -> `()' #}

{#fun unsafe sdp_list_free as c_sdp_list_free
  {    `SDPListPtr'
  , id `SDPFreeFunPtr'
  } -> `()' #}

{#fun unsafe sdp_close as c_sdp_close { `SDPSessionPtr' } -> `Int' #}