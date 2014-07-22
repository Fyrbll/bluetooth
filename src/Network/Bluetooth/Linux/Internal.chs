{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}
module Network.Bluetooth.Linux.Internal where

import Data.Ix
import Data.Word

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

import Network.Bluetooth.Utils

#include <bluetooth/bluetooth.h>
#include <bluetooth/rfcomm.h>
#include <bluetooth/sdp.h>
#include <bluetooth/sdp_lib.h>
#include <sys/socket.h>
#include "wr_bluetooth.h"
#include "wr_sdp.h"
#include "wr_sdp_lib.h"

type CUInt8  = {#type uint8_t #}
type CUInt32 = {#type uint32_t #}

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

{#pointer *bdaddr_t       as BDAddrPtr     newtype #}
{#pointer sdp_free_func_t as SDPFreeFunPtr newtype #}

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
  {    `UUIDPtr'
  ,    `Word16'
  } -> `UUIDPtr' #}

{#fun unsafe sdp_list_append as c_sdp_list_append
  {         `SDPListPtr'
  , castPtr `Ptr a'
  }      -> `SDPListPtr' #}

{#fun unsafe sdp_set_service_classes as c_sdp_set_service_classes
  {    `SDPRecordPtr'
  ,    `SDPListPtr'
  } -> `Int' #}

{#fun unsafe sdp_set_profile_descs as c_sdp_set_profile_descs
  {    `SDPRecordPtr'
  ,    `SDPListPtr'
  } -> `Int' #}

{#fun unsafe sdp_set_browse_groups as c_sdp_set_browse_groups
  {    `SDPRecordPtr'
  ,    `SDPListPtr'
  } -> `Int' #}

{#fun unsafe sdp_data_alloc as c_sdp_data_alloc
  {         `Word8'
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

{#fun unsafe sdp_connect as c_sdp_connect
  {    `BDAddrPtr'
  ,    `BDAddrPtr'
  ,    `Word32'
  } -> `SDPSessionPtr' #}

{#fun unsafe sdp_record_register as c_sdp_record_register
  {    `SDPSessionPtr'
  ,    `SDPRecordPtr'
  ,    `Word8'
  } -> `Int' #}

{#fun unsafe sdp_data_free as c_sdp_data_free
  {    `SDPDataPtr'
  } -> `()' #}

{#fun unsafe sdp_list_free as c_sdp_list_free
  {    `SDPListPtr'
  ,    `SDPFreeFunPtr'
  } -> `()' #}

{#fun pure wr_bdaddr_any as c_bdaddr_any {} -> `BDAddrPtr' #}
{#fun pure wr_bdaddr_local as c_bdaddr_local {} -> `BDAddrPtr' #}