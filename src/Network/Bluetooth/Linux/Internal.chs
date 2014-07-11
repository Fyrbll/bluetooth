{-# LANGUAGE ForeignFunctionInterface #-}
module Network.Bluetooth.Linux.Internal where

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
#include "wr_sdp_lib.h"

{#pointer *uuid_t as UUIDPtr newtype #}
{#pointer *uint32_t as CUInt32Ptr newtype #}
{#pointer *sdp_record_t as SDPRecordPtr newtype #}
{#pointer *sdp_list_t as SDPListPtr newtype #}
{#pointer *sdp_data_t as SDPDataPtr newtype #}
{#pointer *bdaddr_t as BDAddrPtr newtype #}
{#pointer *sdp_session_t as SDPSessionPtr newtype #}
{#pointer sdp_free_func_t as SDPFreeFuncPtr newtype #}

{#fun unsafe sdp_uuid128_create as c_sdp_uuid128_create
  {         `UUIDPtr'
  , castPtr `Ptr a'
  }      -> `UUIDPtr' id #}

{#fun unsafe wr_sdp_set_service_id as c_sdp_set_service_id
  {    `SDPRecordPtr' id
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
  , castPtr `Ptr a' id
  }      -> `SDPListPtr' #}

{#fun unsafe sdp_set_service_classes as c_sdp_set_service_classes
  {    `SDPRecordPtr' id
  ,    `SDPListPtr' id
  } -> `Int' #}

{#fun unsafe sdp_set_profile_descs as c_sdp_set_profile_descs
  {    `SDPRecordPtr' id
  ,    `SDPListPtr'
  } -> `Int' #}

{#fun unsafe sdp_set_browse_groups as c_sdp_set_browse_groups
  {    `SDPRecordPtr' id
  ,    `SDPListPtr' id
  } -> `Int' #}

{#fun unsafe sdp_data_alloc as c_sdp_data_alloc
  {         `Word8'
  , castPtr `Ptr a'
  }      -> `SDPDataPtr' #}

{#fun unsafe sdp_set_access_protos as c_sdp_set_access_protos
  {    `SDPRecordPtr' id
  ,    `SDPListPtr'
  } -> `Int' #}

{#fun unsafe sdp_set_info_attr as c_sdp_set_info_attr
  {    `SDPRecordPtr' id
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
  {    `SDPSessionPtr' id
  ,    `SDPRecordPtr' id
  ,    `Word8'
  } -> `Int' #}

{#fun unsafe sdp_data_free as c_sdp_data_free
  {    `SDPDataPtr' id
  } -> `()' #}

{#fun unsafe sdp_list_free as c_sdp_list_free
  {    `SDPListPtr' id
  ,    `SDPFreeFuncPtr' id
  } -> `()' #}