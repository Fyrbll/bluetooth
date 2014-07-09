{-# LANGUAGE ForeignFunctionInterface #-}
module Network.Bluetooth.Linux.Internal where

import Control.Applicative

import Data.Ix

import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Network.Bluetooth.Utils

#include <bluetooth/bluetooth.h>
#include <bluetooth/rfcomm.h>
#include <bluetooth/sdp.h>
#include <bluetooth/sdp_lib.h>
#include <sys/socket.h>

{#enum define SdpUUIDType
  { SDP_UUID16  as SdpUUID16
  , SDP_UUID32  as SdpUUID32
  , SDP_UUID128 as SdpUUID128
  } deriving (Eq, Ord, Bounded, Show, Read, Ix) #}

type CUInt8   = {#type uint8_t #}
type CUInt16  = {#type uint16_t #}
type CUInt32  = {#type uint32_t #}
-- type CUInt128 = {#type uint128_t #}

data CUInt128 = CUInt128 CUInt8 CUInt8 CUInt8 CUInt8
                         CUInt8 CUInt8 CUInt8 CUInt8
                         CUInt8 CUInt8 CUInt8 CUInt8
                         CUInt8 CUInt8 CUInt8 CUInt8

data UUID = UUID16  CUInt16
          | UUID32  CUInt32
          | UUID128 CUInt128

instance Storable CUInt128 where
  sizeOf _ = {#sizeof uint128_t #}
  alignment _ = {#alignof uint128_t #}
  peek ptr' = do
    arr <- {#get uint128_t.data #} ptr'
    [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p] <- peekArray 16 arr
    return $ CUInt128 a b c d e f g h i j k l m n o p
  poke ptr' (CUInt128 a b c d e f g h i j k l m n o p) =
    withArray [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p] $ \ arr ->
      {#set uint128_t.data #} ptr' arr

instance Storable UUID where
  sizeOf _ = {#sizeof uuid_t #}
  alignment _ = {#alignof uuid_t #}
  peek p = do
    t <- {#get uuid_t.type #} p
    case cToEnum t of
         SdpUUID16  -> mkUUID UUID16  {#get uuid_t.value.uuid16 #}
         SdpUUID32  -> mkUUID UUID32  {#get uuid_t.value.uuid32 #}
         SdpUUID128 -> UUID128 <$> peekByteOff p {#offsetof uuid_t.value.uuid128 #}
    where mkUUID con get = con <$> fromIntegral <$> get p
  poke p (UUID16 i) = do
    {#set uuid_t.type #} p $ cFromEnum SdpUUID16
    {#set uuid_t.value.uuid16 #} p $ fromIntegral i
  poke p (UUID32 i) = do
    {#set uuid_t.type #} p $ cFromEnum SdpUUID32
    {#set uuid_t.value.uuid32 #} p $ fromIntegral i
  poke p (UUID128 i) = do
    {#set uuid_t.type #} p $ cFromEnum SdpUUID128
    pokeByteOff p {#offsetof uuid_t.value.uuid128 #} i

{#pointer *uuid_t as UUIDPtr -> UUID #}
{#pointer *uint32_t as CUInt32Ptr -> CUInt32 #}

{#fun unsafe sdp_uuid128_create as c_sdp_uuid128_create
  {         `UUIDPtr'
  , castPtr `Ptr a'
  } -> `Ptr UUID' id #}
-- 
-- {#fun unsafe sdp_set_service_id as c_sdp_set_service_id
--   { `SDPRecord' -- sdp_record_t *rec
--   
--   } -> `()'#}