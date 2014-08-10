{-# LANGUAGE CPP, MagicHash, NamedFieldPuns #-}
module Network.Bluetooth.Linux.SDP where

import           Control.Monad

import           Data.Ix
import qualified Data.Set as S
import           Data.Set (Set)
import qualified Data.UUID as U
import           Data.UUID (UUID)
import qualified Data.Word as W
import           Data.Word (Word32)

import           Foreign.C.Error
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable

#if __GLASGOW_HASKELL__ < 708
import           GHC.Prim
import           GHC.Word
#endif

import           Network.Bluetooth.Linux.Addr
import           Network.Bluetooth.Linux.Protocol
import           Network.Bluetooth.Utils
import           Network.Bluetooth.UUID

#include "wr_bluetooth.h"
#include "wr_sdp.h"
#include "wr_sdp_lib.h"

registerSDPService :: UUID -> SDPInfo -> BluetoothProtocol -> BluetoothPort -> IO SDPSessionPtr
registerSDPService uuid info proto port = do
    -- TODO: Check if service is already being advertised
    let uuidSize              = {#sizeof uuid_t #}
        sdpUUID16Create uuid' = throwErrnoIfNull_ "sdp_uuid16_create" . c_sdp_uuid16_create uuid'
        sdpListAppend list    = throwErrnoIfNull "sdp_list_append" . c_sdp_list_append list
        sdpListAppend_ list   = void . sdpListAppend list
        setForM s             = forM $ S.toList s
        sdpUUID128Create hUuid cUuid = withUUIDArray hUuid $
          throwErrnoIfNull_ "sdp_uuid128_create" . c_sdp_uuid128_create cUuid
    
    allocaBytes   uuidSize                       $ \rootUuid     ->
      allocaBytes uuidSize                       $ \l2capUuid    ->
      allocaBytes uuidSize                       $ \rfcommUuid   ->
      allocaBytes uuidSize                       $ \svcUuid      ->
      allocaBytes uuidSize                       $ \svcClassUuid ->
      allocaBytes {#sizeof sdp_profile_desc_t #} $ \profile      ->
      allocaBytes {#sizeof sdp_record_t #}       $ \record       -> do
          -- Convert the UUID to a uuid_t
          sdpUUID128Create uuid svcUuid
          
          {#set sdp_record_t.handle #} record 0xffffffff
          
          -- Make the record publicly browsable
          sdpUUID16Create rootUuid $ toShortUUID publicBrowseGroupServiceClassUUID
          rootList <- sdpListAppend nullPtr rootUuid
          throwErrnoIfNegative_ "sdp_set_browse_groups" $
            c_sdp_set_browse_groups record rootList
          
          -- Set L2CAP information (always happens)
          sdpUUID16Create l2capUuid $ toShortUUID l2capProtocolUUID
          l2capList <- sdpListAppend nullPtr l2capUuid
          protoList <- sdpListAppend nullPtr l2capList
          
          (portData, rfcommList) <- case proto of
              L2CAP -> do
                  -- Register the L2CAP PSM
                  psm <- with port $ c_sdp_data_alloc SDPCUInt16
                  sdpListAppend_ l2capList psm
                  return (psm, l2capList)
              RFCOMM -> do
                  -- Register the RFCOMM channel
                  sdpUUID16Create rfcommUuid $ toShortUUID rfcommProtocolUUID
                  channel <- with port $ c_sdp_data_alloc SDPCUInt8
                  rfcommList <- sdpListAppend nullPtr rfcommUuid
                  sdpListAppend_ rfcommList channel
                  sdpListAppend_ protoList rfcommList
                  return (channel, rfcommList)
          
          extraProtosList <- setForM (sdpExtraProtocols info) $ \hProtoUuid -> do
              cProtoUuid <- mallocBytes uuidSize
              sdpUUID128Create hProtoUuid cProtoUuid
              newList <- sdpListAppend nullPtr cProtoUuid
              sdpListAppend_ protoList newList
              return newList
          
          c_sdp_set_service_id record svcUuid
          
          sdpUUID16Create svcClassUuid $ toShortUUID serialPortServiceClassUUID
          svcClassList <- sdpListAppend nullPtr svcClassUuid
          throwErrnoIfNegative_ "sdp_set_service_classes" $
            c_sdp_set_service_classes record svcClassList
          
          sdpUUID16Create (c_sdp_profile_desc_get_uuid profile) SerialPortProfileID
          {#set sdp_profile_desc_t.version #} profile 0x0100
          profileList <- sdpListAppend nullPtr profile
          throwErrnoIfNegative_ "sdp_set_profile_descs" $
            c_sdp_set_profile_descs record profileList
          
          accessProtoList <- sdpListAppend nullPtr protoList
          throwErrnoIfNegative_ "sdp_set_access_protos" $
            c_sdp_set_access_protos record accessProtoList
          
          case info of SDPInfo {
              sdpServiceName  = name
            , sdpProviderName = prov
            , sdpDescription  = desc
          } -> c_sdp_set_info_attr record name prov desc
          
          session <- throwErrnoIfNull "sdp_connect" $
            c_sdp_connect anyAddr localAddr SDPRetryIfBusy
          throwErrnoIfMinus1_ "sdp_record_register" $
            c_sdp_record_register session record 0
          
          c_sdp_data_free portData
          
          withSDPFreeFunPtr free $ \freeFunPtr -> do
            c_sdp_list_free l2capList nullFunPtr
            when (proto == RFCOMM) $
              c_sdp_list_free rfcommList nullFunPtr
            forM_ extraProtosList $ flip c_sdp_list_free freeFunPtr
            c_sdp_list_free rootList nullFunPtr
            c_sdp_list_free accessProtoList nullFunPtr
            c_sdp_list_free svcClassList nullFunPtr
            c_sdp_list_free profileList nullFunPtr
          return session

closeSDPService :: SDPSessionPtr -> IO ()
closeSDPService = throwErrnoIfMinus1_ "sdp_close" . c_sdp_close

data SDPInfo = SDPInfo {
    sdpServiceName    :: Maybe String
  , sdpProviderName   :: Maybe String
  , sdpDescription    :: Maybe String
  , sdpExtraProtocols :: Set UUID
} deriving (Read, Ord, Show, Eq)

defaultSDPInfo :: SDPInfo
defaultSDPInfo = SDPInfo {
    sdpServiceName    = Nothing
  , sdpProviderName   = Nothing
  , sdpDescription    = Nothing
  , sdpExtraProtocols = S.empty
}

-------------------------------------------------------------------------------

type CUInt32       = {#type uint32_t #}
type SDPFreeFun    = Ptr () -> IO ()
type SDPFreeFunPtr = {#type sdp_free_func_t #}

withUUIDArray :: UUID -> (Ptr CUInt32 -> IO a) -> IO a
withUUIDArray uuid = let (w1,w2,w3,w4) = U.toWords uuid
                      in withArray $ map (fromIntegral . byteSwap32) [w1,w2,w3,w4]

byteSwap32 :: Word32 -> Word32
#if __GLASGOW_HASKELL__ >= 708
byteSwap32 = W.byteSwap32
#else
byteSwap32 (W32# w#) = W32# (narrow32Word# (byteSwap32# w#))
#endif

-- {#enum define ProtocolUUID {
--     SDP_UUID     as SDP_UUID
--   , UDP_UUID     as SDP_UUID
--   , RFCOMM_UUID  as RFCOMM_UUID
--   , TCP_UUID     as TCP_UUID
--   , TCS_BIN_UUID as TCS_BIN_UUID
--   , TCS_AT_UUID  as TCS_AT_UUID
--   , ATT_UUID     as ATT_UUID
--   , OBEX_UUID    as OBEX_UUID
--   , IP_UUID      as IP_UUID
--   , 
--   , L2CAP_UUID   as L2CAP_UUID
--   } deriving (Ix, Show, Eq, Read, Ord, Bounded) #}
-- 
-- {#enum define ServiceClassID {
--     PUBLIC_BROWSE_GROUP    as PublicBrowseGroup
--   , SERIAL_PORT_SVCLASS_ID as SerialPortServiceClassID
--   } deriving (Ix, Show, Eq, Read, Ord, Bounded) #}

{#enum define ProfileID {
    SERIAL_PORT_PROFILE_ID as SerialPortProfileID
  } deriving (Ix, Show, Eq, Read, Ord, Bounded) #}

{#enum define SDPDataRep {
    SDP_UINT8  as SDPCUInt8
  , SDP_UINT16 as SDPCUInt16
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

foreign import ccall "wrapper"
  wrapSDPFreeFun :: SDPFreeFun -> IO SDPFreeFunPtr

withSDPFreeFunPtr :: SDPFreeFun -> (SDPFreeFunPtr -> IO a) -> IO a
withSDPFreeFunPtr sff f = do
    sffp <- wrapSDPFreeFun sff
    res <- f sffp
    freeHaskellFunPtr sffp
    return res

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
  ,    `String'&
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
  {                   `SDPRecordPtr'
  , maybeWithCString* `Maybe String'
  , maybeWithCString* `Maybe String'
  , maybeWithCString* `Maybe String'
  }                -> `()' #}
  where
    maybeWithCString :: Maybe String -> (CString -> IO a) -> IO a
    maybeWithCString = maybeWith withCString

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