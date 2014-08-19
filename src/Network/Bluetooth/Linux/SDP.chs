module Network.Bluetooth.Linux.SDP (
      registerSDPService
    , closeSDPService
    , SDPInfo(..)
    , defaultSDPInfo
    ) where

import           Control.Monad

import           Data.Ix
import qualified Data.Set as S
import           Data.Set (Set)
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
import           Network.Bluetooth.UUID
import           Network.Socket.Internal

#include "wr_bluetooth.h"
#include "wr_sdp.h"
#include "wr_sdp_lib.h"

registerSDPService :: UUID -> SDPInfo -> BluetoothProtocol -> BluetoothPort -> IO SDPSessionPtr
registerSDPService uuid info btProto port = do
    -- TODO: Check if service is already being advertised
    let uuidSize                     = {#sizeof uuid_t #}
        sdpUUID16Create uuid'        = throwSocketErrorIfNull_ "sdp_uuid16_create" . c_sdp_uuid16_create uuid'
        sdpListAppend list           = throwSocketErrorIfNull "sdp_list_append" . c_sdp_list_append list
        sdpListAppend_ list          = void . sdpListAppend list
        setFoldM q s f               = foldM f q $ S.toList s
        sdpUUID128Create cUuid hUuid = withUUIDArray hUuid $
          throwSocketErrorIfNull_ "sdp_uuid128_create" . c_sdp_uuid128_create cUuid
    
    allocaBytes   uuidSize                 $ \rootUuid     ->
      allocaBytes uuidSize                 $ \l2capUuid    ->
      allocaBytes uuidSize                 $ \rfcommUuid   ->
      allocaBytes uuidSize                 $ \svcUuid      ->
      allocaBytes {#sizeof sdp_record_t #} $ \record       -> do
          -- Convert the UUID to a uuid_t
          sdpUUID128Create svcUuid uuid
          
          {#set sdp_record_t.handle #} record 0xffffffff
          
          -- Make the record publicly browsable
          sdpUUID16Create rootUuid PublicBrowseGroup
          rootList <- sdpListAppend nullPtr rootUuid
          throwSocketErrorIfMinus1_ "sdp_set_browse_groups" $
            c_sdp_set_browse_groups record rootList
          
          -- Set L2CAP information (always happens)
          sdpUUID16Create l2capUuid L2CAPProtocol
          l2capList <- sdpListAppend nullPtr l2capUuid
          protoList <- sdpListAppend nullPtr l2capList
          
          portData <- case btProto of
              L2CAP -> do
                  -- Register the L2CAP PSM
                  psm <- with port $ c_sdp_data_alloc SDP_CUInt16
                  sdpListAppend_ l2capList psm
                  return $ Left psm
              RFCOMM -> do
                  -- Register the RFCOMM channel
                  sdpUUID16Create rfcommUuid RFCOMMProtocol
                  channel <- with port $ c_sdp_data_alloc SDP_CUInt8
                  rfcommList <- sdpListAppend nullPtr rfcommUuid
                  sdpListAppend_ rfcommList channel
                  sdpListAppend_ protoList rfcommList
                  return $ Right (channel, rfcommList)
          
          -- Add additional UUID protocols
          (extraProtosList, protoList') <- setFoldM ([], protoList) (sdpExtraProtocols info)
              $ \(extraProtosList, protoList') hUUIDProto -> do
                  cUUIDProto <- mallocBytes uuidSize
                  sdpUUID16Create cUUIDProto hUUIDProto
                  newList <- sdpListAppend nullPtr cUUIDProto
                  protoList'' <- sdpListAppend protoList' newList
                  return (newList : extraProtosList, protoList'')
          
          accessProtoList <- sdpListAppend nullPtr protoList'
          throwSocketErrorIfMinus1_ "sdp_set_access_protos" $
            c_sdp_set_access_protos record accessProtoList
          
          -- Add UUID service classes
          svcClassList <- if (sdpRegisterUUIDAsServiceClass info)
            then do
                svcClassUuid <- mallocBytes uuidSize
                sdpUUID128Create svcClassUuid uuid
                sdpListAppend nullPtr svcClassUuid
            else return nullPtr
          
          svcClassList' <- setFoldM svcClassList (sdpServiceClasses info)
              $ \svcClassList' hUUIDSvcClass -> do
                  cUUIDSvcClass <- mallocBytes uuidSize
                  sdpUUID16Create cUUIDSvcClass hUUIDSvcClass
                  sdpListAppend svcClassList' cUUIDSvcClass
          
          throwSocketErrorIfMinus1_ "sdp_set_service_classes" $
            c_sdp_set_service_classes record svcClassList'
          
          -- Add UUID profiles
          profileList <- setFoldM nullPtr (sdpProfiles info)
              $ \profileList profile -> do
                  profileDesc <- mallocBytes {#sizeof sdp_profile_desc_t #}
                  sdpUUID16Create (c_sdp_profile_desc_get_uuid profileDesc) profile
                  {#set sdp_profile_desc_t.version #} profileDesc 0x0100 -- Magic number
                  sdpListAppend profileList profileDesc
          
          throwSocketErrorIfMinus1_ "sdp_set_profile_descs" $
            c_sdp_set_profile_descs record profileList
          
          -- Add service name, provider, and description
          case info of SDPInfo {
              sdpServiceName  = name
            , sdpProviderName = prov
            , sdpDescription  = desc
          } -> c_sdp_set_info_attr record name prov desc
          
          -- Set the general service ID
          c_sdp_set_service_id record svcUuid
          
          -- Connect to the local SDP server, register
          -- the service record, and disconnect
          session <- throwErrnoIfNull "sdp_connect" $
            c_sdp_connect anyAddr localAddr SDPRetryIfBusy
          throwSocketErrorIfMinus1_ "sdp_record_register" $
            c_sdp_record_register session record 0
          
          -- Cleanup
          case portData of
               Left psm -> do
                   c_sdp_data_free psm
                   c_sdp_list_free l2capList nullFunPtr
               Right (channel, rfcommList) -> do
                   c_sdp_data_free channel
                   c_sdp_list_free l2capList nullFunPtr
                   c_sdp_list_free rfcommList nullFunPtr
          
          forM_ (reverse extraProtosList) $ flip c_sdp_list_free finalizerFree
          c_sdp_list_free rootList nullFunPtr
          c_sdp_list_free accessProtoList nullFunPtr
          c_sdp_list_free svcClassList' finalizerFree
          c_sdp_list_free profileList finalizerFree
          return session

closeSDPService :: SDPSessionPtr -> IO ()
closeSDPService = throwErrnoIfMinus1_ "sdp_close" . c_sdp_close

data SDPInfo = SDPInfo {
    sdpServiceName                :: Maybe String
  , sdpProviderName               :: Maybe String
  , sdpDescription                :: Maybe String
  , sdpExtraProtocols             :: Set UUIDProtocol
  , sdpServiceClasses             :: Set UUIDServiceClass
  , sdpRegisterUUIDAsServiceClass :: Bool
  , sdpProfiles                   :: Set UUIDProfile
} deriving (Read, Ord, Show, Eq)

defaultSDPInfo :: SDPInfo
defaultSDPInfo = SDPInfo {
    sdpServiceName                = Nothing
  , sdpProviderName               = Nothing
  , sdpDescription                = Nothing
  , sdpExtraProtocols             = S.empty
  , sdpServiceClasses             = S.empty
  , sdpRegisterUUIDAsServiceClass = True
  , sdpProfiles                   = S.empty
}

-------------------------------------------------------------------------------

type CUInt32       = {#type uint32_t #}
type SDPFreeFunPtr = {#type sdp_free_func_t #}

withUUIDArray :: UUID -> (Ptr CUInt32 -> IO a) -> IO a
withUUIDArray uuid = let (w1,w2,w3,w4) = U.toWords uuid
                      in withArray $ map (fromIntegral . byteSwap32) [w1,w2,w3,w4]

{#enum define SDPDataRep {
    SDP_DATA_NIL        as SDPDataNil
  , SDP_UINT8           as SDP_CUInt8
  , SDP_UINT16          as SDP_CUInt16
  , SDP_UINT32          as SDP_CUInt32
  , SDP_UINT64          as SDP_CUInt64
  , SDP_UINT128         as SDP_CUInt128
  , SDP_INT8            as SDP_CInt8
  , SDP_INT16           as SDP_CInt16
  , SDP_INT32           as SDP_CInt32
  , SDP_INT64           as SDP_CInt64
  , SDP_INT128          as SDP_CInt128
  , SDP_UUID_UNSPEC     as SDP_UUIDUnspec
  , SDP_UUID16          as SDP_UUID16
  , SDP_UUID32          as SDP_UUID32
  , SDP_UUID128         as SDP_UUID128
  , SDP_TEXT_STR_UNSPEC as SDPTextStrUnspec
  , SDP_TEXT_STR8       as SDPTextStr8
  , SDP_TEXT_STR16      as SDPTextStr16
  , SDP_TEXT_STR32      as SDPTextStr32
  , SDP_BOOL            as SDPBool
  , SDP_SEQ_UNSPEC      as SDPSeqUnspec
  , SDP_SEQ8            as SDPSeq8
  , SDP_SEQ16           as SDPSeq16
  , SDP_SEQ32           as SDPSeq32
  , SDP_ALT_UNSPEC      as SDPAltUnspec
  , SDP_ALT8            as SDPAlt8
  , SDP_ALT16           as SDPAlt16
  , SDP_ALT32           as SDPAlt32
  , SDP_URL_STR_UNSPEC  as SDP_URLStrUnspec
  , SDP_URL_STR8        as SDP_URLStr8
  , SDP_URL_STR16       as SDP_URLStr16
  , SDP_URL_STR32       as SDP_URLStr32
  } deriving (Ix, Show, Eq, Read, Ord, Bounded) #}

{#enum define SDPConnectFlag {
    SDP_RETRY_IF_BUSY as SDPRetryIfBusy
  , SDP_WAIT_ON_CLOSE as SDPWaitOnClose
  , SDP_NON_BLOCKING  as SDPNonBlocking
  , SDP_LARGE_MTU     as SDPLargeMTU
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
{#pointer *wr_bdaddr_t        as BluetoothAddrPtr  -> BluetoothAddr #}

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
  { with'*    `BluetoothAddr'
  , with'*    `BluetoothAddr'
  , cFromEnum `e'
  }        -> `SDPSessionPtr' #}

{#fun unsafe sdp_record_register as c_sdp_record_register
  {    `SDPSessionPtr'
  ,    `SDPRecordPtr'
  ,    `Int'
  } -> `Int' #}

{#fun unsafe sdp_data_free as c_sdp_data_free { `SDPDataPtr' } -> `()' #}

{#fun sdp_list_free as c_sdp_list_free
  {    `SDPListPtr'
  , id `SDPFreeFunPtr'
  } -> `()' #}

{#fun unsafe sdp_close as c_sdp_close { `SDPSessionPtr' } -> `Int' #}