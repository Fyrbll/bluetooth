module Network.Bluetooth.Linux.SDP where

import Control.Monad

import Data.UUID

import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Network.Bluetooth.Linux.Internal
import Network.Bluetooth.Utils

#include <bluetooth/sdp.h>
#include <bluetooth/sdp_lib.h>

data SDPInfo = SDPAttributes {
                 serviceName  :: String
               , providerName :: String
               , description  :: String
             }
             | SDPNoInfo
  deriving (Read, Show)

registerSDPService :: UUID -> SDPInfo -> Int -> IO SDPSessionPtr
registerSDPService uuid info rfcommChannel = do
    let uuidSize              = {#sizeof uuid_t #}
        (w1,w2,w3,w4)         = toWords uuid
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
          channel <- with rfcommChannel $ c_sdp_data_alloc SDPCUInt8
          rfcommList <- sdpListAppend nullPtr rfcommUuid
          sdpListAppend_ rfcommList channel
          sdpListAppend_ protoList rfcommList
          
          accessProtoList <- sdpListAppend nullPtr protoList
          throwErrnoIfNegative_ "sdp_set_access_protos" $
            c_sdp_set_access_protos record accessProtoList
          
          case info of
               (SDPAttributes sn pn d) -> c_sdp_set_info_attr record sn pn d
               SDPNoInfo               -> return ()
          
--           allocaBytes {#sizeof bdaddr_t #} $ \aa ->
--             allocaBytes {#sizeof bdaddr_t #} $ \la ->
--             withArray [0,0,0,0,0,0] $ \aaa ->
--             withArray [0,0,0,0xff,0xff,0xff] $ \laa -> do
--             {#set bdaddr_t.b #} aa aaa
--             {#set bdaddr_t.b #} la laa
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