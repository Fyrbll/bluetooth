module Network.Bluetooth.Linux.SDP where

import Data.UUID
import Data.Word

import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Network.Bluetooth.Linux.Internal
import Network.Bluetooth.Types
import Network.Bluetooth.Utils

#include <bluetooth/sdp.h>

registerSDPService :: UUID -> SDPInfo -> Int -> IO SDPSessionPtr
registerSDPService uuid info rfcommChannel = do
    -- Null pointer creation a-go-go
    let rootUuid        = nullPtr
        l2capUuid       = nullPtr
        rfcommUuid      = nullPtr
        svcUuid         = nullPtr
        svcClassUuid    = nullPtr
        record          = nullPtr
    let (w1,w2,w3,w4) = toWords uuid
    
    allocaBytes {#sizeof sdp_profile_desc_t #} $ \profile -> do
        _ <- withArray (map byteSwap32 [w1,w2,w3,w4]) $ c_sdp_uuid128_create svcUuid
        c_sdp_set_service_id record svcUuid
        
--         -- Debugging purposes
--         allocaArray 256 $ \str -> do
--             c_sdp_uuid2strn svcUuid str
--             peekCString str >>= putStrLn
        
        _ <- c_sdp_uuid16_create svcClassUuid SerialPortServiceClassID
        svcClassList <- c_sdp_list_append nullPtr svcClassUuid
        throwErrnoIfNegative_ "sdp_set_service_classes" $
          c_sdp_set_service_classes record svcClassList
        
        _ <- c_sdp_uuid16_create (c_sdp_profile_desc_get_uuid profile) SerialPortProfileID
        {#set sdp_profile_desc_t.version #} profile 0x0100
        profileList <- c_sdp_list_append nullPtr profile
        throwErrnoIfNegative_ "sdp_set_profile_descs" $
          c_sdp_set_profile_descs record profileList
        
        _ <- c_sdp_uuid16_create rootUuid PublicBrowseGroup
        rootList <- c_sdp_list_append nullPtr rootUuid
        throwErrnoIfNegative_ "sdp_set_browse_groups" $
          c_sdp_set_browse_groups record rootList
        
        _ <- c_sdp_uuid16_create l2capUuid L2CAP_UUID
        l2capList <- c_sdp_list_append nullPtr rootUuid
        protoList <- c_sdp_list_append nullPtr l2capList
        
        _ <- c_sdp_uuid16_create rfcommUuid RFCOMM_UUID
        channel <- with (fromIntegral rfcommChannel :: CUInt8) $
          c_sdp_data_alloc (cFromEnum SDPCUInt8)
        rfcommList <- c_sdp_list_append nullPtr rfcommUuid
        _ <- c_sdp_list_append rfcommList channel
        _ <- c_sdp_list_append protoList rfcommList
        
        accessProtoList <- c_sdp_list_append nullPtr protoList
        throwErrnoIfNegative_ "sdp_set_access_protos" $
          c_sdp_set_access_protos record accessProtoList
        
        case info of
             (SDPAttributes sn d pn) -> c_sdp_set_info_attr record sn d pn
             SDPNoInfo               -> return ()
        
        session <- throwErrnoIfNull "sdp_connect" $
          c_sdp_connect c_bdaddr_any c_bdaddr_local SDPRetryIfBusy 
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