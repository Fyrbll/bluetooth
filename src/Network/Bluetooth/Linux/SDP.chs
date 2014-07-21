module Network.Bluetooth.Linux.SDP where

import Control.Monad

import Data.UUID
import Data.Word

import Foreign.C.Error
import Foreign.C.String
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
        session         = nullPtr
    let (w1,w2,w3,w4) = toWords uuid
    
    alloca $ \profile -> do
        withArray . map byteSwap32 [w1,w2,w3,w4] $ c_sdp_uuid128_create svcUuid
        c_sdp_set_service_id record svcUuid
        
        -- Debugging purposes
        allocaArray 256 $ \str -> do
            c_sdp_uuid2strn svcUuid str 256
            peekCString str >>= putStrLn
        -- 
        
        c_sdp_uuid16_create svcClassUuid $ cFromEnum SerialPortServiceClassID
        (_,svcClassList) <- c_sdp_list_append 0 svcClassUuid
        throwErrnoIfNegative_ "sdp_set_service_classes" $
          c_sdp_set_service_classes record svcClassList
        
        c_sdp_uuid16_create (c_sdp_profile_desc_get_uuid profile) $ cFromEnum SerialPortProfileID
        {#set sdp_profile_desc_t.version #} profile 0x0100
        (_,profileList) <- c_sdp_list_append 0 profile
        throwErrnoIfNegative_ "sdp_set_profile_descs" $
          c_sdp_set_profile_descs record profileList
        
        c_sdp_uuid16_create rootUuid $ cFromEnum PublicBrowseGroup
        (_,rootList) <- c_sdp_list_append 0 rootUuid
        throwErrnoIfNegative_ "sdp_set_browse_groups" $
          c_sdp_set_browse_groups record rootList
        
        c_sdp_uuid16_create l2capUuid $ cFromEnum L2CAP_UUID
        (_,l2capList) <- c_sdp_list_append 0 rootUuid
        (_,protoList) <- c_sdp_list_append 0 l2capList
        
        c_sdp_uuid16_create rfcommUuid $ cFromEnum RFCOMM_UUID
        channel <- with (fromIntegral rfcommChannel :: CUInt8) $
          c_sdp_data_alloc (cFromEnum SDPCUInt8)
        (_,rfcommList) <- c_sdp_list_append 0 rfcommUuid
        c_sdp_list_append rfcommList channel
        c_sdp_list_append protoList rfcommList
        
        (_,accessProtoList) <- c_sdp_list_append 0 protoList
        throwErrnoIfNegative_ "sdp_set_access_protos" $
          c_sdp_set_access_protos record accessProtoList
        
        case info of
             (SDPAttributes sn d pn) -> c_sdp_set_info_attr record sn d pn
             SDPNoInfo               -> return ()
        
        session <- throwErrnoIfNull "sdp_connect" $
          c_sdp_connect c_bdaddr_any c_bdaddr_local $ cFromEnum SDPRetryIfBusy 
        throwErrnoIfMinus1_ "sdp_record_register" $
          c_sdp_record_register session record 0
        
        c_sdp_data_free channel
        mapM_ (flip c_sdp_list_append 0)
          [l2capList,rfcommList,rootList,accessProtoList,svcClassList,profileList]
        return session