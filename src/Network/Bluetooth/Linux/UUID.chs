module Network.Bluetooth.Linux.UUID (
      UUIDProtocol(..)
    , UUIDServiceClass(..)
    ) where

import Data.Ix

#include <bluetooth/sdp.h>

{#enum define UUIDProtocol
  { SDP_UUID       as SDPProtocol
  , UDP_UUID       as UDPProtocol
  , RFCOMM_UUID    as RFCOMMProtocol
  , TCP_UUID       as TCPProtocol
  , TCS_BIN_UUID   as TCS_BINProtocol
  , TCS_AT_UUID    as TCS_ATProtocol
  , ATT_UUID       as ATTProtocol
  , OBEX_UUID      as OBEXProtocol
  , IP_UUID        as IPProtocol
  , FTP_UUID       as FTPProtocol
  , HTTP_UUID      as HTTPProtocol
  , WSP_UUID       as WSPProtocol
  , BNEP_UUID      as BNEPProtocol
  , UPNP_UUID      as UPnPProtocol
  , HIDP_UUID      as HIDPProtocol
  , HCRP_CTRL_UUID as HardcopyControlChannelProtocol
  , HCRP_DATA_UUID as HardcopyDataChannelProtocol
  , HCRP_NOTE_UUID as HardcopyNotificationProtocol
  , AVCTP_UUID     as AVCTPProtocol
  , AVDTP_UUID     as AVDTPProtocol
  , CMTP_UUID      as CMTPProtocol
  , UDI_UUID       as UDI_CPlaneProtocol
  , MCAP_CTRL_UUID as MCAPControlChannelProtocol
  , MCAP_DATA_UUID as MCAPDataChannelProtocol
  , L2CAP_UUID     as L2CAPProtocol
  } deriving (Ix, Show, Eq, Read, Ord, Bounded) #}

{#enum define UUIDServiceClass
  { SDP_SERVER_SVCLASS_ID           as ServiceDiscoveryServer
  , BROWSE_GRP_DESC_SVCLASS_ID      as BrowseGroupDescriptor
  , PUBLIC_BROWSE_GROUP             as PublicBrowseGroup
  , SERIAL_PORT_SVCLASS_ID          as SerialPort
  , LAN_ACCESS_SVCLASS_ID           as LANAccessUsingPPP
  , DIALUP_NET_SVCLASS_ID           as DialupNetworking
  , IRMC_SYNC_SVCLASS_ID            as IrMCSync
  , OBEX_OBJPUSH_SVCLASS_ID         as OBEXObjectPush
  , OBEX_FILETRANS_SVCLASS_ID       as OBEXFileTransfer
  , IRMC_SYNC_CMD_SVCLASS_ID        as IrMCSyncCommand
  , HEADSET_SVCLASS_ID              as Headset
  , CORDLESS_TELEPHONY_SVCLASS_ID   as CordlessTelephony
  , AUDIO_SOURCE_SVCLASS_ID         as AudioSource
  , AUDIO_SINK_SVCLASS_ID           as AudioSink
  , AV_REMOTE_TARGET_SVCLASS_ID     as AVRemoteControlTarget
  , ADVANCED_AUDIO_SVCLASS_ID       as AdvancedAudioDistribution
  , AV_REMOTE_SVCLASS_ID            as AVRemoteControl
  , INTERCOM_SVCLASS_ID             as Intercom
  , FAX_SVCLASS_ID                  as Fax
  , HEADSET_AGW_SVCLASS_ID          as HeadsetAudioGateway
  , WAP_SVCLASS_ID                  as WAP
  , WAP_CLIENT_SVCLASS_ID           as WAPClient
  , PANU_SVCLASS_ID                 as PANU
  , NAP_SVCLASS_ID                  as NAP
  , GN_SVCLASS_ID                   as GN
  , DIRECT_PRINTING_SVCLASS_ID      as DirectPrinting
  , REFERENCE_PRINTING_SVCLASS_ID   as ReferencePrinting
  , IMAGING_SVCLASS_ID              as Imaging
  , IMAGING_RESPONDER_SVCLASS_ID    as ImagingResponder
  , IMAGING_ARCHIVE_SVCLASS_ID      as ImagingAutomaticArchive
  , IMAGING_REFOBJS_SVCLASS_ID      as ImagingReferencedObjects
  , HANDSFREE_SVCLASS_ID            as Handsfree
  , HANDSFREE_AGW_SVCLASS_ID        as HandsfreeAudioGateway
  , DIRECT_PRT_REFOBJS_SVCLASS_ID   as DirectPrintingReferenceObjects
  , REFLECTED_UI_SVCLASS_ID         as ReflectedUI
  , BASIC_PRINTING_SVCLASS_ID       as BasicPrinting
  , PRINTING_STATUS_SVCLASS_ID      as PrintingStatus
  , HID_SVCLASS_ID                  as HumanInterfaceDevice
  , HCR_SVCLASS_ID                  as HardcopyCableReplacement
  , HCR_PRINT_SVCLASS_ID            as HCRPrint
  , HCR_SCAN_SVCLASS_ID             as HCRScan
  , CIP_SVCLASS_ID                  as CommonISDNAccess
  , VIDEO_CONF_GW_SVCLASS_ID        as VideoConferencingGW
  , UDI_MT_SVCLASS_ID               as UDI_MT
  , UDI_TA_SVCLASS_ID               as UDI_TA
  , AV_SVCLASS_ID                   as AudioVideo
  , SAP_SVCLASS_ID                  as SIMAccess
  , PBAP_PCE_SVCLASS_ID             as PhonebookAccessPCE
  , PBAP_PSE_SVCLASS_ID             as PhonebookAccessPSE
  , PBAP_SVCLASS_ID                 as PhonebookAccess
  , PNP_INFO_SVCLASS_ID             as PnPInformation
  , GENERIC_NETWORKING_SVCLASS_ID   as GenericNetworking
  , GENERIC_FILETRANS_SVCLASS_ID    as GenericFileTransfer
  , GENERIC_AUDIO_SVCLASS_ID        as GenericAudio
  , GENERIC_TELEPHONY_SVCLASS_ID    as GenericTelephony
  , UPNP_SVCLASS_ID                 as UPnP
  , UPNP_IP_SVCLASS_ID              as UPnP_IP
  , UPNP_PAN_SVCLASS_ID             as ESDP_UPnP_IP_PAN
  , UPNP_LAP_SVCLASS_ID             as ESDP_UPnP_IP_LAP
  , UPNP_L2CAP_SVCLASS_ID           as ESDP_UPnP_L2CAP
  , VIDEO_SOURCE_SVCLASS_ID         as VideoSource
  , VIDEO_SINK_SVCLASS_ID           as VideoSink
  , VIDEO_DISTRIBUTION_SVCLASS_ID   as VideoDistribution
  , HDP_SVCLASS_ID                  as HDP
  , HDP_SOURCE_SVCLASS_ID           as HDPSource
  , HDP_SINK_SVCLASS_ID             as HDPSink
  , GENERIC_ATTRIB_SVCLASS_ID       as GenericAttribute
  , APPLE_AGENT_SVCLASS_ID          as AppleAgentService
  } deriving (Ix, Show, Eq, Read, Ord, Bounded) #}