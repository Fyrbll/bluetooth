{-# LANGUAGE CPP #-}
module Network.Bluetooth.UUID (
    -- * Short UUIDs
      ShortUUID
    , fromShortUUID
    , toShortUUID
    -- * Base UUID
    ,  baseUUID
     -- * Protocol identifiers
    , sdpProtocolUUID
    , udpProtocolUUID
    , rfcommProtocolUUID
    , tcpProtocolUUID
    , tcsBinProtocolUUID
    , tcsAtProtocolUUID
#if defined(linux_HOST_OS)
    , attProtocolUUID
#endif
    , obexProtocolUUID
    , ipProtocolUUID
    , ftpProtocolUUID
    , httpProtocolUUID
    , wspProtocolUUID
    , bnepProtocolUUID
    , upnpProtocolUUID
#if !defined(mingw32_HOST_OS)
    , hidpProtocolUUID
#endif
    , hardcopyControlChannelProtocolUUID
    , hardcopyDataChannelProtocolUUID
    , hardcopyNotificationProtocolUUID
    , avctpProtocolUUID
    , avdtpProtocolUUID
    , cmtpProtocolUUID
    , udiCPlaneProtocolUUID
#if defined(linux_HOST_OS)
    , mcapControlChannelProtocolUUID
    , mcapDataChannelProtocolUUID
#endif
    , l2capProtocolUUID
    -- * Service classes
    , serviceDiscoveryServerServiceClassUUID
    , browseGroupsDescriptorServiceClassUUID
    , publicBrowseGroupServiceClassUUID
    , serialPortServiceClassUUID
    , lanAccessUsingPPPServiceClassUUID
    , dialupNetworkingServiceClassUUID
    , irMCSyncServiceClassUUID
    , obexObjectPushServiceClassUUID
    , obexFileTransferServiceClassUUID
    , irMCSyncCommandServiceClassUUID
    , headsetServiceClassUUID
    , cordlessTelephonyServiceClassUUID
    , audioSourceServiceClassUUID
    , audioSinkServiceClassUUID
    , avRemoteControlTargetServiceClassUUID
    , advancedAudioDistributionServiceClassUUID
    , avRemoteControlServiceClassUUID
    , avRemoteControlControllerServiceClassUUID
    , intercomServiceClassUUID
    , faxServiceClassUUID
    , headsetAudioGatewayAGServiceClassUUID
    , wapServiceClassUUID
    , wapClientServiceClassUUID
    , panuServiceClassUUID
    , napServiceClassUUID
    , gnServiceClassUUID
    , directPrintingServiceClassUUID
    , referencePrintingServiceClassUUID
    , basicImagingProfileServiceClassUUID
    , imagingResponderServiceClassUUID
    , imagingAutomaticArchiveServiceClassUUID
    , imagingReferencedObjectsServiceClassUUID
    , handsfreeServiceClassUUID
    , handsfreeAudioGatewayUUID
    , directPrintingReferenceObjectsServiceClassUUID
    , reflectingUIServiceClassUUID
    , basicPrintingServiceClassUUID
    , printingStatusServiceClassUUID
    , humanInterfaceDeviceServiceClassUUID
    , hardcopyCableReplacementServiceClassUUID
    , hcrPrintServiceClassUUID
    , hcrScanServiceClassUUID
    , commonISDNAccessServiceClassUUID
    , videoConferencingGWServiceClassUUID
    , udiMTServiceClassUUID
    , udiTAServiceClassUUID
    , audioVideoServiceClassUUID
    , simAccessServiceClassUUID
    , phonebookAccessPCEServiceClassUUID
    , phonebookAccessPSEServiceClassUUID
    , phonebookAccessServiceClassUUID
    , pnPInformationServiceClassUUID
    , genericNetworkingServiceClassUUID
    , genericFileTransferServiceClassUUID
    , genericAudioServiceClassUUID
    , genericTelephonyServiceClassUUID
#if !defined(mingw32_HOST_OS)
    , upnpServiceClassUUID
    , upnpIPServiceClassUUID
    , esdpUpnpIpPANServiceClassUUID
    , esdpUpnpIpLAPServiceClassUUID
    , esdpUpnpL2CAPServiceClassUUID
    , videoSourceServiceClassUUID
    , videoSinkServiceClassUUID
    , videoDistributionServiceClassUUID
#if defined(linux_HOST_OS)
    , hdpServiceClassUUID
    , hdpSourceServiceClassUUID
    , hdpSinkServiceClassUUID
    , appleAgentServiceClassUUID
    , genericAttribServiceClassUUID
#endif
#endif
    ) where

import Data.UUID
import Data.Word

type ShortUUID = Word16

fromShortUUID :: ShortUUID -> UUID
fromShortUUID su = fromWords (fromIntegral su) 0x00001000 0x80000080 0x5F9B34FB

toShortUUID :: UUID -> ShortUUID
toShortUUID uuid = case toWords uuid of (w1,_,_,_) -> fromIntegral w1

baseUUID,
  sdpProtocolUUID,
  udpProtocolUUID,
  rfcommProtocolUUID,
  tcpProtocolUUID,
  tcsBinProtocolUUID,
  tcsAtProtocolUUID,
  attProtocolUUID,
  obexProtocolUUID,
  ipProtocolUUID,
  ftpProtocolUUID,
  httpProtocolUUID,
  wspProtocolUUID,
  bnepProtocolUUID,
  upnpProtocolUUID,
  hidpProtocolUUID,
  hardcopyControlChannelProtocolUUID,
  hardcopyDataChannelProtocolUUID,
  hardcopyNotificationProtocolUUID,
  avctpProtocolUUID,
  avdtpProtocolUUID,
  cmtpProtocolUUID,
  udiCPlaneProtocolUUID,
  mcapControlChannelProtocolUUID,
  mcapDataChannelProtocolUUID,
  l2capProtocolUUID :: UUID
[baseUUID,
  sdpProtocolUUID,
  udpProtocolUUID,
  rfcommProtocolUUID,
  tcpProtocolUUID,
  tcsBinProtocolUUID,
  tcsAtProtocolUUID,
  attProtocolUUID,
  obexProtocolUUID,
  ipProtocolUUID,
  ftpProtocolUUID,
  _,
  httpProtocolUUID,
  _,
  wspProtocolUUID,
  bnepProtocolUUID,
  upnpProtocolUUID,
  hidpProtocolUUID,
  hardcopyControlChannelProtocolUUID,
  _,
  hardcopyDataChannelProtocolUUID,
  _,
  hardcopyNotificationProtocolUUID,
  avctpProtocolUUID,
  _,
  avdtpProtocolUUID,
  _,
  cmtpProtocolUUID,
  _,
  udiCPlaneProtocolUUID,
  mcapControlChannelProtocolUUID,
  mcapDataChannelProtocolUUID]
    = map fromShortUUID [0x0000..0x001F]
l2capProtocolUUID = fromShortUUID 0x0100

serviceDiscoveryServerServiceClassUUID,
  browseGroupsDescriptorServiceClassUUID,
  publicBrowseGroupServiceClassUUID,
  serialPortServiceClassUUID,
  lanAccessUsingPPPServiceClassUUID,
  dialupNetworkingServiceClassUUID,
  irMCSyncServiceClassUUID,
  obexObjectPushServiceClassUUID,
  obexFileTransferServiceClassUUID,
  irMCSyncCommandServiceClassUUID,
  headsetServiceClassUUID,
  cordlessTelephonyServiceClassUUID,
  audioSourceServiceClassUUID,
  audioSinkServiceClassUUID,
  avRemoteControlTargetServiceClassUUID,
  advancedAudioDistributionServiceClassUUID,
  avRemoteControlServiceClassUUID,
  avRemoteControlControllerServiceClassUUID,
  intercomServiceClassUUID,
  faxServiceClassUUID,
  headsetAudioGatewayAGServiceClassUUID,
  wapServiceClassUUID,
  wapClientServiceClassUUID,
  panuServiceClassUUID,
  napServiceClassUUID,
  gnServiceClassUUID,
  directPrintingServiceClassUUID,
  referencePrintingServiceClassUUID,
  basicImagingProfileServiceClassUUID,
  imagingResponderServiceClassUUID,
  imagingAutomaticArchiveServiceClassUUID,
  imagingReferencedObjectsServiceClassUUID,
  handsfreeServiceClassUUID,
  handsfreeAudioGatewayUUID,
  directPrintingReferenceObjectsServiceClassUUID,
  reflectingUIServiceClassUUID,
  basicPrintingServiceClassUUID,
  printingStatusServiceClassUUID,
  humanInterfaceDeviceServiceClassUUID,
  hardcopyCableReplacementServiceClassUUID,
  hcrPrintServiceClassUUID,
  hcrScanServiceClassUUID,
  commonISDNAccessServiceClassUUID,
  videoConferencingGWServiceClassUUID,
  udiMTServiceClassUUID,
  udiTAServiceClassUUID,
  audioVideoServiceClassUUID,
#if !defined(mingw32_HOST_OS)
  simAccessServiceClassUUID,
  phonebookAccessPCEServiceClassUUID,
  phonebookAccessPSEServiceClassUUID,
  phonebookAccessServiceClassUUID,
#endif
  pnPInformationServiceClassUUID,
  genericNetworkingServiceClassUUID,
  genericFileTransferServiceClassUUID,
  genericAudioServiceClassUUID,
#if defined(mingw32_HOST_OS)
  genericTelephonyServiceClassUUID
#else
  genericTelephonyServiceClassUUID,
  upnpServiceClassUUID,
  upnpIPServiceClassUUID,
  esdpUpnpIpPANServiceClassUUID,
  esdpUpnpIpLAPServiceClassUUID,
  esdpUpnpL2CAPServiceClassUUID,
  videoSourceServiceClassUUID,
  videoSinkServiceClassUUID,
  videoDistributionServiceClassUUID,
#if defined(linux_HOST_OS)
  hdpServiceClassUUID,
  hdpSourceServiceClassUUID,
  hdpSinkServiceClassUUID,
  appleAgentServiceClassUUID,
  genericAttribServiceClassUUID
#endif
#endif
    :: UUID
serviceDiscoveryServerServiceClassUUID = fromShortUUID 0x1000
browseGroupsDescriptorServiceClassUUID = fromShortUUID 0x1001
publicBrowseGroupServiceClassUUID      = fromShortUUID 0x1002
[serialPortServiceClassUUID,
  lanAccessUsingPPPServiceClassUUID,
  dialupNetworkingServiceClassUUID,
  irMCSyncServiceClassUUID,
  obexObjectPushServiceClassUUID,
  obexFileTransferServiceClassUUID,
  irMCSyncCommandServiceClassUUID,
  headsetServiceClassUUID,
  cordlessTelephonyServiceClassUUID,
  audioSourceServiceClassUUID,
  audioSinkServiceClassUUID,
  avRemoteControlTargetServiceClassUUID,
  advancedAudioDistributionServiceClassUUID,
  avRemoteControlServiceClassUUID,
  avRemoteControlControllerServiceClassUUID,
  intercomServiceClassUUID,
  faxServiceClassUUID,
  headsetAudioGatewayAGServiceClassUUID,
  wapServiceClassUUID,
  wapClientServiceClassUUID,
  panuServiceClassUUID,
  napServiceClassUUID,
  gnServiceClassUUID,
  directPrintingServiceClassUUID,
  referencePrintingServiceClassUUID,
  basicImagingProfileServiceClassUUID,
  imagingResponderServiceClassUUID,
  imagingAutomaticArchiveServiceClassUUID,
  imagingReferencedObjectsServiceClassUUID,
  handsfreeServiceClassUUID,
  handsfreeAudioGatewayUUID,
  directPrintingReferenceObjectsServiceClassUUID,
  reflectingUIServiceClassUUID,
  basicPrintingServiceClassUUID,
  printingStatusServiceClassUUID,
  humanInterfaceDeviceServiceClassUUID,
  hardcopyCableReplacementServiceClassUUID,
  hcrPrintServiceClassUUID,
  hcrScanServiceClassUUID,
  commonISDNAccessServiceClassUUID,
  videoConferencingGWServiceClassUUID,
  udiMTServiceClassUUID,
  udiTAServiceClassUUID,
#if !defined(mingw32_HOST_OS)
  audioVideoServiceClassUUID,
  simAccessServiceClassUUID,
  phonebookAccessPCEServiceClassUUID,
  phonebookAccessPSEServiceClassUUID,
  phonebookAccessServiceClassUUID]
    = map fromShortUUID [0x1101..0x1130]
#else
  audioVideoServiceClassUUID]
    = map fromShortUUID [0x1101..0x112C]
#endif
[pnPInformationServiceClassUUID,
  genericNetworkingServiceClassUUID,
  genericFileTransferServiceClassUUID,
  genericAudioServiceClassUUID,
#if defined(mingw32_HOST_OS)
  genericTelephonyServiceClassUUID]
    = map fromShortUUID [0x1300..0x1204]
#else
  genericTelephonyServiceClassUUID,
  upnpServiceClassUUID,
  upnpIPServiceClassUUID]
    = map fromShortUUID [0x1200..0x1206]
[esdpUpnpIpPANServiceClassUUID,
  esdpUpnpIpLAPServiceClassUUID,
  esdpUpnpL2CAPServiceClassUUID,
  videoSourceServiceClassUUID,
  videoSinkServiceClassUUID,
  videoDistributionServiceClassUUID]
    = map fromShortUUID [0x1300..0x1305]
#if defined(linux_HOST_OS)
[hdpServiceClassUUID,
  hdpSourceServiceClassUUID,
  hdpSinkServiceClassUUID]
    = map fromShortUUID [0x1400..0x1402]
appleAgentServiceClassUUID    = fromShortUUID 0x2112
genericAttribServiceClassUUID = fromShortUUID 0x1801
#endif
#endif