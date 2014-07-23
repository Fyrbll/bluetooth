module Network.Bluetooth.UUID (
    -- * Protocol identifiers
      baseUUID
    , sdpProtocolUUID
    , udpProtocolUUID
    , rfcommProtocolUUID
    , tcpProtocolUUID
    , tcsBinProtocolUUID
    , tcsAtProtocolUUID
    , attProtocolUUID
    , obexProtocolUUID
    , ipProtocolUUID
    , ftpProtocolUUID
    , httpProtocolUUID
    , wspProtocolUUID
    , bnepProtocolUUID
    , upnpProtocolUUID
    , hidpProtocolUUID
    , hardcopyControlChannelProtocolUUID
    , hardcopyDataChannelProtocolUUID
    , hardcopyNotificationProtocolUUID
    , avctpProtocolUUID
    , avdtpProtocolUUID
    , cmtpProtocolUUID
    , mcapControlChannelProtocolUUID
    , mcapDataChannelProtocolUUID
    , l2capProtocolUUID
    -- * Service classes
    , serviceDiscoveryServerServiceClassIdUUID
    , browseGroupsDescriptorServiceClassIdUUID
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
    , simAccessServiceClassUUID
    , phonebookAccessPCEServiceClassUUID
    , phonebookAccessPSEServiceClassUUID
    , phonebookAccessServiceClassUUID
    , headsetHSServiceClassUUID
    , messageAccessServerServiceClassUUID
    , messageNotificationServerServiceClassUUID
    , messageAccessProfileServiceClassUUID
    , gnssServiceClassUUID
    , gnssServerServiceClassUUID
    , threeDDisplayServiceClassUUID
    , threeDGlassesServiceClassUUID
    , threeDSynchronizationServiceClassUUID
    , mpsProfileServiceClassUUID
    , mpsServiceClassUUID
    , pnPInformationServiceClassUUID
    , genericNetworkingServiceClassUUID
    , genericFileTransferServiceClassUUID
    , genericAudioServiceClassUUID
    , genericTelephonyServiceClassUUID
    , upnpServiceClassUUID
    , upnpIPServiceClassUUID
    , esdpUpnpIpPANServiceClassUUID
    , esdpUpnpIpLAPServiceClassUUID
    , esdpUpnpL2CAPServiceClassUUID
    , videoSourceServiceClassUUID
    , videoSinkServiceClassUUID
    , videoDistributionServiceClassUUID
    , hdpServiceClassUUID
    , hdpSourceServiceClassUUID
    , hdpSinkServiceClassUUID
    ) where

import Data.UUID
import Data.Word

type ShortUUID = Word16

fromShortUUID :: ShortUUID -> UUID
fromShortUUID su = fromWords (fromIntegral su) 0x00001000 0x80000080 0x5F9B34FB

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
  _,
  mcapControlChannelProtocolUUID,
  mcapDataChannelProtocolUUID]
    = map fromShortUUID [0x0000..0x001F]
l2capProtocolUUID = fromShortUUID 0x0100

serviceDiscoveryServerServiceClassIdUUID,
  browseGroupsDescriptorServiceClassIdUUID,
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
  simAccessServiceClassUUID,
  phonebookAccessPCEServiceClassUUID,
  phonebookAccessPSEServiceClassUUID,
  phonebookAccessServiceClassUUID,
  headsetHSServiceClassUUID,
  messageAccessServerServiceClassUUID,
  messageNotificationServerServiceClassUUID,
  messageAccessProfileServiceClassUUID,
  gnssServiceClassUUID,
  gnssServerServiceClassUUID,
  threeDDisplayServiceClassUUID,
  threeDGlassesServiceClassUUID,
  threeDSynchronizationServiceClassUUID,
  mpsProfileServiceClassUUID,
  mpsServiceClassUUID,
  pnPInformationServiceClassUUID,
  genericNetworkingServiceClassUUID,
  genericFileTransferServiceClassUUID,
  genericAudioServiceClassUUID,
  genericTelephonyServiceClassUUID,
  upnpServiceClassUUID,
  upnpIPServiceClassUUID,
  esdpUpnpIpPANServiceClassUUID,
  esdpUpnpIpLAPServiceClassUUID,
  esdpUpnpL2CAPServiceClassUUID,
  videoSourceServiceClassUUID,
  videoSinkServiceClassUUID,
  videoDistributionServiceClassUUID,
  hdpServiceClassUUID,
  hdpSourceServiceClassUUID,
  hdpSinkServiceClassUUID :: UUID
serviceDiscoveryServerServiceClassIdUUID = fromShortUUID 0x1000
browseGroupsDescriptorServiceClassIdUUID = fromShortUUID 0x1001
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
  _, _, _, _,
  simAccessServiceClassUUID,
  phonebookAccessPCEServiceClassUUID,
  phonebookAccessPSEServiceClassUUID,
  phonebookAccessServiceClassUUID,
  headsetHSServiceClassUUID,
  messageAccessServerServiceClassUUID,
  messageNotificationServerServiceClassUUID,
  messageAccessProfileServiceClassUUID,
  gnssServiceClassUUID,
  gnssServerServiceClassUUID,
  threeDDisplayServiceClassUUID,
  threeDGlassesServiceClassUUID,
  threeDSynchronizationServiceClassUUID,
  mpsProfileServiceClassUUID,
  mpsServiceClassUUID]
    = map fromShortUUID [0x1101..0x113B]
[pnPInformationServiceClassUUID,
  genericNetworkingServiceClassUUID,
  genericFileTransferServiceClassUUID,
  genericAudioServiceClassUUID,
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
[hdpServiceClassUUID,
  hdpSourceServiceClassUUID,
  hdpSinkServiceClassUUID]
    = map fromShortUUID [0x1400..0x1402]