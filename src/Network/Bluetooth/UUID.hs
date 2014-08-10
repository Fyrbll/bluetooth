{-# LANGUAGE CPP, MagicHash #-}
module Network.Bluetooth.UUID (
    -- * Short UUIDs
      ShortUUID
    , fromShortUUID
    , toShortUUID
    , isReservedUUID
    , byteSwap32
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

import           Data.UUID
import qualified Data.Word as W
import           Data.Word (Word16, Word32)

#if __GLASGOW_HASKELL__ < 708
import           GHC.Prim
import           GHC.Word
#endif

type ShortUUID = Word16

baseUUIDWord2, baseUUIDWord3, baseUUIDWord4 :: Word32
baseUUIDWord2 = 0x00001000
baseUUIDWord3 = 0x80000080
baseUUIDWord4 = 0x5F9B34FB

fromShortUUID :: ShortUUID -> UUID
fromShortUUID su = fromWords (fromIntegral su) baseUUIDWord2 baseUUIDWord3 baseUUIDWord4

toShortUUID :: UUID -> ShortUUID
toShortUUID uuid = let (w1,_,_,_) = toWords uuid
                    in fromIntegral w1

isReservedUUID :: UUID -> Bool
isReservedUUID uuid = let (w1,w2,w3,w4) = toWords uuid
  in (fromIntegral $ byteSwap32 w1) == (0x0000 :: Word16)
  && w2 == baseUUIDWord2
  && w3 == baseUUIDWord3
  && w4 == baseUUIDWord4

byteSwap32 :: Word32 -> Word32
#if __GLASGOW_HASKELL__ >= 708
byteSwap32 = W.byteSwap32
#else
byteSwap32 (W32# w#) = W32# (narrow32Word# (byteSwap32# w#))
#endif

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