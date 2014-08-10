{-# LANGUAGE CPP, MagicHash #-}
module Network.Bluetooth.UUID (
      ShortUUID
    , fromShortUUID
    , toShortUUID
    , isReservedUUID
    , randomUUID
    , protocolToUUID
    , serviceClassToUUID
    , byteSwap32
    , baseUUID
    , UUIDProtocol(..)
    , UUIDServiceClass(..)
    , UUIDProfile
    ) where

import           Data.Ix
import           Data.UUID
import qualified Data.Word as W
import           Data.Word (Word16, Word32)

#if __GLASGOW_HASKELL__ < 708
import           GHC.Prim
import           GHC.Word
#endif

import           Network.Bluetooth.Utils

import           System.Random

type ShortUUID = Word16

baseUUID :: UUID
baseUUID = fromWords 0x00000000 baseUUIDWord2 baseUUIDWord3 baseUUIDWord4

baseUUIDWord2, baseUUIDWord3, baseUUIDWord4 :: Word32
baseUUIDWord2 = 0x00001000
baseUUIDWord3 = 0x80000080
baseUUIDWord4 = 0x5F9B34FB

fromShortUUID :: ShortUUID -> UUID
fromShortUUID su = fromWords (fromIntegral su) baseUUIDWord2 baseUUIDWord3 baseUUIDWord4

toShortUUID :: UUID -> ShortUUID
toShortUUID uuid = let (w1,_,_,_) = toWords uuid
                    in fromIntegral w1

randomUUID :: IO UUID
randomUUID = do
    uuid <- randomIO
    if isReservedUUID uuid
       then randomUUID
       else return uuid

isReservedUUID :: UUID -> Bool
isReservedUUID uuid = let (w1,w2,w3,w4) = toWords uuid
    in (fromIntegral $ byteSwap32 w1) == (0x0000 :: Word16)
    && w2 == baseUUIDWord2
    && w3 == baseUUIDWord3
    && w4 == baseUUIDWord4
    && isReserved (fromIntegral w1)
  where
    isReserved :: Word16 -> Bool
    isReserved w = any (flip inRange w)
      [(0x0000, 0x000A), (0x000C, 0x000C), (0x000E, 0x0012), (0x0014, 0x0014),
       (0x0016, 0x0017), (0x0019, 0x0019), (0x001B, 0x001B), (0x001D, 0x001F),
       (0x0100, 0x0100), (0x1000, 0x1002), (0x1101, 0x113B), (0x1200, 0x1206),
       (0x1300, 0x1305), (0x1400, 0x1402), (0x1801, 0x1801), (0x2112, 0x2122)]

protocolToUUID :: UUIDProtocol -> UUID
protocolToUUID = fromShortUUID . cFromEnum

serviceClassToUUID :: UUIDServiceClass -> UUID
serviceClassToUUID = fromShortUUID . cFromEnum

byteSwap32 :: Word32 -> Word32
#if __GLASGOW_HASKELL__ >= 708
byteSwap32 = W.byteSwap32
#else
byteSwap32 (W32# w#) = W32# (narrow32Word# (byteSwap32# w#))
#endif

data UUIDProtocol = SDPProtocol
                  | UDPProtocol
                  | RFCOMMProtocol
                  | TCPProtocol
                  | TCS_BINProtocol
                  | TCS_ATProtocol
#if defined(linux_HOST_OS)
                  | ATTProtocol
#endif
                  | OBEXProtocol
                  | IPProtocol
                  | FTPProtocol
                  | HTTPProtocol
                  | WSPProtocol
                  | BNEPProtocol
                  | UPnPProtocol
#if !defined(mingw32_HOST_OS)
                  | HIDPProtocol
#endif
                  | HardcopyControlChannelProtocol
                  | HardcopyDataChannelProtocol
                  | HardcopyNotificationProtocol
                  | AVCTPProtocol
                  | AVDTPProtocol
                  | CMTPProtocol
                  | UDI_CPlaneProtocol
#if defined(linux_HOST_OS)
                  | MCAPControlChannelProtocol
                  | MCAPDataChannelProtocol
#endif
                  | L2CAPProtocol
                  deriving (Ix, Show, Eq, Read, Ord, Bounded)

instance Enum UUIDProtocol where
    fromEnum SDPProtocol                    = 0x0001
    fromEnum UDPProtocol                    = 0x0002
    fromEnum RFCOMMProtocol                 = 0x0003
    fromEnum TCPProtocol                    = 0x0004
    fromEnum TCS_BINProtocol                = 0x0005
    fromEnum TCS_ATProtocol                 = 0x0006
#if defined(linux_HOST_OS)
    fromEnum ATTProtocol                    = 0x0007
#endif
    fromEnum OBEXProtocol                   = 0x0008
    fromEnum IPProtocol                     = 0x0009
    fromEnum FTPProtocol                    = 0x000A
    fromEnum HTTPProtocol                   = 0x000C
    fromEnum WSPProtocol                    = 0x000E
    fromEnum BNEPProtocol                   = 0x000F
    fromEnum UPnPProtocol                   = 0x0010
#if !defined(mingw32_HOST_OS)
    fromEnum HIDPProtocol                   = 0x0011
#endif
    fromEnum HardcopyControlChannelProtocol = 0x0012
    fromEnum HardcopyDataChannelProtocol    = 0x0014
    fromEnum HardcopyNotificationProtocol   = 0x0016
    fromEnum AVCTPProtocol                  = 0x0017
    fromEnum AVDTPProtocol                  = 0x0019
    fromEnum CMTPProtocol                   = 0x001B
    fromEnum UDI_CPlaneProtocol             = 0x001D
#if defined(linux_HOST_OS)
    fromEnum MCAPControlChannelProtocol     = 0x001E
    fromEnum MCAPDataChannelProtocol        = 0x001F
#endif
    fromEnum L2CAPProtocol                  = 0x0100
    
    toEnum 0x0001 = SDPProtocol
    toEnum 0x0002 = UDPProtocol
    toEnum 0x0003 = RFCOMMProtocol
    toEnum 0x0004 = TCPProtocol
    toEnum 0x0005 = TCS_BINProtocol
    toEnum 0x0006 = TCS_ATProtocol
#if defined(linux_HOST_OS)
    toEnum 0x0007 = ATTProtocol
#endif
    toEnum 0x0008 = OBEXProtocol
    toEnum 0x0009 = IPProtocol
    toEnum 0x000A = FTPProtocol
    toEnum 0x000C = HTTPProtocol
    toEnum 0x000E = WSPProtocol
    toEnum 0x000F = BNEPProtocol
    toEnum 0x0010 = UPnPProtocol
#if !defined(mingw32_HOST_OS)
    toEnum 0x0011 = HIDPProtocol
#endif
    toEnum 0x0012 = HardcopyControlChannelProtocol
    toEnum 0x0014 = HardcopyDataChannelProtocol
    toEnum 0x0016 = HardcopyNotificationProtocol
    toEnum 0x0017 = AVCTPProtocol
    toEnum 0x0019 = AVDTPProtocol
    toEnum 0x001B = CMTPProtocol
    toEnum 0x001D = UDI_CPlaneProtocol
#if defined(linux_HOST_OS)
    toEnum 0x001E = MCAPControlChannelProtocol
    toEnum 0x001F = MCAPDataChannelProtocol
#endif
    toEnum 0x0100 = L2CAPProtocol
    toEnum unmatched = error $ "UUIDProtocol.toEnum: Cannot match " ++ show unmatched

type UUIDProfile = UUIDServiceClass

data UUIDServiceClass = ServiceDiscoveryServer
                      | BrowseGroupDescriptor
                      | PublicBrowseGroup
                      | SerialPort
                      | LANAccessUsingPPP
                      | DialupNetworking
                      | IrMCSync
                      | OBEXObjectPush
                      | OBEXFileTransfer
                      | IrMCSyncCommand
                      | Headset
                      | CordlessTelephony
                      | AudioSource
                      | AudioSink
                      | AVRemoteControlTarget
                      | AdvancedAudioDistribution
                      | AVRemoteControl
                      | VideoConferencing
                      | Intercom
                      | Fax
                      | HeadsetAudioGateway
                      | WAP
                      | WAPClient
                      | PANU
                      | NAP
                      | GN
                      | DirectPrinting
                      | ReferencePrinting
                      | Imaging
                      | ImagingResponder
                      | ImagingAutomaticArchive
                      | ImagingReferencedObjects
                      | Handsfree
                      | HandsfreeAudioGateway
                      | DirectPrintingReferenceObjects
                      | ReflectedUI
                      | BasicPrinting
                      | PrintingStatus
                      | HumanInterfaceDevice
                      | HardcopyCableReplacement
                      | HCRPrint
                      | HCRScan
                      | CommonISDNAccess
                      | VideoConferencingGW
                      | UDI_MT
                      | UDI_TA
                      | AudioVideo
#if !defined(mingw32_HOST_OS)
                      | SIMAccess
                      | PhonebookAccessPCE
                      | PhonebookAccessPSE
                      | PhonebookAccess
#endif
                      | PnPInformation
                      | GenericNetworking
                      | GenericFileTransfer
                      | GenericAudio
                      | GenericTelephony
#if !defined(mingw32_HOST_OS)
                      | UPnP
                      | UPnP_IP
                      | ESDP_UPnP_IP_PAN
                      | ESDP_UPnP_IP_LAP
                      | ESDP_UPnP_L2CAP
                      | VideoSource
                      | VideoSink
                      | VideoDistribution
#if defined(linux_HOST_OS)
                      | HDP
                      | HDPSource
                      | HDPSink
                      | GenericAttribute
                      | AppleAgentService
#endif
#endif
                      deriving (Ix, Show, Eq, Read, Ord, Bounded)

instance Enum UUIDServiceClass where
    fromEnum ServiceDiscoveryServer         = 0x1000
    fromEnum BrowseGroupDescriptor          = 0x1001
    fromEnum PublicBrowseGroup              = 0x1002
    fromEnum SerialPort                     = 0x1101
    fromEnum LANAccessUsingPPP              = 0x1102
    fromEnum DialupNetworking               = 0x1103
    fromEnum IrMCSync                       = 0x1104
    fromEnum OBEXObjectPush                 = 0x1105
    fromEnum OBEXFileTransfer               = 0x1106
    fromEnum IrMCSyncCommand                = 0x1107
    fromEnum Headset                        = 0x1108
    fromEnum CordlessTelephony              = 0x1109
    fromEnum AudioSource                    = 0x110A
    fromEnum AudioSink                      = 0x110B
    fromEnum AVRemoteControlTarget          = 0x110C
    fromEnum AdvancedAudioDistribution      = 0x110D
    fromEnum AVRemoteControl                = 0x110E
    fromEnum VideoConferencing              = 0x110F
    fromEnum Intercom                       = 0x1110
    fromEnum Fax                            = 0x1111
    fromEnum HeadsetAudioGateway            = 0x1112
    fromEnum WAP                            = 0x1113
    fromEnum WAPClient                      = 0x1114
    fromEnum PANU                           = 0x1115
    fromEnum NAP                            = 0x1116
    fromEnum GN                             = 0x1117
    fromEnum DirectPrinting                 = 0x1118
    fromEnum ReferencePrinting              = 0x1119
    fromEnum Imaging                        = 0x111A
    fromEnum ImagingResponder               = 0x111B
    fromEnum ImagingAutomaticArchive        = 0x111C
    fromEnum ImagingReferencedObjects       = 0x111D
    fromEnum Handsfree                      = 0x111E
    fromEnum HandsfreeAudioGateway          = 0x111F
    fromEnum DirectPrintingReferenceObjects = 0x1120
    fromEnum ReflectedUI                    = 0x1121
    fromEnum BasicPrinting                  = 0x1122
    fromEnum PrintingStatus                 = 0x1123
    fromEnum HumanInterfaceDevice           = 0x1124
    fromEnum HardcopyCableReplacement       = 0x1125
    fromEnum HCRPrint                       = 0x1126
    fromEnum HCRScan                        = 0x1127
    fromEnum CommonISDNAccess               = 0x1128
    fromEnum VideoConferencingGW            = 0x1129
    fromEnum UDI_MT                         = 0x112A
    fromEnum UDI_TA                         = 0x112B
    fromEnum AudioVideo                     = 0x112C
#if !defined(mingw32_HOST_OS)
    fromEnum SIMAccess                      = 0x112D
    fromEnum PhonebookAccessPCE             = 0x112E
    fromEnum PhonebookAccessPSE             = 0x112F
    fromEnum PhonebookAccess                = 0x1130
#endif
    fromEnum PnPInformation                 = 0x1200
    fromEnum GenericNetworking              = 0x1201
    fromEnum GenericFileTransfer            = 0x1202
    fromEnum GenericAudio                   = 0x1203
    fromEnum GenericTelephony               = 0x1204
#if !defined(mingw32_HOST_OS)
    fromEnum UPnP                           = 0x1205
    fromEnum UPnP_IP                        = 0x1206
    fromEnum ESDP_UPnP_IP_PAN               = 0x1300
    fromEnum ESDP_UPnP_IP_LAP               = 0x1301
    fromEnum ESDP_UPnP_L2CAP                = 0x1302
    fromEnum VideoSource                    = 0x1303
    fromEnum VideoSink                      = 0x1304
    fromEnum VideoDistribution              = 0x1305
#if defined(linux_HOST_OS)
    fromEnum HDP                            = 0x1400
    fromEnum HDPSource                      = 0x1401
    fromEnum HDPSink                        = 0x1402
    fromEnum GenericAttribute               = 0x1801
    fromEnum AppleAgentService              = 0x2112
#endif
#endif

    toEnum 0x1000 = ServiceDiscoveryServer
    toEnum 0x1001 = BrowseGroupDescriptor
    toEnum 0x1002 = PublicBrowseGroup
    toEnum 0x1101 = SerialPort
    toEnum 0x1102 = LANAccessUsingPPP
    toEnum 0x1103 = DialupNetworking
    toEnum 0x1104 = IrMCSync
    toEnum 0x1105 = OBEXObjectPush
    toEnum 0x1106 = OBEXFileTransfer
    toEnum 0x1107 = IrMCSyncCommand
    toEnum 0x1108 = Headset
    toEnum 0x1109 = CordlessTelephony
    toEnum 0x110A = AudioSource
    toEnum 0x110B = AudioSink
    toEnum 0x110C = AVRemoteControlTarget
    toEnum 0x110D = AdvancedAudioDistribution
    toEnum 0x110E = AVRemoteControl
    toEnum 0x110F = VideoConferencing
    toEnum 0x1110 = Intercom
    toEnum 0x1111 = Fax
    toEnum 0x1112 = HeadsetAudioGateway
    toEnum 0x1113 = WAP
    toEnum 0x1114 = WAPClient
    toEnum 0x1115 = PANU
    toEnum 0x1116 = NAP
    toEnum 0x1117 = GN
    toEnum 0x1118 = DirectPrinting
    toEnum 0x1119 = ReferencePrinting
    toEnum 0x111A = Imaging
    toEnum 0x111B = ImagingResponder
    toEnum 0x111C = ImagingAutomaticArchive
    toEnum 0x111D = ImagingReferencedObjects
    toEnum 0x111E = Handsfree
    toEnum 0x111F = HandsfreeAudioGateway
    toEnum 0x1120 = DirectPrintingReferenceObjects
    toEnum 0x1121 = ReflectedUI
    toEnum 0x1122 = BasicPrinting
    toEnum 0x1123 = PrintingStatus
    toEnum 0x1124 = HumanInterfaceDevice
    toEnum 0x1125 = HardcopyCableReplacement
    toEnum 0x1126 = HCRPrint
    toEnum 0x1127 = HCRScan
    toEnum 0x1128 = CommonISDNAccess
    toEnum 0x1129 = VideoConferencingGW
    toEnum 0x112A = UDI_MT
    toEnum 0x112B = UDI_TA
    toEnum 0x112C = AudioVideo
#if !defined(mingw32_HOST_OS)
    toEnum 0x112D = SIMAccess
    toEnum 0x112E = PhonebookAccessPCE
    toEnum 0x112F = PhonebookAccessPSE
    toEnum 0x1130 = PhonebookAccess
#endif
    toEnum 0x1200 = PnPInformation
    toEnum 0x1201 = GenericNetworking
    toEnum 0x1202 = GenericFileTransfer
    toEnum 0x1203 = GenericAudio
    toEnum 0x1204 = GenericTelephony
#if !defined(mingw32_HOST_OS)
    toEnum 0x1205 = UPnP
    toEnum 0x1206 = UPnP_IP
    toEnum 0x1300 = ESDP_UPnP_IP_PAN
    toEnum 0x1301 = ESDP_UPnP_IP_LAP
    toEnum 0x1302 = ESDP_UPnP_L2CAP
    toEnum 0x1303 = VideoSource
    toEnum 0x1304 = VideoSink
    toEnum 0x1305 = VideoDistribution
#if defined(linux_HOST_OS)
    toEnum 0x1400 = HDP
    toEnum 0x1401 = HDPSource
    toEnum 0x1402 = HDPSink
    toEnum 0x1801 = GenericAttribute
    toEnum 0x2112 = AppleAgentService
#endif
#endif
    toEnum unmatched = error $ "UUIDServiceClass.toEnum: Cannot match " ++ show unmatched