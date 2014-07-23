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
  avdtpProtocolUUID,
  _,
  cmtpProtocolUUID,
  _, _,
  mcapControlChannelProtocolUUID,
  mcapDataChannelProtocolUUID]
    = map fromShortUUID [0x0000..0x001F]
l2capProtocolUUID = fromShortUUID 0x0100