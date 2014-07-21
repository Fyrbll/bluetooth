module Network.Bluetooth.Types where

data SDPInfo = SDPAttributes {
    serviceName :: String
  , description :: String
  , providerName :: String
} | SDPNoInfo
  deriving (Read, Show)