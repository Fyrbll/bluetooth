{-# LANGUAGE CPP #-}
module Network.Bluetooth.Protocol (
#if   defined(mingw32_HOST_OS)
      module Network.Bluetooth.Windows.Protocol
#elif defined(darwin_HOST_OS)
      module Network.Bluetooth.OSX.Protocol
#elif defined(linux_HOST_OS)
      module Network.Bluetooth.Linux.Protocol
#elif defined(freebsd_HOST_OS)
      module Network.Bluetooth.FreeBSD.Protocol
#endif
    ) where

#if   defined(mingw32_HOST_OS)
import Network.Bluetooth.Windows.Protocol
#elif defined(darwin_HOST_OS)
import Network.Bluetooth.OSX.Protocol
#elif defined(linux_HOST_OS)
import Network.Bluetooth.Linux.Protocol
#elif defined(freebsd_HOST_OS)
import Network.Bluetooth.FreeBSD.Protocol
#endif