{-# LANGUAGE CPP #-}
module Network.Bluetooth.SDP (
#if   defined(mingw32_HOST_OS)
      module Network.Bluetooth.Windows.SDP
#elif defined(darwin_HOST_OS)
      module Network.Bluetooth.OSX.SDP
#elif defined(linux_HOST_OS)
      module Network.Bluetooth.Linux.SDP
#elif defined(freebsd_HOST_OS)
      module Network.Bluetooth.FreeBSD.SDP
#endif
    ) where

#if   defined(mingw32_HOST_OS)
import Network.Bluetooth.Windows.SDP
#elif defined(darwin_HOST_OS)
import Network.Bluetooth.OSX.SDP
#elif defined(linux_HOST_OS)
import Network.Bluetooth.Linux.SDP
#elif defined(freebsd_HOST_OS)
import Network.Bluetooth.FreeBSD.SDP
#endif