{-# LANGUAGE CPP #-}
module Network.Bluetooth.Device (
#if   defined(mingw32_HOST_OS)
      module Network.Bluetooth.Windows.Device
#elif defined(darwin_HOST_OS)
      module Network.Bluetooth.OSX.Device
#elif defined(linux_HOST_OS)
      module Network.Bluetooth.Linux.Device
#elif defined(freebsd_HOST_OS)
      module Network.Bluetooth.FreeBSD.Device
#endif
    ) where

#if   defined(mingw32_HOST_OS)
import Network.Bluetooth.Windows.Device
#elif defined(darwin_HOST_OS)
import Network.Bluetooth.OSX.Device
#elif defined(linux_HOST_OS)
import Network.Bluetooth.Linux.Device
#elif defined(freebsd_HOST_OS)
import Network.Bluetooth.FreeBSD.Device
#endif