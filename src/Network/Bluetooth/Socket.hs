{-# LANGUAGE CPP #-}
module Network.Bluetooth.Socket (
#if     defined(mingw32_HOST_OS)
      module Network.Bluetooth.Windows.Socket
#elif defined(darwin_HOST_OS)
      module Network.Bluetooth.OSX.Socket
#elif defined(linux_HOST_OS)
      module Network.Bluetooth.Linux.Socket
#elif defined(freebsd_HOST_OS)
      module Network.Bluetooth.FreeBSD.Socket
#endif
    ) where

#if     defined(mingw32_HOST_OS)
import Network.Bluetooth.Windows.Socket
#elif defined(darwin_HOST_OS)
import Network.Bluetooth.OSX.Socket
#elif defined(linux_HOST_OS)
import Network.Bluetooth.Linux.Socket
#elif defined(freebsd_HOST_OS)
import Network.Bluetooth.FreeBSD.Socket
#endif