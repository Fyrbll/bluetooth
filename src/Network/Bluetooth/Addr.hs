{-# LANGUAGE CPP #-}
module Network.Bluetooth.Addr (
#if   defined(mingw32_HOST_OS)
      module Network.Bluetooth.Windows.Addr
#elif defined(darwin_HOST_OS)
      module Network.Bluetooth.OSX.Addr
#elif defined(linux_HOST_OS)
      module Network.Bluetooth.Linux.Addr
#elif defined(freebsd_HOST_OS)
      module Network.Bluetooth.FreeBSD.Addr
#endif
    ) where

#if   defined(mingw32_HOST_OS)
import Network.Bluetooth.Windows.Addr
#elif defined(darwin_HOST_OS)
import Network.Bluetooth.OSX.Addr
#elif defined(linux_HOST_OS)
import Network.Bluetooth.Linux.Addr
#elif defined(freebsd_HOST_OS)
import Network.Bluetooth.FreeBSD.Addr
#endif