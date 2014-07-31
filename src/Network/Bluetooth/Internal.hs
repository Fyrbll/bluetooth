{-# LANGUAGE CPP #-}
module Network.Bluetooth.Internal (
#if   defined(mingw32_HOST_OS)
      module Network.Bluetooth.Windows.Internal
#elif defined(darwin_HOST_OS)
      module Network.Bluetooth.OSX.Internal
#elif defined(linux_HOST_OS)
        BluetoothProtocol(..)
#elif defined(freebsd_HOST_OS)
      module Network.Bluetooth.FreeBSD.Internal
#endif
    ) where

#if   defined(mingw32_HOST_OS)
import Network.Bluetooth.Windows.Internal
#elif defined(darwin_HOST_OS)
import Network.Bluetooth.OSX.Internal
#elif defined(linux_HOST_OS)
import Network.Bluetooth.Linux.Internal (BluetoothProtocol(..))
#elif defined(freebsd_HOST_OS)
import Network.Bluetooth.FreeBSD.Internal
#endif