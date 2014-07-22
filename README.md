# `bluetooth`

This library abstracts low-level C Bluetooth utilities on each of the [Tier 1](https://ghc.haskell.org/trac/ghc/wiki/Platforms#Tier1platforms) GHC platforms, similar to Java's [BlueCove](https://code.google.com/p/bluecove/) and Python's [PyBluez](https://code.google.com/p/pybluez/). This is currently a work in progress, so do not expect anything to work yet.

## Checklist
- [ ] Windows x86_64 ([Winsock](http://msdn.microsoft.com/en-us/library/ms740673.aspx))<sup>1</sup>

  - [ ] Port the [Winsock examples](http://msdn.microsoft.com/en-us/library/windows/desktop/ms737889%28v=vs.85%29.aspx)

- [ ] OS X ([IOBluetooth](https://developer.apple.com/library/mac/navigation/#section=Frameworks&topic=IOBluetooth))<sup>2</sup>

  - [ ] Port each of the [OSX Bluetooth examples](http://ryanglscott.github.io/Bluetooth.zip)

- [ ] Linux ([BlueZ](http://www.bluez.org/))

  - [ ] Wrap all needed C functionality with `c2hs`
  - [ ] [Device inquiry](http://web.archive.org/web/20071028165809/http://www.btessentials.com/examples/bluez/simplescan.c)
  - [ ] [RFCOMM server](http://web.archive.org/web/20071028165749/http://www.btessentials.com/examples/bluez/rfcomm-server.c)
  - [ ] [RFCOMM client](http://web.archive.org/web/20071028165739/http://www.btessentials.com/examples/bluez/rfcomm-client.c)
  - [ ] [L2CAP server](http://web.archive.org/web/20071028165729/http://www.btessentials.com/examples/bluez/l2cap-server.c)
  - [ ] [L2CAP client](http://web.archive.org/web/20071028165724/http://www.btessentials.com/examples/bluez/l2cap-client.c)
  - [ ] [SDP register](http://web.archive.org/web/20071028165754/http://www.btessentials.com/examples/bluez/sdp-register.c)
  - [ ] [SDP search](http://web.archive.org/web/20071028165759/http://www.btessentials.com/examples/bluez/sdp-search.c)
  - [ ] [RFCOMM server select](http://web.archive.org/web/20071028165744/http://www.btessentials.com/examples/bluez/rfcomm-server-select.c)
  - [ ] [RFCOMM client select](http://web.archive.org/web/20071028165734/http://www.btessentials.com/examples/bluez/rfcomm-client-select.c)
  - [ ] [Set L2CAP flush timeout](http://web.archive.org/web/20071028165804/http://www.btessentials.com/examples/bluez/set-flush-to.c)

- [ ] FreeBSD ([`netgraph`](http://www.freebsd.org/cgi/man.cgi?query=netgraph&sektion=4))

## Prerequisites
Aside from having a Bluetooth-enabled computer, you must have some software installed before installing the `bluetooth` package.

### Linux
You need to have the BlueZ development libraries installed.

#### Debian/Ubuntu
```bash
apt-get install bluez libbluetooth-dev
```

#### Fedora
```bash
yum install bluez-libs-devel
```

Alternatively, you can install from [source](http://www.bluez.org/download/).

---
<sup>1</sup> Many of the Winsock headers (e.g., `ws2bth2.h`) appear to be available in [MinGW-w64](http://mingw-w64.sourceforge.net/) but not [MinGW](http://www.mingw.org/). Although MinGW-w64 supports both 32- and 64-bit Windows, only 64-bit Windows GHC is packaged with MinGW-w64 at the moment, so the `bluetooth` package does not support 32-bit Windows.

<sup>2</sup> I do not own a Mac, so I am unable to test OS X at the moment.
