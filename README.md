# bluetooth

This library abstracts low-level Bluetooth utilities on each of the [Tier 1](https://ghc.haskell.org/trac/ghc/wiki/Platforms#Tier1platforms) GHC platforms. This is currently a work in progress, so do not expect anything to work yet.

## Checklist
- [ ] Windows x86_64<sup>1</sup>
- [ ] OS X<sup>2</sup>
- [ ] Linux (in progress)
- [ ] FreeBSD

---
<sup>1</sup> Many of the [Winsock](http://msdn.microsoft.com/en-us/library/ms740673.aspx) headers (e.g., `ws2bth2.h`) appear to be available in [MinGW-w64](http://mingw-w64.sourceforge.net/) but not [MinGW](http://www.mingw.org/). Although MinGW-w64 supports both 32- and 64-bit Windows, only 64-bit Windows GHC is packaged with MinGW-w64 at the moment, so `bluetooth` does not support 32-bit Windows.

<sup>2</sup>I do not own a Mac, so I am unable to test OS X at the moment.
