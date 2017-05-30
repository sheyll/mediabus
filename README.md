# Multimedia streaming on top of Conduit

[![Build Status](https://travis-ci.org/sheyll/mediabus.svg?branch=master)](https://travis-ci.org/sheyll/mediabus)
[![Hackage](https://img.shields.io/hackage/v/mediabus.svg)](http://hackage.haskell.org/package/mediabus)

Functions and types for multimedia streaming on top of conduit.

It is supposed to be a minimalistic rip-off of gstreamer.

Extra functionality is contained in external packages, usually called
_mediabus-foo_.

Special care has been taken to ensure that the functions in this package do not
have space leaks.

Also, this package supports concurrent stream processing.
