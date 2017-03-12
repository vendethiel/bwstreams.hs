#!/usr/bin/sh
stack build && cp path.txt /Users/ven/os/haskell/scplayer/.stack-work/install/x86_64-osx/lts-7.14/8.0.1/bin/ && stack exec scplayer
