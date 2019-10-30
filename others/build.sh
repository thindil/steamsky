#!/usr/bin/env sh

gprbuild -p steamsky.gpr -XMode=release
mkdir -p usr/bin
cp -r share usr
cp bin/steamsky usr/bin
cp -r bin/data usr/share
rm -r usr/share/data/mods
rm -r usr/share/data/saves
rm -r usr/share/data/themes
cp -r bin/doc usr/share
cp README.md usr/share/doc
