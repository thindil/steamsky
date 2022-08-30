#!/bin/sh

cd nim
nim c -d:mingw -d:release src/nimtest.nim
cd ..
gprbuild -p -P steamsky.gpr -XMode=release -XOS=Windows --target=x86_64-windows -largs -L/opt/lib
