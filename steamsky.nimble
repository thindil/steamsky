import os

# Package

version = "12.4"
author = "Bartek thindil Jasicki"
description = "A roguelike game with steampunk setting"
license = "GPL-3"
srcDir = "src"
bin = @["steamsky"]
binDir = "bin"


# Dependencies

requires "nim >= 2.2.6"
requires "contracts >= 0.2.2"
requires "nimalyzer >= 0.12.2"
requires "unittest2"

# Tasks

task olddebug, "builds the old game UI in debug mode":
  exec "nim c -d:debug --app:gui --styleCheck:hint --spellSuggest:auto --errorMax:0 --outdir:" &
      binDir & " " & srcDir & DirSep & "oldsteamsky.nim"

task release, "builds the project in release mode":
  exec "nim c -d:release --app:gui --passC:-flto --passL:-Wl,-s --passL:-Wl,--disable-new-dtags,-rpath,$ORIGIN/lib --outdir:" &
      binDir & " --passl:\"-lm -lSDL2 -lSDL2_image\" --passc:\"-Isrc/ui/nuklear\" " &
      srcDir & DirSep & "steamsky.nim"

task releasewindows, "builds the project in release mode for Windows 64-bit on Linux":
  exec "nim c -d:mingw --app:gui --os:windows --cpu:amd64 --amd64.windows.gcc.exe:x86_64-w64-mingw32-gcc --amd64.windows.gcc.linkerexe=x86_64-w64-mingw32-gcc -d:release --passC:-flto --passL:-Wl,-s --passL:-mwindows --outdir:" &
      binDir & " --passL:\"-L/usr/local/x86_64-w64-mingw32/lib -lm -lSDL2 -lSDL2_image -lmingw32 -lSDL2main -mwindows\" --passC:\"-I/usr/local/x86_64-w64-mingw32/include/ -Isrc/ui/nuklear\" " &
      srcDir & DirSep & "steamsky.nim"

task fullrelease, "builds the project in release mode for all supported platforms":
  exec "others/build.nims x86_64-linux-gnu"
  exec "others/build.nims x86_64-windows"

task olddebugwindows, "builds the old game UI in debug mode for Windows 64-bit on Linux":
  exec "nim c -d:mingw --app:gui --os:windows --cpu:amd64 --amd64.windows.gcc.exe:x86_64-w64-mingw32-gcc --amd64.windows.gcc.linkerexe=x86_64-w64-mingw32-gcc -d:debug --passL:-mwindows --outdir:" &
      binDir & " " & srcDir & DirSep & "oldsteamsky.nim"

task analyze, "builds the project in analyze mode (release with nimprofiler support)":
  exec "nim c -d:release --profiler:on --stackTrace:on --passC:-flto --passL:-Wl,-s --outdir:" &
      binDir & " " & srcDir & DirSep & "steamsky.nim"

task docs, "builds the project's documentation":
  exec "nim doc --project --outdir:htmldocs src" & DirSep & "steamsky.nim"

task debug, "builds the new game UI in debug mode (temporary task)":
  exec "nim c -d:debug --app:gui --styleCheck:hint --spellSuggest:auto --errorMax:0 --outdir:" &
      binDir & " --passl:\"-lm -lSDL2 -lSDL2_image\" --passc:\"-Isrc/ui/nuklear -Wno-int-conversion -Wno-incompatible-function-pointer-types\" " &
      srcDir & DirSep & "steamsky.nim"

task debugwindows, "builds the new game UI in debug mode for Windows 64-bit on Linux":
  exec "nim c -d:mingw --app:gui --os:windows --cpu:amd64 --amd64.windows.gcc.exe:x86_64-w64-mingw32-gcc --amd64.windows.gcc.linkerexe=x86_64-w64-mingw32-gcc -d:debug --outdir:" &
      binDir & " --passL:\"-L/usr/local/x86_64-w64-mingw32/lib -lm -lSDL2 -lSDL2_image -lmingw32 -lSDL2main -mwindows\" --passC:\"-I/usr/local/x86_64-w64-mingw32/include/ -Isrc/ui/nuklear\" " &
      srcDir & DirSep & "steamsky.nim"
