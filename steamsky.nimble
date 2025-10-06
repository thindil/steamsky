import os

# Package

version = "11.7"
author = "Bartek thindil Jasicki"
description = "A roguelike game with steampunk setting"
license = "GPL-3"
srcDir = "src"
bin = @["steamsky"]
binDir = "bin"


# Dependencies

requires "nim >= 2.2.0"
requires "contracts >= 0.2.2"
requires "nimalyzer >= 0.9"
requires "unittest2"

# Tasks

task debug, "builds the game in debug mode":
  exec "nim c -d:debug --app:gui --styleCheck:hint --spellSuggest:auto --errorMax:0 --outdir:" &
      binDir & " " & srcDir & DirSep & "steamsky.nim"

task release, "builds the project in release mode":
  exec "nim c -d:release --app:gui --passC:-flto --passL:-Wl,-s --passL:-Wl,-rpath,$ORIGIN/lib --outdir:" & binDir & " " &
      srcDir & DirSep & "steamsky.nim"

task releasewindows, "builds the project in release mode for Windows 64-bit on Linux":
  exec "nim c -d:mingw --app:gui --os:windows --cpu:amd64 --amd64.windows.gcc.exe:x86_64-w64-mingw32-gcc --amd64.windows.gcc.linkerexe=x86_64-w64-mingw32-gcc -d:release --passC:-flto --passL:-Wl,-s --passL:-mwindows --outdir:" & binDir & " " & srcDir & DirSep & "steamsky.nim"

task fullrelease, "builds the project in release mode for all supported platforms":
  exec "others/build.nims x86_64-linux-gnu"
  exec "others/build.nims x86_64-windows"

task debugwindows, "builds the project in debug mode for Windows 64-bit on Linux":
  exec "nim c -d:mingw --app:gui --os:windows --cpu:amd64 --amd64.windows.gcc.exe:x86_64-w64-mingw32-gcc --amd64.windows.gcc.linkerexe=x86_64-w64-mingw32-gcc -d:debug --passL:-mwindows --outdir:" & binDir & " " & srcDir & DirSep & "steamsky.nim"

task analyze, "builds the project in analyze mode (release with nimprofiler support)":
  exec "nim c -d:release --profiler:on --stackTrace:on --passC:-flto --passL:-Wl,-s --outdir:" & binDir & " " &
      srcDir & DirSep & "steamsky.nim"

task docs, "builds the project's documentation":
  exec "nim doc --project --outdir:htmldocs src" & DirSep & "steamsky.nim"

task newdebug, "builds the new game UI in debug mode (temporary task)":
  exec "nim c -d:debug --app:gui --styleCheck:hint --spellSuggest:auto --errorMax:0 --outdir:" &
      binDir & " --passl:\"-I/opt/include -I/opt/include/SDL2 -L/opt/lib -lm -lSDL2 -lSDL2_image\" --passc:\"-I/opt/include -Isrc/newui/nuklear\" " & srcDir & DirSep & "newsteamsky.nim"
