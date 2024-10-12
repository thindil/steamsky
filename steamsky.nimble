import os

# Package

version = "10.4"
author = "Bartek thindil Jasicki"
description = "A roguelike game with steampunk setting"
license = "GPL-3"
srcDir = "src"
bin = @["steamsky"]
binDir = "bin"


# Dependencies

requires "nim >= 2.0.0"
requires "contracts >= 0.2.2"
requires "nimalyzer >= 0.9"
requires "unittest2"

# Tasks

task debug, "builds the game in debug mode":
  exec "nim c -d:debug --app:gui --styleCheck:hint --spellSuggest:auto --errorMax:0 --outdir:" &
      binDir & " " & srcDir & DirSep & "steamsky.nim"

task release, "builds the project in release mode":
  exec "nim c -d:release --app:gui --passc:-flto --passl:-s --passl:-Wl,-rpath,$ORIGIN/libs --outdir:" & binDir & " " &
      srcDir & DirSep & "steamsky.nim"

task releasewindows, "builds the project in release mode for Windows 64-bit":
  exec "nim c -d:mingw --app:gui --os:windows --cpu:amd64 --amd64.windows.gcc.exe:x86_64-w64-mingw32-gcc --amd64.windows.gcc.linkerexe=x86_64-w64-mingw32-gcc -d:release --passc:-flto --passl:-s --passl:-mwindows --outdir:" & binDir & " " & srcDir & DirSep & "steamsky.nim"

task analyze, "builds the project in analyze mode (release with nimprofiler support)":
  exec "nim c -d:release --profiler:on --stackTrace:on --passc:-flto --passl:-s --outdir:" & binDir & " " &
      srcDir & DirSep & "steamsky.nim"
