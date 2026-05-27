#!/usr/bin/env -S nim --hints:off

import std/[distros, os]

# Check if the script was started from the proper location (root directory)
if not fileExists("steamsky.nimble"):
  echo "This script must be run in the directory where steamsky.nimble file is"
  quit QuitFailure

# Set the target for the compilation. If no arguments, use system default
let target =
  if paramCount() == 2:
    if detectOs(Linux):
      "linux"
    elif detectOs(FreeBSD):
      "freebsd"
    else:
      "windows"
  else:
    paramStr(3)

# Check if correct target was set
if target notin ["linux", "windows", "freebsd"]:
  echo "Invalid compilation target. Allowed options are linux, freebsd and windows"
  quit QuitFailure

# Clean and compile the game
rmDir(nimCacheDir().parentDir() & DirSep & "build_r")
if target in ["linux", "freebsd"]:
  exec "nimble release -y"
else:
  exec "nimble releasewindows -y"
let dirName =
  case target
  of "linux":
    "release" & DirSep & "steamsky-linux" & DirSep
  of "windows":
    "release" & DirSep & "steamsky-windows" & DirSep
  of "freebsd":
    "release" & DirSep & "steamsky-freebsd" & DirSep
  else:
    discard
let extension =
  if target in ["linux", "freebsd"]:
    ""
  else:
    ".exe"

# Copy all files and directories to release directory
echo "Copying files and directories ..."
mkDir(dirName)
cpFile("bin" & DirSep & "steamsky" & extension, dirName & "steamsky" & extension)
if detectOs(Linux):
  exec "chmod 777 " & dirName & "steamsky" & extension
cpDir("bin" & DirSep & "data", dirName & "data")
rmDir(dirName & "data" & DirSep & "mods")
rmDir(dirName & "data" & DirSep & "saves")
rmDir(dirname & "data" & DirSep & "themes")
cpDir("bin" & DirSep & "doc", dirName & "doc")
cpFile("README.md", dirName & "doc" & DirSep & "README.md")
if target != "windows":
  mkDir(dirName & DirSep & "lib")
case target
of "linux":
  cpFile("/usr/lib/libSDL2_image-2.0.so.0.800.12", dirName & DirSep &
      "lib/libSDL2_image.so")
  cpFile("/usr/lib/libSDL2-2.0.so.0.3200.10", dirName & DirSep &
      "lib/libSDL2.so")
of "windows":
  cpFile("/opt/SDL/SDL2_image.dll", dirName & DirSep & "SDL2_image.dll")
  cpFile("/opt/SDL/SDL2.dll", dirName & DirSep & "SDL2.dll")
else:
  discard
echo "Files and directories copied."
