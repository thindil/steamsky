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
      "x86_64-linux-gnu"
    else:
      "x86_64-windows"
  else:
    paramStr(3)

# Check if correct target was set
if target notin ["x86_64-linux-gnu", "x86_64-windows"]:
  echo "Invalid compilation target. Allowed options are x86_64-linux-gnu and x86_64-windows"
  quit QuitFailure

# Clean and compile the game
rmDir(nimCacheDir().parentDir() & DirSep & "build_r")
if target == "x86_64-linux-gnu":
  exec "nimble release -y"
else:
  exec "nimble releasewindows -y"
let dirName =
  if target == "x86_64-linux-gnu":
    "release" & DirSep & "steamsky-linux" & DirSep
  else:
    "release" & DirSep & "steamsky-windows" & DirSep
let extension =
  if target == "x86_64-linux-gnu":
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
echo "Files and directories copied."
exec "gprclean -P steamsky.gpr --target=" & target
rmDir("nim" & DirSep & "obj")
mkDir("nim" & DirSep & "obj")
