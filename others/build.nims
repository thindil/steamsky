#!/usr/bin/env -S nim --hints:off

import std/[distros, os]

# Check if the script was started from the proper location (root directory)
if not fileExists("steamsky.gpr"):
  echo "This script must be run in the directory where steamsky.gpr file is"
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
rmDir("nim" & DirSep & "obj")
mkDir("nim" & DirSep & "obj")
try:
  exec "gprclean -P steamsky.gpr --target=" & target
except:
  discard
withDir("nim"):
  if target == "x86_64-linux-gnu":
    exec "nim release"
  else:
    exec "nim windows"
if target == "x86_64-linux-gnu":
  exec "gprbuild -p -P steamsky.gpr -XMode=release -XOS=Unix --target=" & target
else:
  exec "gprbuild -p -P steamsky.gpr -XMode=release -XOS=Windows --target=" & target & " -largs -L/opt/lib"
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
