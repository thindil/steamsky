#!/usr/bin/env -S nim --hints:off

import std/strutils

if not fileExists("steamsky.nimble"):
  echo "This script must be run in the directory where steamsky.nimble file is"
  quit QuitFailure

# Run Nim tests
for i in 1..parseInt(paramStr(paramCount())):
  echo i
  for file in listFiles("tests"):
    if file.endsWith("nim"):
      exec "nim c --verbosity:0 --NimblePath:/root/.nimble/pkgs2 -r " & file & " -v"
