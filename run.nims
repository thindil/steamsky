#!/usr/bin/env -S nim --hints:off

import std/[os, strutils]

cd "bin"
try:
  exec getCurrentDir() & DirSep & "steamsky" & ExeExt &
    (if paramCount() > 2: " " & commandLineParams()[2 .. paramCount() - 1].join(" ") else: "")
except OSError:
  quit QuitFailure
