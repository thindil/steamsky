#!/usr/bin/env -S nim --hints:off

withDir "nim":
  try:
    exec "nim debug"
  except OSError:
    quit QuitFailure

try:
  exec "gprbuild -P steamsky.gpr"
except OSError:
  quit QuitFailure
