import os

task debug, "builds the project in debug mode":
  switch("noMain")
  switch("noLinking")
  switch("nimcache", "obj")
  switch("define", "debug")
  switch("styleCheck", "error")
  switch("spellSuggest", "auto")
  switch("verbosity", "2")
  setCommand("c", "src" & DirSep & "steamsky.nim")

task release, "builds the project in release mode":
  switch("noMain")
  switch("noLinking")
  switch("nimcache", "obj")
  switch("define", "release")
  setCommand("c", "src" & DirSep & "steamsky.nim")

task windows, "builds the project in release mode for Windows":
  switch("noMain")
  switch("noLinking")
  switch("nimcache", "obj")
  switch("define", "release")
  switch("define", "mingw")
  switch("os", "windows")
  switch("cpu", "amd64")
  switch("amd64.windows.gcc.exe", "x86_64-w64-mingw32-gcc")
  setCommand("c", "src" & DirSep & "steamsky.nim")
