import os

switch("noMain")
switch("noLinking")
switch("nimcache", "obj")

task debug, "builds the project in debug mode":
  switch("define", "debug")
  switch("styleCheck", "error")
  switch("spellSuggest", "auto")
  switch("verbosity", "2")
  setCommand("c", "src" & DirSep & "steamsky.nim")

task release, "builds the project in release mode":
  switch("define", "release")
  setCommand("c", "src" & DirSep & "steamsky.nim")
