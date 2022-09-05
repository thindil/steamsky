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
