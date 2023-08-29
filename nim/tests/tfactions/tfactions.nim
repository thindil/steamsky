discard """
  exitcode: 0
  output: '''Loading the game data.
Testing getReputation.
Testing isFriendly.
Testing getRandomFaction.'''
"""

import std/tables
import ../../src/[factions, careers, game, items]

echo "Loading the game data."
if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")

echo "Testing getReputation."
try:
  assert getReputation("POLEIS", "POLEIS") == 0
except AssertionDefect:
  writeLine(stderr, "Failed to get reputation for the same faction.")
try:
  assert getReputation("POLEIS", "PIRATES") == -10
except AssertionDefect:
  writeLine(stderr, "Failed to get reputation for enemy factions.")

echo "Testing isFriendly."
try:
  assert isFriendly("POLEIS", "INDEPENDENT")
except AssertionDefect:
  writeLine(stderr, "Failed to check if friendly factions are friendly.")
try:
  assert not isFriendly("POLEIS", "PIRATES")
except AssertionDefect:
  writeLine(stderr, "Failed to check if enemies factions are unfriendly.")

echo "Testing getRandomFaction."
let factionIndex = getRandomFaction()
try:
  assert factionIndex.len > 0
except AssertionDefect:
  writeLine(stderr, "Failed to get random faction index.")
try:
  assert factionIndex in factionsList
except AssertionDefect:
  writeLine(stderr, "Failed to get existing random faction index.")
