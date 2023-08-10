discard """
  exitcode: 0
  output: '''Loading the game data.
Testing getModuleType.'''
"""

import std/tables
import ../../src/[careers, factions, game, items, shipmodules]

echo "Loading the game data."
if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
  loadModules("../bin/data/shipmodules.dat")

echo "Testing getModuleType."
try:
  assert getModuleType(1) == "Hull"
except AssertionDefect:
  echo "Failed to get the type of the module."
try:
  assert getModuleType(6) == "Alchemy lab"
except AssertionDefect:
  echo "Failed to get the type of the module with two words."
