discard """
  exitcode: 0
  output: '''Loading the game data.
Testing findSkillIndex.'''
"""

import ../../src/[game]

## Temporary code
import std/tables
import ../../src/[items, careers, factions]
if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
# end of temporary code

echo "Loading the game data."
loadData("../bin/data/game.dat")

echo "Testing findSkillIndex."
try:
  assert findSkillIndex(skillName = "Piloting") == 1
except AssertionDefect:
  writeLine(stderr, "Failed to find the index of the skill.")
try:
  assert findSkillIndex(skillName = "sdfwerwerwe") == 0
except AssertionDefect:
  writeLine(stderr, "Failed to not find the index of the non-existing skill.")
