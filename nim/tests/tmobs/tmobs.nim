discard """
  exitcode: 0
  output: '''Loading the game data.
Testing generateMob.'''
"""

import std/tables
import ../../src/[careers, factions, game, items, mobs, types]

echo "Loading the game data."
if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
if protoMobsList.len == 0:
  loadMobs("../bin/data/mobs.dat")

echo "Testing generateMob."
let newMob = generateMob(5, "POLEIS")
try:
  assert newMob.attributes[0].level == 2
except AssertionDefect:
  writeLine(stderr, "Failed to set the mob's attributes.")
try:
  assert newMob.orderTime == 15
except AssertionDefect:
  writeLine(stderr, "Failed to set the mob's order's time.")
