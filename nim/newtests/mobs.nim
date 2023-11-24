import ../src/[careers, factions, game, items, mobs, types]
import unittest2

suite "Unit tests for mobs module":

  checkpoint "Loading the game data."
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
  loadMobs("../bin/data/mobs.dat")

  test "Testing generateMob.":
    let newMob = generateMob(5, "POLEIS")
    check:
      newMob.attributes[0].level == 2
      newMob.orderTime == 15
