import ../src/careers
import unittest2
include ../src/mobs

suite "Unit tests for mobs module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat")
  loadItems("bin/data/items.dat")
  loadCareers("bin/data/careers.dat")
  loadFactions("bin/data/factions.dat")
  loadMobs("bin/data/mobs.dat")

  test "Generating a random mob.":
    let newMob = generateMob(5, "POLEIS")
    check:
      newMob.attributes[0].level == 2
      newMob.orderTime == 15
