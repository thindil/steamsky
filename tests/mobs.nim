import ../src/careers
import unittest2
include ../src/mobs

suite "Unit tests for mobs module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat".Path)
  loadItems("bin/data/items.dat".Path)
  loadCareers("bin/data/careers.dat".Path)
  loadFactions("bin/data/factions.dat".Path)
  loadMobs("bin/data/mobs.dat".Path)

  test "Generating a random mob.":
    let newMob = generateMob(5, "POLEIS")
    check:
      newMob.attributes[0].level == 2
      newMob.orderTime == 15
