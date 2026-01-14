import std/paths
import ../src/[careers, crafts, factions, items, mobs, shipmodules, ships]
import unittest2
include ../src/stories2

suite "Unit tests for stories2 module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat".Path)
  loadItems("bin/data/items.dat".Path)
  loadCareers("bin/data/careers.dat".Path)
  loadFactions("bin/data/factions.dat".Path)
  loadModules("bin/data/shipmodules.dat".Path)
  loadRecipes("bin/data/recipes.dat".Path)
  loadMobs("bin/data/mobs.dat".Path)
  loadShips("bin/data/ships.dat".Path)
  loadStories("bin/data/stories.dat".Path)

  playerShip.crew = @[]
  playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
      homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1,
      1, 1, 0, 0], order: talk, loyalty: 100, skills: @[SkillInfo(index: 4,
      level: 4, experience: 0)], attributes: @[MobAttributeRecord(level: 3,
      experience: 0), MobAttributeRecord(level: 3, experience: 0),
      MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
      experience: 0)]))
  while currentStory.index.len == 0:
    startStory("Undead", dropItem)
    if currentStory.index.len > 0:
      break
  var amount: Natural = getRandom(min = 1, max = 20)
  while amount > 0:
    skyBases[getRandom(min = skyBases.low, max = skyBases.high)].known = true
    amount.dec

  test "Progress the current story.":
    discard progressStory()
