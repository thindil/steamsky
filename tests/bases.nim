import std/paths
import unittest2
import ../src/careers
include ../src/bases

suite "Unit tests for bases module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat".Path)
  loadItems("bin/data/items.dat".Path)
  loadCareers("bin/data/careers.dat".Path)
  loadFactions("bin/data/factions.dat".Path)
  loadBasesTypes("bin/data/bases.dat".Path)
  checkpoint "Setting the tests."
  playerShip.skyX = 1
  playerShip.skyY = 1
  playerShip.crew = @[]
  playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
      homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1, 1,
      1, 0, 0], order: talk, loyalty: 100, skills: @[SkillInfo(index: 4, level: 4,
      experience: 0)], attributes: @[MobAttributeRecord(level: 3, experience: 0),
      MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
      experience: 0), MobAttributeRecord(level: 3, experience: 0)], health: 100))
  skyMap[1][1].baseIndex = 1
  skyBases[1].population = 100
  skyBases[1].baseType = "1"
  skyBases[1].owner = "POLEIS"
  gameDate = DateRecord(year: 1600, month: 1, day: 1, hour: 8, minutes: 0)
  skyBases[1].recruits = @[]
  resetReputations()

  test "Generate name for a base.":
    check:
      generateBaseName("POLEIS").len() > 0

  test "Gaining reputation in a base.":
    skyBases[1].reputation = ReputationData(level: 1, experience: 1)
    gainRep(1, 1)
    check:
      skyBases[1].reputation.experience == 2

  test "Losing reputation in a base.":
    gainRep(1, -1)
    check:
      skyBases[1].reputation.experience == 1

  test "Raising a price in a base.":
    var price: Natural = 100
    countPrice(price, 0, false)
    check:
      price > 100

  test "Lowering a price in a base.":
    var price: Natural = 100
    countPrice(price, 0)
    check:
      price < 100

  test "Updating population in a base.":
    updatePopulation()

  test "Generating recruits in a base with positive reputation.":
    skyBases[1].reputation.level = 10
    generateRecruits()
    check:
      skyBases[1].recruits.len > 0

  test "Generating recruits in a base with negative reputation.":
    skyBases[1].recruits = @[]
    skyBases[1].recruitDate = DateRecord(year: 0, month: 0, day: 0, hour: 0, minutes: 0)
    skyBases[1].reputation.level = -50
    generateRecruits()
    check:
      skyBases[1].recruits.len > 0

  test "Generating recruits in a base with zero reputation":
    skyBases[1].recruits = @[]
    skyBases[1].recruitDate = DateRecord(year: 0, month: 0, day: 0, hour: 0, minutes: 0)
    skyBases[1].reputation.level = 0
    generateRecruits()
    check:
      skyBases[1].recruits.len > 0

  test "Updating prices in a game.":
    updatePrices()

  test "Get the size of a base population":
    check:
      getBasePopulation(baseIndex = 1) == small
