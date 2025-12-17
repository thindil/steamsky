import std/paths
import unittest2
import ../src/[careers, factions]
include ../src/basescargo

suite "Unit tests for basescargo module.":
  checkpoint "Loading the game data."

  loadData("bin/data/game.dat".Path)
  loadItems("bin/data/items.dat".Path)
  loadCareers("bin/data/careers.dat".Path)
  loadFactions("bin/data/factions.dat".Path)
  loadBasesTypes("bin/data/bases.dat".Path)

  skyBases[1].reputation = ReputationData(level: 1, experience: 1)
  playerShip.skyX = 1
  playerShip.skyY = 1
  playerShip.crew = @[]
  playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
      homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1,
      1, 1, 0, 0], order: talk, loyalty: 100, skills: @[SkillInfo(index: 4,
      level: 4, experience: 0)], attributes: @[MobAttributeRecord(level: 3,
      experience: 0), MobAttributeRecord(level: 3, experience: 0),
      MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
      experience: 0)], health: 100))
  skyMap[1][1].baseIndex = 1
  skyBases[1].population = 100
  skyBases[1].baseType = "1"
  skyBases[1].owner = "POLEIS"
  skyBases[1].size = big
  gameDate = DateRecord(year: 1600, month: 1, day: 1, hour: 8, minutes: 0)

  test "Generate a base's cargo.":
    generateCargo()
    check:
      skyBases[1].cargo.len > 0

  test "Find an existing item in a base's cargo":
    skyBases[1].cargo = @[]
    generateCargo()
    check:
      findBaseCargo(1, quality = normal) == 0

  test "Not find an existing item in a base's cargo":
    check:
      findBaseCargo(40, quality = normal) == -1

  test "Not find an non-existing item in a base's cargo":
    check:
      findBaseCargo(490, quality = normal) == -1

  test "Count the free cargo in a base's cargo":
    skyBases[1].cargo = @[]
    check:
      countFreeCargo(baseIndex = 1) == 128

  test "Remove an item from a base's cargo with protoIndex":
    skyBases[1].cargo = @[]
    generateCargo()
    let
      amount = skyBases[1].cargo[0].amount - 1
      protoIndex = skyBases[1].cargo[0].protoIndex
    updateBaseCargo(protoIndex, -1, quality = normal)
    check:
      skyBases[1].cargo[0].amount == amount

  test "Remove an item from a base's cargo with amount":
    let
      amount = skyBases[1].cargo[0].amount - 1
    updateBaseCargo(cargoIndex = 0, amount = -1, quality = normal)
    check:
      skyBases[1].cargo[0].amount == amount
