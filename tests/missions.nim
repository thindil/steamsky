import std/paths
import ../src/[careers, factions, items, reputation, shipmodules]
import unittest2
include ../src/missions

suite "Unit tests for missions module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat")
  loadItems("bin/data/items.dat".Path)
  loadCareers("bin/data/careers.dat")
  loadFactions("bin/data/factions.dat".Path)
  loadModules("bin/data/shipmodules.dat")

  playerShip.skyX = 1
  playerShip.skyY = 1
  playerShip.crew = @[]
  playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
      homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1,
      1, 1, 0, 0], order: talk, loyalty: 100, skills: @[SkillInfo(index: 4,
      level: 4, experience: 0)], attributes: @[MobAttributeRecord(level: 3,
      experience: 0), MobAttributeRecord(level: 3, experience: 0),
      MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
      experience: 0)]))
  playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
      homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 0, 1, 1,
      1, 0, 0], order: gunner, loyalty: 100))
  playerShip.modules = @[]
  playerShip.modules.add(ModuleData(mType: cargoRoom, protoIndex: 7,
      durability: 100))
  playerShip.cargo = @[]
  playerShip.cargo.add(InventoryData(protoIndex: 1, amount: 100,
      durability: 100))
  skyMap[1][1].baseIndex = 1
  skyBases[1] = BaseRecord(skyX: 1, skyY: 1, owner: "POLEIS")
  resetReputations()

  test "Deleting a mission.":
    acceptedMissions = @[]
    acceptedMissions.add(y = MissionData(mType: explore, time: 1, targetX: 1,
        targetY: 1, reward: 1, startBase: 1, finished: true, multiplier: 0.0, target: 0))
    deleteMission(0, false)
    check:
      acceptedMissions.len == 0

  test "Generating missions in a base.":
    skyBases[1].missionsDate = DateRecord(year: 0, month: 0, day: 0, hour: 0, minutes: 0)
    generateMissions()

  test "Update accepted missions":
    acceptedMissions = @[]
    acceptedMissions.add(y = MissionData(mType: explore, time: 10, targetX: 1,
        targetY: 1, reward: 1, startBase: 1, finished: true, multiplier: 0.0, target: 0))
    updateMissions(8)
    check:
      acceptedMissions[0].time == 2

  test "Remove accepted mission":
    updateMissions(2)
    check:
      acceptedMissions.len == 0

  test "Getting a mission's type.":
    check:
      getMissionType(patrol) == "Patrol area"

  test "Updating a mission.":
    acceptedMissions = @[]
    acceptedMissions.add(y = MissionData(mType: explore, time: 10, targetX: 1,
        targetY: 1, reward: 1, startBase: 1, finished: true, multiplier: 0.0, target: 0))
    updateMission(0)
    check:
      acceptedMissions[0].finished
