import std/paths
import ../src/[careers, factions, game, maps, items, shipscrew, types]
import unittest2

suite "Unit tests for shipscrew module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat".Path)
  loadItems("bin/data/items.dat".Path)
  loadCareers("bin/data/careers.dat".Path)
  loadFactions("bin/data/factions.dat".Path)

  playerCareer = "general"
  playerShip.skyX = 1
  playerShip.skyY = 1
  playerShip.crew = @[]
  playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
      homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1, 1,
      1, 0, 0], order: talk, loyalty: 100, skills: @[SkillInfo(index: 4, level: 4,
      experience: 0)], attributes: @[MobAttributeRecord(level: 3, experience: 0),
      MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
      experience: 0), MobAttributeRecord(level: 3, experience: 0)]))
  playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
      homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 0, 1, 1,
      0, 0, 0], order: gunner, loyalty: 100))
  skyMap[1][1].baseIndex = 1

  test "Generate a crew member's name.":
    check:
      generateMemberName('M', "POLEIS").len() > 0

  let oldMorale = playerShip.crew[0].morale[2]
  test "Raise the level of morale of the crew member.":
    let oldLevel = playerShip.crew[0].morale[1]
    updateMorale(playerShip, 0, 1)
    check:
      playerShip.crew[0].morale[2] == oldMorale + 1 or playerShip.crew[
        0].morale[1] == oldLevel + 1

  test "Lower the level of morale of the crew member.":
    updateMorale(playerShip, 0, -1)
    check:
      playerShip.crew[0].morale[2] == oldMorale

  test "Give an order to the player.":
    giveOrders(playerShip, 0, rest)
    check:
      playerShip.crew[0].order == talk

  test "Give an order to the crew member.":
    giveOrders(playerShip, 1, rest)
    check:
      playerShip.crew[1].order == rest

  test "Updating a crew members orders.":
    giveOrders(playerShip, 0, rest, -1, false)
    updateOrders(playerShip)
    check:
      playerShip.crew[0].order == talk

  test "Get skill level of the player.":
    check:
      getSkillLevel(playerShip.crew[0], 1) == 0

  test "Get the skill level of the crew member.":
    check:
      getSkillLevel(playerShip.crew[0], 4) == 7

  test "Find a member with the selected order.":
    check:
      findMember(talk) == 0

  test "Not find a member with the selected order.":
    check:
      findMember(defend) == -1

  test "Gaining experience in a skill.":
    gainExp(10, 4, 0)
    check:
      playerShip.crew[0].skills[0].experience == 10
