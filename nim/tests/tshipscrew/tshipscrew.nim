discard """
  exitcode: 0
"""

import std/tables
import ../../src/[careers, factions, game, maps, items, shipscrew, types]

if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")

assert generateMemberName('M', "POLEIS").len() > 0

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
    1, 0, 0], order: gunner, loyalty: 100))
skyMap[1][1].baseIndex = 1

let oldMorale = playerShip.crew[0].morale[2]
let oldLevel = playerShip.crew[0].morale[1]
updateMorale(playerShip, 0, 1)
assert playerShip.crew[0].morale[2] == oldMorale + 1 or playerShip.crew[
    0].morale[1] == oldLevel + 1, "Failed to raise morale of the crew member."
updateMorale(playerShip, 0, -1)
assert playerShip.crew[0].morale[2] == oldMorale, "Failed to lower morale of the crew member."

giveOrders(playerShip, 0, rest)
assert playerShip.crew[0].order == talk, "Failed to give order to the player."

giveOrders(playerShip, 1, rest)
assert playerShip.crew[1].order == rest, "Failed to give order to the crew member."

giveOrders(playerShip, 0, rest, -1, false)
updateOrders(playerShip)
assert playerShip.crew[0].order == talk, "Failed to update the player's ship's crew orders."

assert getSkillLevel(playerShip.crew[0], 1) == 0, "Failed to get the level of the player's skill."
assert getSkillLevel(playerShip.crew[0], 4) == 1, "Failed to get the level of the crew member's skill."

assert findMember(talk) == 0, "Failed to find a crew member with the selected order."
assert findMember(defend) == -1, "Failed to not find a crew member with the selected order."

gainExp(10, 4, 0)
assert playerShip.crew[0].skills[0].experience == 10, "Failed to gain experience for a crew member."
