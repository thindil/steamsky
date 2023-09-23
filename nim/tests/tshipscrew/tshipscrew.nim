discard """
  exitcode: 0
  output: '''Loading the game data.
Testing generateMemberName.
Testing updateMorale.
Testing giveOrders.
Testing updateOrders.
Testing getSkillLevel.
Testing findMember.
Testing gainExp.'''
"""

import std/tables
import ../../src/[careers, factions, game, maps, items, shipscrew, types]

echo "Loading the game data."
if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")

echo "Testing generateMemberName."
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

echo "Testing updateMorale."
let oldMorale = playerShip.crew[0].morale[2]
let oldLevel = playerShip.crew[0].morale[1]
updateMorale(playerShip, 0, 1)
try:
  assert playerShip.crew[0].morale[2] == oldMorale + 1 or playerShip.crew[
    0].morale[1] == oldLevel + 1
except AssertionDefect:
  writeLine(stderr, "Failed to raise morale of the crew member.")
updateMorale(playerShip, 0, -1)
try:
  assert playerShip.crew[0].morale[2] == oldMorale
except AssertionDefect:
  writeLine(stderr, "Failed to lower morale of the crew member.")

echo "Testing giveOrders."
giveOrders(playerShip, 0, rest)
try:
  assert playerShip.crew[0].order == talk
except AssertionDefect:
  writeLine(stderr, "Failed to give order to the player.")
giveOrders(playerShip, 1, rest)
try:
  assert playerShip.crew[1].order == rest
except AssertionDefect:
  writeLine(stderr, "Failed to give order to the crew member.")

echo "Testing updateOrders."
giveOrders(playerShip, 0, rest, -1, false)
updateOrders(playerShip)
try:
  assert playerShip.crew[0].order == talk
except AssertionDefect:
  writeLine(stderr, "Failed to update the player's ship's crew orders.")

echo "Testing getSkillLevel."
try:
  assert getSkillLevel(playerShip.crew[0], 1) == 0
except AssertionDefect:
  writeLine(stderr, "Failed to get the level of the player's skill.")
try:
  assert getSkillLevel(playerShip.crew[0], 4) == 7
except AssertionDefect:
  writeLine(stderr, "Failed to get the level of the crew member's skill.")

echo "Testing findMember."
try:
  assert findMember(talk) == 0
except AssertionDefect:
  writeLine(stderr, "Failed to find a crew member with the selected order.")
try:
  assert findMember(defend) == -1
except AssertionDefect:
  writeLine(stderr, "Failed to not find a crew member with the selected order.")

echo "Testing gainExp."
gainExp(10, 4, 0)
try:
  assert playerShip.crew[0].skills[0].experience == 10
except AssertionDefect:
  writeLine(stderr, "Failed to gain experience for a crew member.")
