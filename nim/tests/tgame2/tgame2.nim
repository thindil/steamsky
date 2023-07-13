discard """
  exitcode: 0
  output: '''Loading the game data.
Testing updateGame.
Testing endGame.'''
"""

import std/tables
import ../../src/[basestypes, careers, factions, game, game2, items, messages, types]

echo "Loading the game data."
if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
if basesTypesList.len == 0:
  loadBasesTypes("../bin/data/bases.dat")

gameDate = DateRecord(year: 1600, month: 1, day: 1, hour: 8, minutes: 0)
playerShip.modules = @[]
playerShip.modules.add(ModuleData(mType: ModuleType2.engine, protoIndex: 3,
    durability: 100, fuelUsage: 4, power: 2000, disabled: false))
playerShip.cargo = @[]
playerShip.cargo.add(InventoryData(protoIndex: 1, amount: 100, durability: 100))
skyBases[1].baseType = "1"
playerShip.crew = @[]

echo "Testing updateGame."
updateGame(1)
assert gameDate.minutes == 1, "Failed to updated the game."

echo "Testing endGame."
let oldSaveDir = saveDirectory
saveDirectory = "."
addMessage("Test message", otherMessage)
endGame(false)
saveDirectory = oldSaveDir
assert messagesAmount(0) == 0, "Failed to end the game."
