# Copyright 2023-2025 Bartek thindil Jasicki
#
# This file is part of Steam Sky.
#
# Steam Sky is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Steam Sky is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

## Provides code related to the sky bases and the player's ship's inteactions,
## like paying for docking and repairinig the ship.

import std/tables
import contracts
import bases, basescargo, basestypes, config, game, items, maps, messages, shipscrew, types

proc payForDock*() {.raises: [KeyError, ReputationError, NoFreeSpaceError],
    tags: [], contractual.} =
  ## Pay daily fee for docking, if the player doesn't have enough money for
  ## pay, reduce the player's reputation in the base
  let baseIndex: BasesRange = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  if getBasePopulation(baseIndex = baseIndex) == empty:
    return
  let moneyAmount: Natural = moneyAmount(inventory = playerShip.cargo)
  if moneyAmount == 0:
    gainRep(baseIndex = baseIndex, points = -10)
    addMessage(message = "You don't have " & moneyName &
        " for pay for docking!", mType = otherMessage, color = red)
    return
  var dockingCost: Natural = 0
  for module in playerShip.modules:
    if module.mType == ModuleType2.hull:
      dockingCost = module.maxModules
      break
  dockingCost = (dockingCost.float * newGameSettings.pricesBonus).Natural
  if dockingCost == 0:
    dockingCost = 1
  let traderIndex: int = findMember(order = talk)
  countPrice(price = dockingCost, traderIndex = traderIndex)
  if dockingCost > moneyAmount:
    dockingCost = moneyAmount
  try:
    updateMoney(memberIndex = -1, amount = -(dockingCost), quality = any)
  except CrewNoSpaceError:
    discard
  updateBaseCargo(protoIndex = moneyIndex, amount = dockingCost,
      quality = normal)
  addMessage(message = "You pay " & $dockingCost & " " & moneyName &
      " docking fee.", mType = otherMessage)
  if traderIndex > -1:
    gainExp(amount = 1, skillNumber = talkingSkill, crewIndex = traderIndex)

proc repairCost*(cost, time: var Natural; moduleIndex: int) {.raises: [
    KeyError], tags: [], contractual.} =
  ## Count the repair cost and time required for the player's ship in the base
  ##
  ## * cost        - the cost of repair action
  ## * time        - the time required to finish the repair action
  ## * moduleIndex - the index of the module to repair. Values below 0 means:
  ##                 -1: repair all damaged modules slowly, -2 repair all damaged
  ##                 modules in normal speed, -3 repais all damaged modules fast
  ##
  ## Returns the modified parameters cost and time
  let baseIndex: BasesRange = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  var protoIndex: int = -1
  if moduleIndex > -1:
    time = playerShip.modules[moduleIndex].maxDurability - playerShip.modules[
        moduleIndex].durability
    protoIndex = findProtoItem(itemType = modulesList[playerShip.modules[
        moduleIndex].protoIndex].repairMaterial)
    cost = time * getPrice(baseType = skyBases[baseIndex].baseType,
        itemIndex = protoIndex, quality = normal)
  else:
    for module in playerShip.modules:
      if module.durability < module.maxDurability:
        time = time + module.maxDurability - module.durability
        protoIndex = findProtoItem(itemType = modulesList[
            module.protoIndex].repairMaterial)
        cost += ((module.maxDurability - module.durability) * getPrice(
            baseType = skyBases[baseIndex].baseType, itemIndex = protoIndex,
                quality = normal))
    if moduleIndex == -2:
      cost *= 2
      time = (time / 2).Natural
    elif moduleIndex == -3:
      cost *= 4
      time = (time / 4).Natural
  if "shipyard" in basesTypesList[skyBases[baseIndex].baseType].flags:
    cost = (cost / 2).Natural
  cost = (cost.float * newGameSettings.pricesBonus).Natural
  if cost == 0:
    cost = 1
  if time == 0:
    time = 1
