# Copyright 2023 Bartek thindil Jasicki
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

import std/tables
import bases, basescargo, basestypes, config, crewinventory, game, items, maps,
    messages, shipscargo, shipscrew, types

proc payForDock*() {.sideEffect, raises: [KeyError], tags: [].} =
  ## Pay daily fee for docking, if the player doesn't have enough money for
  ## pay, reduce the player's reputation in the base
  let baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  if skyBases[baseIndex].population == 0:
    return
  let moneyIndex2 = findItem(inventory = playerShip.cargo,
      protoIndex = moneyIndex)
  if moneyIndex2 == -1:
    gainRep(baseIndex = baseIndex, points = -10)
    addMessage(message = "You don't have " & moneyName &
        " for pay for docking!", mType = otherMessage, color = red)
    return
  var dockingCost: Natural = 0
  for module in playerShip.modules.items:
    if module.mType == ModuleType2.hull:
      dockingCost = module.maxModules
      break
  dockingCost = (dockingCost.float * newGameSettings.pricesBonus).Natural
  if dockingCost == 0:
    dockingCost = 1
  let traderIndex = findMember(order = talk)
  countPrice(price = dockingCost, traderIndex = traderIndex)
  if dockingCost > playerShip.cargo[moneyIndex2].amount:
    dockingCost = playerShip.cargo[moneyIndex].amount
  updateCargo(ship = playerShip, cargoIndex = moneyIndex2, amount = -(dockingCost))
  updateBaseCargo(protoIndex = moneyIndex, amount = dockingCost)
  addMessage(message = "You pay " & $dockingCost & " " & moneyName &
      " docking fee.", mType = otherMessage)
  if traderIndex > -1:
    gainExp(amount = 1, skillNumber = talkingSkill, crewIndex = traderIndex)

proc repairCost*(cost, time: var Natural; moduleIndex: int) {.sideEffect,
    raises: [KeyError], tags: [].} =
  ## Count the repair cost and time required for the player's ship in the base
  ##
  ## * cost        - the cost of repair action
  ## * time        - the time required to finish the repair action
  ## * moduleIndex - the index of the module to repair. Values below 0 means:
  ##                 -1: repair all damaged modules slowly, -2 repair all damaged
  ##                 modules in normal speed, -3 repais all damaged modules fast
  ##
  ## Returns the modified parameters cost and time
  let baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  var protoIndex: int
  if moduleIndex > -1:
    time = playerShip.modules[moduleIndex].maxDurability - playerShip.modules[
        moduleIndex].durability
    protoIndex = findProtoItem(itemType = modulesList[playerShip.modules[
        moduleIndex].protoIndex].repairMaterial)
    cost = time * getPrice(baseType = skyBases[baseIndex].baseType,
        itemIndex = protoIndex)
  else:
    for module in playerShip.modules:
      if module.durability < module.maxDurability:
        time = time + module.maxDurability - module.durability
        protoIndex = findProtoItem(itemType = modulesList[
            module.protoIndex].repairMaterial)
        cost = cost + ((module.maxDurability - module.durability) * getPrice(
            baseType = skyBases[baseIndex].baseType, itemIndex = protoIndex))
    if moduleIndex == -2:
      cost = cost * 2
      time = (time / 2).Natural
    elif moduleIndex == -3:
      cost = cost * 4
      time = (time / 4).Natural
  if "shipyard" in basesTypesList[skyBases[baseIndex].baseType].flags:
    cost = (cost / 2).Natural
  cost = (cost.float * newGameSettings.pricesBonus).Natural
  if cost == 0:
    cost = 1
  if time == 0:
    time = 1

# Temporary code for interfacing with Ada

proc payAdaForDock() {.raises: [], tags: [], exportc.} =
  try:
    payForDock()
  except KeyError:
    discard

proc repairAdaCost(cost, time: var cint; moduleIndex: cint) {.raises: [],
    tags: [], exportc.} =
  try:
    var
      nimCost = cost.Natural
      nimTime = time.Natural
    repairCost(cost = nimCost, time = nimTime, moduleIndex = moduleIndex - 1)
    cost = nimCost.cint
    time = nimTime.cint
  except KeyError:
    discard
