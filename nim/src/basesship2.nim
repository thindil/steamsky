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
import bases, basescargo, basesship, crewinventory, game, game2, maps, messages,
    shipscargo, shipscrew, trades, types

type
  NothingToRepairError* = object of CatchableError
    ## Raised when there is nothing to repair on the player's ship
  UniqueModuleError* = object of CatchableError
    ## Raised when there is installed the same type of an unique module

proc repairShip*(moduleIndex: int) {.sideEffect, raises: [NothingToRepairError,
    NotEnoughMoneyError, KeyError, Exception], tags: [WriteIOEffect,
    RootEffect].} =
  ## Repair the selected module or the whole player's ship in bases
  ##
  ## * moduleIndex - the index of the module to repair. If less than 0, repair
  ##                 the whole player's ship. -1 - slow repair, -2 normal speed,
  ##                 -3 fast repair.
  var cost, time: Natural = 0
  repairCost(cost = cost, time = time, moduleIndex = moduleIndex)
  if cost == 0:
    raise newException(exceptn = NothingToRepairError, message = "")
  let traderIndex = findMember(order = talk)
  countPrice(price = cost, traderIndex = traderIndex)
  let moneyIndex2 = findItem(inventory = playerShip.cargo,
      protoIndex = moneyIndex)
  if playerShip.cargo[moneyIndex2].amount < cost:
    raise newException(exceptn = NotEnoughMoneyError, message = "")
  for index, member in playerShip.crew:
    if member.order == repair:
      giveOrders(ship = playerShip, memberIndex = index, givenOrder = rest)
  if moduleIndex > -1:
    playerShip.modules[moduleIndex].durability = playerShip.modules[
        moduleIndex].maxDurability
    addMessage(message = "You bought " & playerShip.modules[moduleIndex].name &
        " repair for " & $cost & " " & moneyName & ".", mType = tradeMessage)
  else:
    for module in playerShip.modules.mitems:
      if module.durability < module.maxDurability:
        module.durability = module.maxDurability
    addMessage(message = "You bought an entire ship repair for " & $cost & " " &
        moneyName & ".", mType = tradeMessage)
  updateCargo(ship = playerShip, cargoIndex = moneyIndex2, amount = -cost)
  updateBaseCargo(protoIndex = moneyIndex, amount = cost)
  gainExp(amount = 1, skillNumber = talkingSkill, crewIndex = traderIndex)
  gainRep(baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex, points = 1)
  updateGame(minutes = time)

proc upgradeShip*(install: bool; moduleIndex: Natural) =
  let moneyIndex2 = findItem(inventory = playerShip.cargo,
      protoIndex = moneyIndex)
  if moneyIndex2 == -1:
    raise newException(exceptn = NoMoneyError, message = "")
  var hullIndex, modulesAmount, freeTurretIndex = 0
  for index, module in playerShip.modules:
    case module.mType
    of hull:
      hullIndex = index
      modulesAmount = module.installedModules
    of turret:
      if module.gunIndex == -1 and install and modulesList[
          module.protoIndex].size >= modulesList[moduleIndex].size:
        freeTurretIndex = index
    else:
      discard
  let traderIndex = findMember(order = talk)
  if install:
    var price = modulesList[moduleIndex].price
    countPrice(price = price, traderIndex = traderIndex)
    if playerShip.cargo[moneyIndex2].amount < price:
      raise newException(exceptn = NotEnoughMoneyError, message = modulesList[
          moduleIndex].name)

# Temporary code for interfacing with Ada

proc repairAdaShip2(moduleIndex: cint): cstring {.raises: [], tags: [
    WriteIOEffect, RootEffect], exportc.} =
  try:
    repairShip(moduleIndex = moduleIndex - 1)
    return "".cstring
  except Exception as e:
    return ($e.name & " " & e.msg).cstring

