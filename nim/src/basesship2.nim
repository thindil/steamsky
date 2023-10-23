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
  InstallationError* = object of CatchableError
    ## Raised when there is a problem with installing a new module on the
    ## player's ship

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
    for module in playerShip.modules:
      if modulesList[module.protoIndex].mType == modulesList[
          moduleIndex].mType and modulesList[moduleIndex].unique:
        raise newException(exceptn = UniqueModuleError, message = modulesList[
            moduleIndex].name)
    if modulesList[moduleIndex].mType == ModuleType.hull:
      for module in playerShip.modules:
        if modulesList[module.protoIndex].size > modulesList[moduleIndex].value:
          raise newException(exceptn = InstallationError,
              message = "This hull don't allow to have installed that big modules what you currently have.")
      if modulesList[moduleIndex].maxValue < modulesAmount:
        raise newException(exceptn = InstallationError,
            message = "This hull is too small for your ship. Remove some modules first.")
      playerShip.modules.delete(hullIndex)
    else:
      if modulesList[moduleIndex].size > modulesList[playerShip.modules[
          hullIndex].protoIndex].value:
        raise newException(exceptn = InstallationError,
            message = "You can't install this module because it is too big for this hull.")
      if modulesList[moduleIndex].mType notin {ModuleType.gun, harpoonGun, armor}:
        modulesAmount = modulesAmount + modulesList[moduleIndex].size
      if modulesAmount > playerShip.modules[hullIndex].maxModules and
          modulesList[moduleIndex].mType notin {ModuleType.gun, harpoonGun, armor}:
        raise newException(exceptn = InstallationError,
            message = "You don't have free modules space for more modules.")
      if modulesList[moduleIndex].mType in {ModuleType.gun, harpoonGun} and
          freeTurretIndex == -1:
        raise newException(exceptn = InstallationError,
            message = "You don't have free turret with proprer size for this gun. Install new turret or remove old gun first.")
    updateCargo(ship = playerShip, cargoIndex = moneyIndex2, amount = -price)
    updateBaseCargo(protoIndex = moneyIndex, amount = price)
    gainExp(amount = 1, skillNumber = talkingSkill, crewIndex = traderIndex)
    gainRep(baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex, points = 1)
    updateGame(minutes = modulesList[moduleIndex].installTime)
    if modulesList[moduleIndex].mType == ModuleType.hull:
      playerShip.modules.insert(ModuleData(mType: hull, name: modulesList[
          moduleIndex].name, protoIndex: moduleIndex, weight: modulesList[
          moduleIndex].weight, durability: modulesList[moduleIndex].durability,
          maxDurability: modulesList[moduleIndex].durability,
          upgradeAction: none, installedModules: modulesList[moduleIndex].value,
          maxModules: modulesList[moduleIndex].maxValue), hullIndex)
    else:
      var owners: seq[int] = @[]
      for i in 1 .. modulesList[moduleIndex].maxOwners:
        owners.add(-1)
      case modulesList[moduleIndex].mType
      of alchemyLab .. greenhouse:
        playerShip.modules.add(ModuleData(mType: workshop, name: modulesList[
            moduleIndex].name, protoIndex: moduleIndex, weight: modulesList[
            moduleIndex].weight, durability: modulesList[
            moduleIndex].durability, maxDurability: modulesList[
            moduleIndex].durability, owner: owners, upgradeAction: none,
            craftingIndex: "", craftingTime: 0, craftingAmount: 0))
      of medicalRoom:
        playerShip.modules.add(ModuleData(mType: medicalRoom, name: modulesList[
            moduleIndex].name, protoIndex: moduleIndex, weight: modulesList[
            moduleIndex].weight, durability: modulesList[
            moduleIndex].durability, maxDurability: modulesList[
            moduleIndex].durability, owner: owners, upgradeAction: none))
      of trainingRoom:
        playerShip.modules.add(ModuleData(mType: trainingRoom,
            name: modulesList[moduleIndex].name, protoIndex: moduleIndex,
                weight: modulesList[
            moduleIndex].weight, durability: modulesList[
            moduleIndex].durability, maxDurability: modulesList[
            moduleIndex].durability, owner: owners, upgradeAction: none,
            trainedSkill: 0))
      of cockpit:
        playerShip.modules.add(ModuleData(mType: cockpit, name: modulesList[
            moduleIndex].name, protoIndex: moduleIndex, weight: modulesList[
            moduleIndex].weight, durability: modulesList[
            moduleIndex].durability, maxDurability: modulesList[
            moduleIndex].durability, owner: owners, upgradeAction: none))
      of turret:
        playerShip.modules.add(ModuleData(mType: turret, name: modulesList[
            moduleIndex].name, protoIndex: moduleIndex, weight: modulesList[
            moduleIndex].weight, durability: modulesList[
            moduleIndex].durability, maxDurability: modulesList[
            moduleIndex].durability, owner: owners, upgradeAction: none,
            gunIndex: -1))
      of cabin:
        playerShip.modules.add(ModuleData(mType: cabin, name: modulesList[
            moduleIndex].name, protoIndex: moduleIndex, weight: modulesList[
            moduleIndex].weight, durability: modulesList[
            moduleIndex].durability, maxDurability: modulesList[
            moduleIndex].durability, owner: owners, upgradeAction: none,
            cleanliness: modulesList[moduleIndex].value, quality: modulesList[
            moduleIndex].maxValue))
      of cargo:
        playerShip.modules.add(ModuleData(mType: cargoRoom, name: modulesList[
            moduleIndex].name, protoIndex: moduleIndex, weight: modulesList[
            moduleIndex].weight, durability: modulesList[
            moduleIndex].durability, maxDurability: modulesList[
            moduleIndex].durability, owner: owners, upgradeAction: none))
      else:
        discard

# Temporary code for interfacing with Ada

proc repairAdaShip2(moduleIndex: cint): cstring {.raises: [], tags: [
    WriteIOEffect, RootEffect], exportc.} =
  try:
    repairShip(moduleIndex = moduleIndex - 1)
    return "".cstring
  except Exception as e:
    return ($e.name & " " & e.msg).cstring

