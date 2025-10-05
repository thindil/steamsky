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

## Provides code related to repair and upgrade the player's ship in sky bases.
## Split from basesships module to avoid circular dependencies.

import std/tables
import contracts
import bases, basescargo, basesship, game, game2, items, maps, messages,
    shipscargo, shipscrew, types

type
  NothingToRepairError* = object of CatchableError
    ## Raised when there is nothing to repair on the player's ship
  UniqueModuleError* = object of CatchableError
    ## Raised when there is installed the same type of an unique module
  InstallationError* = object of CatchableError
    ## Raised when there is a problem with installing a new module on the
    ## player's ship
  RemovingError* = object of CatchableError
    ## Raised when there is a problem with removing a modue from the player's
    ## ship

proc repairShip*(moduleIndex: int) {.raises: [NothingToRepairError,
    NotEnoughMoneyError, KeyError, Exception], tags: [WriteIOEffect,
    RootEffect], contractual.} =
  ## Repair the selected module or the whole player's ship in bases
  ##
  ## * moduleIndex - the index of the module to repair. If less than 0, repair
  ##                 the whole player's ship. -1 - slow repair, -2 normal speed,
  ##                 -3 fast repair.
  require:
    moduleIndex in -3..playerShip.modules.high
  body:
    var cost, time: Natural = 0
    repairCost(cost = cost, time = time, moduleIndex = moduleIndex)
    if cost == 0:
      raise newException(exceptn = NothingToRepairError, message = "")
    let traderIndex: int = findMember(order = talk)
    countPrice(price = cost, traderIndex = traderIndex)
    let moneyAmount: Natural = moneyAmount(inventory = playerShip.cargo)
    if moneyAmount < cost:
      raise newException(exceptn = NotEnoughMoneyError, message = "")
    for index, member in playerShip.crew:
      if member.order == repair:
        giveOrders(ship = playerShip, memberIndex = index, givenOrder = rest)
    if moduleIndex > -1:
      playerShip.modules[moduleIndex].durability = playerShip.modules[
          moduleIndex].maxDurability
      addMessage(message = "You bought " & playerShip.modules[
          moduleIndex].name & " repair for " & $cost & " " & moneyName & ".",
          mType = tradeMessage)
    else:
      for module in playerShip.modules.mitems:
        if module.durability < module.maxDurability:
          module.durability = module.maxDurability
      addMessage(message = "You bought an entire ship repair for " & $cost &
          " " & moneyName & ".", mType = tradeMessage)
    updateMoney(memberIndex = -1, amount = -cost, quality = any)
    updateBaseCargo(protoIndex = moneyIndex, amount = cost, quality = normal)
    gainExp(amount = 1, skillNumber = talkingSkill, crewIndex = traderIndex)
    gainRep(baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex, points = 1)
    updateGame(minutes = time)

proc installModule(moduleIndex, traderIndex, moneyAmount, hullIndex: int;
    modulesAmount, freeTurretIndex: var int; baseIndex: ExtendedBasesRange;
    price: var Natural) {.raises: [KeyError, NotEnoughMoneyError,
    UniqueModuleError, InstallationError, IOError, Exception], tags: [
    WriteIOEffect, RootEffect], contractual.} =
  ## Install the selected module on the player's ship
  ##
  ## * moduleIndex     - the index of the proto module to install
  ## * traderIndex     - the index of the trader in the player's ship's crew
  ## * moneyAmount     - the amount of the money in the player's ship's cargo
  ## * hullIndex       - the index of the hull in the player's ship modules' list
  ## * modulesAmount   - the amount of modules installed on the player's ship
  ## * freeTurretIndex - the index of the free turret in the player's ship
  ## * baseIndex       - the index of the base in which the module will be installed
  ## * price           - the price of the module
  ##
  ## Returns modified arguments freeTurretIndex and price
  body:
    price = modulesList[moduleIndex].price
    countPrice(price = price, traderIndex = traderIndex)
    if moneyAmount < price:
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
      playerShip.modules.delete(i = hullIndex)
    else:
      if modulesList[moduleIndex].size > modulesList[playerShip.modules[
          hullIndex].protoIndex].value:
        raise newException(exceptn = InstallationError,
            message = "You can't install this module because it is too big for this hull.")
      if modulesList[moduleIndex].mType notin {ModuleType.gun, harpoonGun, armor}:
        modulesAmount += modulesList[moduleIndex].size
      if modulesAmount > playerShip.modules[hullIndex].maxModules and
          modulesList[moduleIndex].mType notin {ModuleType.gun, harpoonGun, armor}:
        raise newException(exceptn = InstallationError,
            message = "You don't have free modules space for more modules.")
      if modulesList[moduleIndex].mType in {ModuleType.gun, harpoonGun} and
          freeTurretIndex == -1:
        raise newException(exceptn = InstallationError,
            message = "You don't have free turret with proprer size for this gun. Install new turret or remove old gun first.")
    updateMoney(memberIndex = -1, amount = -price, quality = any)
    updateBaseCargo(protoIndex = moneyIndex, amount = price, quality = normal)
    gainExp(amount = 1, skillNumber = talkingSkill, crewIndex = traderIndex)
    gainRep(baseIndex = baseIndex, points = 1)
    updateGame(minutes = modulesList[moduleIndex].installTime)
    if modulesList[moduleIndex].mType == ModuleType.hull:
      playerShip.modules.insert(item = ModuleData(mType: hull,
          name: modulesList[moduleIndex].name, protoIndex: moduleIndex,
              weight: modulesList[
          moduleIndex].weight, durability: modulesList[moduleIndex].durability,
          maxDurability: modulesList[moduleIndex].durability,
          upgradeAction: none, installedModules: modulesList[moduleIndex].value,
          maxModules: modulesList[moduleIndex].maxValue), i = hullIndex)
    else:
      var owners: seq[int] = @[]
      for i in 1 .. modulesList[moduleIndex].maxOwners:
        owners.add(y = -1)
      case modulesList[moduleIndex].mType
      of alchemyLab..greenhouse:
        playerShip.modules.add(y = ModuleData(mType: workshop,
            name: modulesList[moduleIndex].name, protoIndex: moduleIndex,
                weight: modulesList[
            moduleIndex].weight, durability: modulesList[
            moduleIndex].durability, maxDurability: modulesList[
            moduleIndex].durability, owner: owners, upgradeAction: none,
            craftingIndex: "", craftingTime: 0, craftingAmount: 0))
      of medicalRoom:
        playerShip.modules.add(y = ModuleData(mType: medicalRoom,
            name: modulesList[moduleIndex].name, protoIndex: moduleIndex,
                weight: modulesList[
            moduleIndex].weight, durability: modulesList[
            moduleIndex].durability, maxDurability: modulesList[
            moduleIndex].durability, owner: owners, upgradeAction: none))
      of trainingRoom:
        playerShip.modules.add(y = ModuleData(mType: trainingRoom,
            name: modulesList[moduleIndex].name, protoIndex: moduleIndex,
                weight: modulesList[
            moduleIndex].weight, durability: modulesList[
            moduleIndex].durability, maxDurability: modulesList[
            moduleIndex].durability, owner: owners, upgradeAction: none,
            trainedSkill: 0))
      of cockpit:
        playerShip.modules.add(y = ModuleData(mType: cockpit, name: modulesList[
            moduleIndex].name, protoIndex: moduleIndex, weight: modulesList[
            moduleIndex].weight, durability: modulesList[
            moduleIndex].durability, maxDurability: modulesList[
            moduleIndex].durability, owner: owners, upgradeAction: none))
      of turret:
        playerShip.modules.add(y = ModuleData(mType: turret, name: modulesList[
            moduleIndex].name, protoIndex: moduleIndex, weight: modulesList[
            moduleIndex].weight, durability: modulesList[
            moduleIndex].durability, maxDurability: modulesList[
            moduleIndex].durability, owner: owners, upgradeAction: none,
            gunIndex: -1))
      of cabin:
        playerShip.modules.add(y = ModuleData(mType: cabin, name: modulesList[
            moduleIndex].name, protoIndex: moduleIndex, weight: modulesList[
            moduleIndex].weight, durability: modulesList[
            moduleIndex].durability, maxDurability: modulesList[
            moduleIndex].durability, owner: owners, upgradeAction: none,
            cleanliness: modulesList[moduleIndex].value, quality: modulesList[
            moduleIndex].maxValue))
      of cargo:
        playerShip.modules.add(y = ModuleData(mType: cargoRoom,
            name: modulesList[moduleIndex].name, protoIndex: moduleIndex,
                weight: modulesList[
            moduleIndex].weight, durability: modulesList[
            moduleIndex].durability, maxDurability: modulesList[
            moduleIndex].durability, owner: owners, upgradeAction: none))
      of engine:
        playerShip.modules.add(y = ModuleData(mType: engine, name: modulesList[
            moduleIndex].name, protoIndex: moduleIndex, weight: modulesList[
            moduleIndex].weight, durability: modulesList[
            moduleIndex].durability, maxDurability: modulesList[
            moduleIndex].durability, owner: owners, upgradeAction: none,
            fuelUsage: modulesList[moduleIndex].value, power: modulesList[
            moduleIndex].maxValue, disabled: false))
      of armor:
        playerShip.modules.add(y = ModuleData(mType: armor, name: modulesList[
            moduleIndex].name, protoIndex: moduleIndex, weight: modulesList[
            moduleIndex].weight, durability: modulesList[
            moduleIndex].durability, maxDurability: modulesList[
            moduleIndex].durability, owner: owners, upgradeAction: none))
      of batteringRam:
        playerShip.modules.add(y = ModuleData(mType: batteringRam,
            name: modulesList[moduleIndex].name, protoIndex: moduleIndex,
            weight: modulesList[moduleIndex].weight, durability: modulesList[
            moduleIndex].durability, maxDurability: modulesList[
            moduleIndex].durability, owner: owners, upgradeAction: none,
            damage2: modulesList[moduleIndex].maxValue, coolingDown: false))
      of gun:
        playerShip.modules.add(y = ModuleData(mType: gun, name: modulesList[
            moduleIndex].name, protoIndex: moduleIndex, weight: modulesList[
            moduleIndex].weight, durability: modulesList[
            moduleIndex].durability, maxDurability: modulesList[
            moduleIndex].durability, owner: owners, upgradeAction: none,
            damage: modulesList[moduleIndex].maxValue, ammoIndex: -1))
      of harpoonGun:
        playerShip.modules.add(y = ModuleData(mType: harpoonGun,
            name: modulesList[moduleIndex].name, protoIndex: moduleIndex,
                weight: modulesList[
            moduleIndex].weight, durability: modulesList[
            moduleIndex].durability, maxDurability: modulesList[
            moduleIndex].durability, owner: owners, upgradeAction: none,
            duration: modulesList[moduleIndex].maxValue, harpoonIndex: -1))
      else:
        discard
    if modulesList[moduleIndex].mType in {ModuleType.gun, harpoonGun}:
      playerShip.modules[freeTurretIndex].gunIndex = playerShip.modules.high
    else:
      playerShip.modules[hullIndex].installedModules = modulesAmount
    addMessage(message = "You installed " & modulesList[moduleIndex].name &
        " on your ship for " & $price & " " & moneyName & ".",
        mType = tradeMessage)

proc upgradeShip*(install: bool; moduleIndex: Natural) {.raises: [
    NoMoneyError, KeyError, NotEnoughMoneyError, UniqueModuleError,
    InstallationError, IOError, RemovingError, NoFreeCargoError,
    NoMoneyInBaseError, CrewOrderError, CrewNoSpaceError, Exception], tags: [
    WriteIOEffect, RootEffect], contractual.} =
  ## Install or remove modules in the player's ship in bases
  ##
  ## * install     - if true, install the selected module
  ## * moduleIndex - the prototype index of the module to install or index of
  ##                 module in the player's ship to remove
  body:
    let moneyAmount: Natural = moneyAmount(inventory = playerShip.cargo)
    if moneyAmount == 0:
      raise newException(exceptn = NoMoneyError, message = "")
    var hullIndex, modulesAmount, freeTurretIndex: int = 0
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
    let
      traderIndex: int = findMember(order = talk)
      baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
          playerShip.skyY].baseIndex
    var price: Natural = 0
    if install:
      installModule(moduleIndex = moduleIndex, traderIndex = traderIndex,
          moneyAmount = moneyAmount, hullIndex = hullIndex,
          modulesAmount = modulesAmount, baseIndex = baseIndex,
          freeTurretIndex = freeTurretIndex, price = price)
    else:
      let
        shipModuleIndex: int = moduleIndex
        damage: float = 1.0 - (playerShip.modules[
            shipModuleIndex].durability.float / playerShip.modules[
            shipModuleIndex].maxDurability.float)
      price = modulesList[playerShip.modules[
          shipModuleIndex].protoIndex].price - (modulesList[playerShip.modules[
          shipModuleIndex].protoIndex].price.float * damage).Natural
      countPrice(price = price, traderIndex = traderIndex, reduce = false)
      if freeCargo(amount = -price) < 0:
        raise newException(exceptn = NoFreeCargoError, message = "")
      if price > skyBases[baseIndex].cargo[0].amount:
        raise newException(exceptn = NoMoneyInBaseError, message = "")
      case modulesList[playerShip.modules[shipModuleIndex].protoIndex].mType
      of turret:
        if playerShip.modules[shipModuleIndex].gunIndex > -1:
          raise newException(exceptn = RemovingError,
              message = "You have installed gun in this turret, remove it before you remove this turret.")
      of gun, harpoonGun:
        for module in playerShip.modules.mitems:
          if module.mType == ModuleType2.turret and module.gunIndex == shipModuleIndex:
            module.gunIndex = -1
            break
      of cargo:
        if freeCargo(amount = 0 - modulesList[playerShip.modules[
            shipModuleIndex].protoIndex].maxValue) < 0:
          raise newException(exceptn = RemovingError,
              message = "You can't sell this cargo bay, because you have items in it.")
      else:
        discard
      if modulesList[playerShip.modules[shipModuleIndex].protoIndex].mType notin
          {ModuleType.hull, armor, gun, harpoonGun}:
        modulesAmount -= modulesList[playerShip.modules[
            shipModuleIndex].protoIndex].size
        playerShip.modules[hullIndex].installedModules = modulesAmount
      if playerShip.upgradeModule == shipModuleIndex:
        playerShip.upgradeModule = -1
        for index, member in playerShip.crew:
          if member.order == upgrading:
            giveOrders(ship = playerShip, memberIndex = index,
                givenOrder = rest)
            break
      if playerShip.modules[shipModuleIndex].mType != ModuleType2.cabin:
        for owner in playerShip.modules[shipModuleIndex].owner:
          if owner > -1:
            giveOrders(ship = playerShip, memberIndex = owner,
                givenOrder = rest, checkPriorities = false)
      updateMoney(memberIndex = -1, amount = price, quality = any)
      updateBaseCargo(protoIndex = moneyIndex, amount = price, quality = normal)
      gainExp(amount = 1, skillNumber = talkingSkill, crewIndex = traderIndex)
      gainRep(baseIndex = baseIndex, points = 1)
      updateGame(minutes = modulesList[playerShip.modules[
          shipModuleIndex].protoIndex].installTime)
      addMessage(message = "You removed " & playerShip.modules[
          shipModuleIndex].name & " from your ship and received " & $price &
          " " &
          moneyName & ".", mType = tradeMessage)
      playerShip.modules.delete(i = shipModuleIndex)
      if playerShip.repairModule > shipModuleIndex:
        playerShip.repairModule.dec
      elif playerShip.repairModule == shipModuleIndex:
        playerShip.repairModule = -1
      if playerShip.upgradeModule > shipModuleIndex:
        playerShip.upgradeModule.dec
      for module in playerShip.modules.mitems:
        if module.mType == ModuleType2.turret and module.gunIndex > shipModuleIndex:
          module.gunIndex.dec
