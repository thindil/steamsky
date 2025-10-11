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

## Provides code related to upgrading the player's ship, like setting the
## upgrade and the upgrading process itself.

import std/tables
import contracts, nimalyzer
import config, crewinventory, game, items, messages, shipscargo, shipscrew, types

type ShipUpgradeError* = object of CatchableError
  ## Raised when there is some problems with starting the ship upgrade

proc upgradeShip*(minutes: Positive) {.raises: [KeyError,
    Exception], tags: [RootEffect], contractual.} =
  ## Upgrade the currently selected module in the player ship
  ##
  ## * minutes - the amount of in-game minutes which passed

  var upgradeMaterial, upgradeTools, workerIndex: int = -1
  {.ruleOff: "varDeclared".}
  var upgradedModule: ModuleData
  {.ruleOn: "varDeclared".}

  proc findMatsAndTools() {.raises: [KeyError, Exception], tags: [
      RootEffect], contractual.} =
    ## Find necessary materials and tools for the upgrade
    upgradeTools = findTools(memberIndex = workerIndex, itemType = repairTools,
        order = upgrading)
    upgradeMaterial = findItem(inventory = playerShip.cargo,
        itemType = modulesList[upgradedModule.protoIndex].repairMaterial,
            itemQuality = any)

  proc maxUpgradeReached(messageText: string) {.raises: [KeyError,
      Exception], tags: [RootEffect], contractual.} =
    ## Show message about reaching the maximum allowed level of upgrades and
    ## clear the player's ship upgrades settings.
    ##
    ## * messageText - the message which will be shown to the player
    require:
      messageText.len > 0
    body:
      addMessage(message = messageText & upgradedModule.name & ".",
          mtype = orderMessage, color = yellow)
      upgradedModule.upgradeProgress = 0
      upgradedModule.upgradeAction = ShipUpgrade.none
      playerShip.modules[playerShip.upgradeModule] = upgradedModule
      playerShip.upgradeModule = -1
      giveOrders(ship = playerShip, memberIndex = workerIndex,
          givenOrder = rest)

  if playerShip.upgradeModule == -1:
    return
  workerIndex = findMember(order = upgrading)
  if workerIndex == -1:
    return
  upgradedModule = playerShip.modules[playerShip.upgradeModule]
  var
    currentMinutes: int = minutes
    orderTime: int = playerShip.crew[workerIndex].orderTime
  if upgradedModule.durability == 0:
    addMessage(message = playerShip.crew[workerIndex].name &
        " stops upgrading " & upgradedModule.name & " because it's destroyed.",
        mType = orderMessage, color = red)
    giveOrders(ship = playerShip, memberIndex = workerIndex, givenOrder = rest)
  var times: Natural = 0
  while currentMinutes > 0:
    if currentMinutes >= orderTime:
      currentMinutes -= orderTime
      times.inc
      orderTime = 15
    else:
      orderTime -= currentMinutes
      currentMinutes = 0
  playerShip.crew[workerIndex].orderTime = orderTime
  if times == 0:
    return
  var upgradePoints: int = ((getSkillLevel(member = playerShip.crew[
      workerIndex], skillIndex = modulesList[
          upgradedModule.protoIndex].repairSkill) /
      10).int * times) + times
  while upgradePoints > 0 and upgradedModule.upgradeProgress > 0:
    findMatsAndTools()
    if upgradeMaterial == -1:
      addMessage(message = "You don't have enough materials to upgrade " &
          upgradedModule.name, mtype = orderMessage, color = red)
      giveOrders(ship = playerShip, memberIndex = workerIndex,
          givenOrder = rest)
      break
    if upgradeTools == -1:
      addMessage(message = "You don't have the repair tool to upgrade " &
          upgradedModule.name, mtype = orderMessage, color = red)
      giveOrders(ship = playerShip, memberIndex = workerIndex,
          givenOrder = rest)
      break
    let
      toolsQuality: ObjectQuality = playerShip.crew[workerIndex].inventory[upgradeTools].quality
      materialsQuality: ObjectQuality = playerShip.cargo[upgradeMaterial].quality
    var resultAmount: Natural = upgradePoints + countItemBonus(
        value = upgradePoints, quality = toolsQuality) + countItemBonus(
        value = upgradePoints, quality = materialsQuality)
    if resultAmount > upgradedModule.upgradeProgress:
      resultAmount = upgradedModule.upgradeProgress
    var materialCost: Natural = 0
    case upgradedModule.upgradeAction
    of maxValue:
      case upgradedModule.mType
      of ModuleType2.engine:
        if resultAmount > playerShip.cargo[upgradeMaterial].amount * 200:
          resultAmount = playerShip.cargo[upgradeMaterial].amount * 200
        materialCost = (resultAmount / 200).int
      of ModuleType2.cabin:
        if resultAmount > playerShip.cargo[upgradeMaterial].amount * 20:
          resultAmount = playerShip.cargo[upgradeMaterial].amount * 20
        materialCost = (resultAmount / 20).int
      of ModuleType2.gun, ModuleType2.batteringRam, ModuleType2.harpoonGun:
        if resultAmount > playerShip.cargo[upgradeMaterial].amount * 10:
          resultAmount = playerShip.cargo[upgradeMaterial].amount * 10
        materialCost = (resultAmount / 10).int
      of ModuleType2.hull:
        if resultAmount > playerShip.cargo[upgradeMaterial].amount * 2:
          resultAmount = playerShip.cargo[upgradeMaterial].amount * 2
        materialCost = (resultAmount / 2).int
      else:
        if resultAmount > playerShip.cargo[upgradeMaterial].amount:
          resultAmount = playerShip.cargo[upgradeMaterial].amount
        materialCost = resultAmount
    of durability:
      if resultAmount > playerShip.cargo[upgradeMaterial].amount * 10:
        resultAmount = playerShip.cargo[upgradeMaterial].amount * 10
      materialCost = (resultAmount / 10).int
    else:
      if resultAmount > playerShip.cargo[upgradeMaterial].amount:
        resultAmount = playerShip.cargo[upgradeMaterial].amount
      materialCost = resultAmount
    materialCost = materialCost - countItemBonus(value = upgradePoints,
        quality = toolsQuality) - countItemBonus(value = upgradePoints,
        quality = materialsQuality)
    if materialCost < times:
      materialCost = times
    if materialCost > playerShip.cargo[upgradeMaterial].amount:
      materialCost = playerShip.cargo[upgradeMaterial].amount
    gainExp(amount = resultAmount, skillNumber = modulesList[
        upgradedModule.protoIndex].repairSkill, crewIndex = workerIndex)
    damageItem(inventory = playerShip.crew[workerIndex].inventory,
        itemIndex = upgradeTools, skillLevel = getSkillLevel(
        member = playerShip.crew[workerIndex], skillIndex = modulesList[
        upgradedModule.protoIndex].repairSkill), memberIndex = workerIndex,
        ship = playerShip)
    findMatsAndTools()
    var upgradeProgress: int = upgradedModule.upgradeProgress - resultAmount
    upgradePoints -= resultAmount
    updateCargo(ship = playerShip, protoIndex = playerShip.cargo[
        upgradeMaterial].protoIndex, amount = -(materialCost),
            quality = playerShip.cargo[upgradeMaterial].quality)
    if upgradeProgress == 0:
      var weightGain: int = (modulesList[upgradedModule.protoIndex].weight /
          modulesList[upgradedModule.protoIndex].durability).int
      if weightGain < 1:
        weightGain = 1
      var upgradeValue, localMaxValue: int = 0
      case upgradedModule.upgradeAction
      of durability:
        if (modulesList[upgradedModule.protoIndex].durability / 20).int > 0:
          upgradedModule.maxDurability += (modulesList[
              upgradedModule.protoIndex].durability / 20).int
          upgradedModule.weight += (weightGain * (modulesList[
              upgradedModule.protoIndex].durability / 20).int)
        else:
          upgradedModule.maxDurability.inc
          upgradedModule.weight += weightGain
        addMessage(message = playerShip.crew[workerIndex].name &
            " has upgraded the durability of " & upgradedModule.name & ".",
            mType = orderMessage, color = green)
        localMaxValue = (modulesList[
            upgradedModule.protoIndex].durability.float * 1.5).int
        if upgradedModule.maxDurability == localMaxValue:
          maxUpgradeReached(messageText = "You've reached the maximum durability for ")
          return
        upgradedModule.upgradeProgress = (modulesList[
            upgradedModule.protoIndex].durability.float *
            newGameSettings.upgradeCostBonus).int
      of maxValue:
        case upgradedModule.mType
        of ModuleType2.hull:
          weightGain *= 10
          upgradedModule.maxModules.inc
          upgradeValue = upgradedModule.maxModules
        of ModuleType2.engine:
          weightGain = (modulesList[upgradedModule.protoIndex].maxValue / 40).int
          upgradedModule.power += (modulesList[
              upgradedModule.protoIndex].maxValue / 20).int
          upgradeValue = upgradedModule.power
        of ModuleType2.cabin:
          upgradedModule.quality += (modulesList[
              upgradedModule.protoIndex].maxValue / 20).int
          upgradeValue = upgradedModule.quality
        of ModuleType2.gun:
          if (modulesList[upgradedModule.protoIndex].maxValue / 20).int > 0:
            upgradedModule.damage += (modulesList[
                upgradedModule.protoIndex].maxValue / 20).int
          else:
            upgradedModule.damage.inc
          upgradeValue = upgradedModule.damage
        of ModuleType2.batteringRam:
          if (modulesList[upgradedModule.protoIndex].maxValue / 20).int > 0:
            upgradedModule.damage2 += (modulesList[
                upgradedModule.protoIndex].maxValue / 20).int
          else:
            upgradedModule.damage2.inc
          upgradeValue = upgradedModule.damage2
        of ModuleType2.harpoonGun:
          if (modulesList[upgradedModule.protoIndex].maxValue / 20).int > 0:
            upgradedModule.duration += (modulesList[
                upgradedModule.protoIndex].maxValue / 20).int
          else:
            upgradedModule.duration.inc
          upgradeValue = upgradedModule.duration
        else:
          discard
      of value:
        if upgradedModule.mType == ModuleType2.engine:
          weightGain *= 10
          upgradedModule.fuelUsage.dec
          upgradeValue = upgradedModule.fuelUsage
        upgradedModule.weight += weightGain
        addMessage(message = playerShip.crew[workerIndex].name &
            " has upgraded " & upgradedModule.name & ".", mType = orderMessage, color = green)
        localMaxValue = (modulesList[upgradedModule.protoIndex].value.float / 2.0).int
        if localMaxValue < 1:
          localMaxValue = 1
        if upgradeValue == localMaxValue:
          maxUpgradeReached(messageText = "You've reached the maximum upgrade for ")
          return
        if modulesList[upgradedModule.protoIndex].mType == ModuleType.engine:
          upgradedModule.upgradeProgress = ((modulesList[
              upgradedModule.protoIndex].value * 20).float *
              newGameSettings.upgradeCostBonus).int
          if upgradedModule.upgradeProgress == 0:
            upgradedModule.upgradeProgress = 1
      else:
        discard
    else:
      upgradedModule.upgradeProgress = upgradeProgress
  playerShip.modules[playerShip.upgradeModule] = upgradedModule

proc startUpgrading*(moduleIndex: Natural, upgradeType: Positive) {.raises: [
    ShipUpgradeError, KeyError], tags: [], contractual.} =
  ## Set the module's upgrade of the player's ship
  ##
  ## * moduleIndex - the index of the module to upgrade
  ## * upgradeType - the type of the upgrade
  if playerShip.modules[moduleIndex].durability == 0 and upgradeType != 3:
    raise newException(exceptn = ShipUpgradeError,
      message = "You can't upgrade " & playerShip.modules[moduleIndex].name & " because it's destroyed.")
  var
    upgradeAction: ShipUpgrade = none
    upgradeProgress: int = 0
  case upgradeType
  of 1:
    let maxValue: Natural = (modulesList[playerShip.modules[
        moduleIndex].protoIndex].durability.float * 1.5).Natural
    if playerShip.modules[moduleIndex].maxDurability == maxValue:
      raise newException(exceptn = ShipUpgradeError,
        message = "You can't futher improve the durability of " &
        playerShip.modules[moduleIndex].name & ".")
    upgradeAction = durability
    upgradeProgress = (modulesList[playerShip.modules[
        moduleIndex].protoIndex].durability.float *
        newGameSettings.upgradeCostBonus).Natural
  of 2:
    let maxValue: Natural = (modulesList[playerShip.modules[
        moduleIndex].protoIndex].maxValue.float * 1.5).Natural
    case modulesList[playerShip.modules[moduleIndex].protoIndex].mType
    of engine:
      if playerShip.modules[moduleIndex].power == maxValue:
        raise newException(exceptn = ShipUpgradeError,
            message = "You can't futher improve the power of " &
            playerShip.modules[moduleIndex].name & ".")
      upgradeProgress = ((modulesList[playerShip.modules[
          moduleIndex].protoIndex].maxValue.float / 20.0) *
          newGameSettings.upgradeCostBonus).Natural
    of cabin:
      if playerShip.modules[moduleIndex].quality == maxValue:
        raise newException(exceptn = ShipUpgradeError,
            message = "You can't futher improve the quality of " &
            playerShip.modules[moduleIndex].name & ".")
      upgradeProgress = (modulesList[playerShip.modules[
          moduleIndex].protoIndex].maxValue.float *
          newGameSettings.upgradeCostBonus).Natural
    of gun, batteringRam:
      let damage: Natural = if playerShip.modules[moduleIndex].mType ==
          ModuleType2.gun: playerShip.modules[moduleIndex].damage
          else:
            playerShip.modules[moduleIndex].damage2
      if damage == maxValue:
        raise newException(exceptn = ShipUpgradeError,
            message = "You can't futher improve the damage of " &
            playerShip.modules[moduleIndex].name & ".")
      upgradeProgress = ((modulesList[playerShip.modules[
          moduleIndex].protoIndex].maxValue.float * 2.0) *
          newGameSettings.upgradeCostBonus).Natural
    of hull:
      if playerShip.modules[moduleIndex].maxModules == maxValue:
        raise newException(exceptn = ShipUpgradeError,
            message = "You can't futher enlarge the size of " &
            playerShip.modules[moduleIndex].name & ".")
      upgradeProgress = ((modulesList[playerShip.modules[
          moduleIndex].protoIndex].maxValue.float * 40.0) *
          newGameSettings.upgradeCostBonus).Natural
    of harpoonGun:
      if playerShip.modules[moduleIndex].duration == maxValue:
        raise newException(exceptn = ShipUpgradeError,
            message = "You can't futher improve the strength of " &
            playerShip.modules[moduleIndex].name & ".")
      upgradeProgress = ((modulesList[playerShip.modules[
          moduleIndex].protoIndex].maxValue.float * 10.0) *
          newGameSettings.upgradeCostBonus).Natural
    else:
      raise newException(exceptn = ShipUpgradeError,
          message = playerShip.modules[moduleIndex].name & " can't be upgraded in that way.")
    upgradeAction = ShipUpgrade.maxValue
  of 3:
    case modulesList[playerShip.modules[moduleIndex].protoIndex].mType
    of engine:
      var maxValue: Natural = (modulesList[playerShip.modules[
          moduleIndex].protoIndex].value.float / 2.0).Natural
      if maxValue < 1:
        maxValue = 1
      if playerShip.modules[moduleIndex].fuelUsage == maxValue:
        raise newException(exceptn = ShipUpgradeError,
            message = "You can't futher improve the fuel usage of " &
            playerShip.modules[moduleIndex].name & ".")
      upgradeProgress = ((modulesList[playerShip.modules[
          moduleIndex].protoIndex].value.float * 20.0) *
          newGameSettings.upgradeCostBonus).Natural
    else:
      raise newException(exceptn = ShipUpgradeError,
          message = playerShip.modules[moduleIndex].name & " can't be upgraded in that way.")
    upgradeAction = value
  of 4:
    if playerShip.modules[moduleIndex].upgradeAction == none:
      raise newException(exceptn = ShipUpgradeError,
          message = playerShip.modules[moduleIndex].name & " doesn't have any upgrade set yet.")
    upgradeAction = playerShip.modules[moduleIndex].upgradeAction
  else:
    return
  let materialIndex: int = findItem(inventory = playerShip.cargo,
      itemType = modulesList[playerShip.modules[
      moduleIndex].protoIndex].repairMaterial, itemQuality = any)
  if materialIndex == -1:
    for item in itemsList.values:
      if item.itemType == modulesList[playerShip.modules[
          moduleIndex].protoIndex].repairMaterial:
        raise newException(exceptn = ShipUpgradeError,
            message = "You don't have the " & item.name & " to upgrade " &
            playerShip.modules[moduleIndex].name & ".")
  playerShip.upgradeModule = moduleIndex
  if playerShip.modules[moduleIndex].upgradeAction != upgradeAction:
    playerShip.modules[moduleIndex].upgradeProgress = upgradeProgress
    if playerShip.modules[moduleIndex].upgradeProgress == 0:
      playerShip.modules[moduleIndex].upgradeProgress = 1
    playerShip.modules[moduleIndex].upgradeAction = upgradeAction
  addMessage(message = "You set the " & playerShip.modules[moduleIndex].name &
      " to upgrade.", mType = orderMessage)
