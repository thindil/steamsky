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
import crew, crewinventory, game, messages, shipmodules, ships, shipscargo,
    types, utils

proc updateOrders*(ship: var ShipRecord; combat: bool = false)

proc giveOrders*(ship: var ShipRecord; memberIndex: Natural;
    givenOrder: CrewOrders; moduleIndex: int = -1;
    checkPriorities: bool = true) =
  if givenOrder == ship.crew[memberIndex].order:
    if givenOrder in [craft, gunner]:
      for index, module in ship.modules.pairs:
        if index == moduleIndex:
          for owner in module.owner:
            if owner == memberIndex:
              return
    else:
      return
  let memberName = ship.crew[memberIndex].name
  if givenOrder != rest and ((ship.crew[memberIndex].morale[1] < 11 and
      getRandom(min = 1, max = 100) < 50) or ship.crew[memberIndex].loyalty < 20):
    if ship.crew == playerShip.crew:
      raise newException(exceptn = CrewOrderError, message = memberName & " refuses to execute order.")
  if givenOrder == train and ship.modules[moduleIndex].trainedSkill == 0:
    raise newException(exceptn = CrewOrderError, message = memberName &
        " can't start training because " & ship.modules[moduleIndex].name & " isn't prepared.")
  if givenOrder in [pilot, engineer, upgrading, talk]:
    for index, member in ship.crew.pairs:
      if member.order == givenOrder:
        giveOrders(ship = ship, memberIndex = index, givenOrder = rest,
            moduleIndex = -1, checkPriorities = false)
        break
  elif givenOrder in [gunner, craft, train] or (givenOrder == heal and
      moduleIndex > 0):
    var freePosition: bool = false
    for owner in ship.modules[moduleIndex].owner:
      if owner == 0:
        freePosition = true
        break
    if not freePosition:
      giveOrders(ship = ship, memberIndex = ship.modules[moduleIndex].owner[0],
          givenOrder = rest, moduleIndex = -1, checkPriorities = false)
  var moduleIndex2 = 0
  if moduleIndex == 0 and givenOrder in [pilot, engineer, rest]:
    let mType: ModuleType = case givenOrder
      of pilot:
        ModuleType.cockpit
      of engineer:
        ModuleType.engine
      of rest:
        ModuleType.cabin
      else:
        ModuleType.engine
    for index, module in ship.modules.pairs:
      if mType != ModuleType.cabin:
        if modulesList[module.protoIndex].mType == mType and module.durability > 0:
          if module.owner[0] > 0:
            giveOrders(ship = ship, memberIndex = module.owner[0],
                givenOrder = rest, moduleIndex = 0, checkPriorities = false)
          moduleIndex2 = index
          break
      else:
        if module.mType == ModuleType2.cabin and module.durability > 0:
          for owner in module.owner:
            if memberIndex == owner:
              moduleIndex2 = index
              break
  else:
    moduleIndex2 = moduleIndex
  if moduleIndex2 == 0 and ship.crew == playerShip.crew:
    case givenOrder
      of pilot:
        raise newException(exceptn = CrewOrderError, message = memberName & " can't start piloting because the cockpit is destroyed or you don't have cockpit.")
      of engineer:
        raise newException(exceptn = CrewOrderError, message = memberName & " can't start engineer's duty because all of the engines are destroyed or you don't have engine.")
      of gunner:
        raise newException(exceptn = CrewOrderError, message = memberName & " can't start operating gun because all of the guns are destroyed or you don't have any installed.")
      of rest:
        block takeCabin:
          for index, module in ship.modules.pairs:
            if module.mType == ModuleType2.cabin and module.durability > 0:
              for i, owner in module.owner.pairs:
                if owner == 0:
                  ship.modules[index].owner[i] = memberIndex;
                  addMessage(message = (memberName & " takes " & module.name &
                      " as their own cabin.").cstring,
                      kind = otherMessage.ord.cint)
                  break take_cabin
      else:
        discard
  block releaseModule:
    for index, module in ship.modules.pairs:
      if module.mType != ModuleType2.cabin:
        for i, owner in module.owner.pairs:
          if owner == memberIndex:
            ship.modules[index].owner[i] = 0
            break releaseModule
  var
    toolsIndex = 0
    requiredTool = ""
  if toolsIndex > 0 and ship.crew[memberIndex].equipment[tool] != toolsIndex:
    updateInventory(memberIndex = memberIndex, amount = 1,
        protoIndex = ship.cargo[toolsIndex].protoIndex, durability = ship.cargo[
        toolsIndex].durability, ship = ship)
    updateCargo(ship = ship, amount = -1, cargoIndex = toolsIndex)
    ship.crew[memberIndex].equipment[tool] = findItem(inventory = ship.crew[
        memberIndex].inventory, itemType = requiredTool)
  toolsIndex = ship.crew[memberIndex].equipment[tool]
  if toolsIndex > 0 and itemsList[ship.crew[memberIndex].inventory[
      toolsIndex].protoIndex].itemType != requiredTool:
    updateCargo(ship = ship, protoIndex = ship.crew[memberIndex].inventory[
        toolsIndex].protoIndex, amount = 1, durability = ship.crew[
        memberIndex].inventory[toolsIndex].durability)
    updateInventory(memberIndex = memberIndex, amount = -1,
        inventoryIndex = toolsIndex, ship = ship)
    toolsIndex = 0
  var toolQuality = defaultItemDurability
  if givenOrder in [upgrading, repair, clean, train]:
    if givenOrder == clean:
      requiredTool = cleaningTools
    elif givenOrder == train:
      requiredTool = skillsList[ship.modules[moduleIndex].trainedSkill].tool
      toolQuality = getTrainingToolQuality(memberIndex = memberIndex,
          skillIndex = ship.modules[moduleIndex].trainedSkill)
    else:
      requiredTool = repairTools
    if requiredTool.len > 0:
      if toolsIndex == 0:
        toolsIndex = findItem(inventory = ship.cargo, itemType = requiredTool,
            quality = toolQuality)
        if toolsIndex == 0:
          toolsIndex = findItem(inventory = ship.crew[memberIndex].inventory,
              itemType = requiredTool, quality = toolQuality)
          if toolsIndex > 0:
            ship.crew[memberIndex].equipment[tool] = toolsIndex
        else:
          ship.crew[memberIndex].equipment[tool] = 0
      if toolsIndex == 0:
        case givenOrder
          of repair:
            raise newException(exceptn = CrewOrderError, message = memberName & " can't start repairing ship because you don't have the proper tools.")
          of clean:
            raise newException(exceptn = CrewOrderError, message = memberName & " can't start cleaning ship because you don't have any cleaning tools.")
          of upgrading:
            raise newException(exceptn = CrewOrderError, message = memberName & " can't start upgrading module because you don't have the proper tools.")
          of train:
            raise newException(exceptn = CrewOrderError, message = memberName & " can't start training because you don't have the proper tools.")
          else:
            return
  if givenOrder == rest:
    ship.crew[memberIndex].previousOrder = rest
    if ship.crew[memberIndex].order in [repair, clean, upgrading, train]:
      toolsIndex = ship.crew[memberIndex].equipment[tool]
      if toolsIndex > 0:
        updateCargo(ship = ship, protoIndex = ship.crew[memberIndex].inventory[
            toolsIndex].protoIndex, amount = 1, durability = ship.crew[
            memberIndex].inventory[toolsIndex].durability)
        updateInventory(memberIndex = memberIndex, amount = -1,
            inventoryIndex = toolsIndex, ship = ship)

proc updateOrders(ship: var ShipRecord; combat: bool = false) =
  discard

proc updateMorale*(ship: var ShipRecord; memberIndex: Natural;
    value: int) {.sideEffect, raises: [KeyError], tags: [].} =
  ## FUNCTION
  ##
  ## Update the morale of the selected crew member in the selected ship
  ##
  ## PARAMETERS
  ##
  ## * ship        - the ship in which the crew member's morale will be changed
  ## * memberIndex - the index of the crew member which morale will be changed
  ## * value       - the value with which the morale will be changed
  ##
  ## RETURNS
  ##
  ## The modified ship parameter with updated morale of the selected crew
  ## member.
  var newMorale, newLoyalty, newValue: int
  let factionIndex = ship.crew[memberIndex].faction
  if "nomorale" in factionsList[factionIndex].flags:
    return
  newValue = value
  if "fanaticism" in factionsList[factionIndex].flags:
    if value > 0:
      newValue = value * 5
    else:
      newValue = (value / 10).int
      if newValue == 0 and getRandom(min = 1, max = 10) <= value.abs:
        newValue = -1
      if newValue == 0:
        return
  newValue = ship.crew[memberIndex].morale[2] + newValue
  newMorale = ship.crew[memberIndex].morale[1]
  while newValue >= 5:
    newValue = newValue - 5
    newMorale = newMorale + 1
  while newValue < 0:
    newValue = newValue + 5
    newMorale = newMorale - 1
  if newMorale > 100:
    newMorale = 100
  elif newMorale < 0:
    newMorale = 0
  ship.crew[memberIndex].morale = [1: newMorale.Natural, 2: newValue.Natural]
  if ship.crew == playerShip.crew and memberIndex == 1:
    return
  newLoyalty = ship.crew[memberIndex].loyalty
  if newMorale > 74 and newLoyalty < 100:
    newLoyalty.inc
  if newMorale < 25 and newLoyalty > 0:
    newLoyalty = newLoyalty - getRandom(min = 5, max = 10)
  if newLoyalty > 100:
    newLoyalty = 100
  elif newLoyalty < 0:
    newLoyalty = 0
  ship.crew[memberIndex].loyalty = newLoyalty

# Temporary code for interfacing with Ada

proc updateAdaMorale(isPlayerShip, memberIndex, value: cint) {.exportc.} =
  if isPlayerShip == 1:
    updateMorale(ship = playerShip, memberIndex = memberIndex - 1, value = value)
  else:
    updateMorale(ship = npcShip, memberIndex = memberIndex - 1, value = value)
