# Copyright 2023-2024 Bartek thindil Jasicki
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

## Provides code related to ships' crews like generating names for members,
## getting skills' levels for members, etc.

import std/[strutils, tables]
import contracts
import careers, config, crewinventory, game, maps, messages, shipscargo, types, utils

proc generateMemberName*(gender: char; factionIndex: string): string {.sideEffect,
    raises: [], tags: [], contractual.} =
  ## Generate the name for the mob, based on his/her faction. Based on
  ## libtcod names generator
  ##
  ## * gender       - The gender of the mob, M - male, F - female
  ## * factionIndex - The index of the faction to which the mob belongs
  ##
  ## Returns the randomly generated name of the mob
  require:
    gender.toLowerAscii in {'m', 'f'}
    factionIndex.len > 0
  ensure:
    result.len > 0
  body:
    try:
      if factionsList[factionIndex].namesType == robotic:
        return $generateRoboticName();
    except KeyError:
      discard
    if gender == 'M':
      result = malesSyllablesStartList[getRandom(min = 0, max = (
          malesSyllablesStartList.len - 1))]
      result = result & malesVocalsList[getRandom(min = 0, max = (
          malesVocalsList.len - 1))]
      if getRandom(min = 1, max = 100) < 36:
        result = result & malesSyllablesMiddleList[getRandom(min = 0, max = (
            malesSyllablesMiddleList.len - 1))]
      if getRandom(min = 1, max = 100) < 11:
        result = result & malesConsonantsList[getRandom(min = 0, max = (
            malesConsonantsList.len - 1))]
      result = result & malesSyllablesEndList[getRandom(min = 0, max = (
          malesSyllablesEndList.len - 1))]
      return
    result = femalesSyllablesStartList[getRandom(min = 0, max = (
        femalesSyllablesStartList.len - 1))]
    result = result & femalesVocalsList[getRandom(min = 0, max = (
        femalesVocalsList.len - 1))]
    if getRandom(min = 1, max = 100) < 36:
      result = result & femalesSyllablesMiddleList[getRandom(min = 0, max = (
          femalesSyllablesMiddleList.len - 1))]
    if getRandom(min = 1, max = 100) < 11:
      result = result & femalesSyllablesMiddleList[getRandom(min = 0, max = (
          femalesSyllablesMiddleList.len - 1))]
    result = result & femalesSyllablesEndList[getRandom(min = 0, max = (
        femalesSyllablesEndList.len - 1))]

proc getSkillLevel*(member: MemberData; skillIndex: Positive): int {.sideEffect,
    raises: [KeyError], tags: [], contractual.} =
  ## Get the real level of the selected skill of the selected crew member.
  ##
  ## * member     - the member which skill will be get
  ## * skillIndex - the index of the skill which will be get
  ##
  ## Returns the selected skill level with bonuses or maluses from health,
  ## hunger, tiredness and morale
  ensure:
    result in 0 .. 100
  body:
    result = 0
    for skill in member.skills:
      if skill.index == skillIndex:
        let baseSkillLevel = skill.level + member.attributes[skillsList[
            skill.index].attribute].level
        var damage = 1.0 - (member.health.float / 100.0)
        result = result + (baseSkillLevel - (baseSkillLevel.float * damage).int)
        if member.thirst > 40:
          damage = 1.0 - (member.thirst.float / 100.0)
          result = result - (baseSkillLevel - (baseSkillLevel.float * damage).int)
        if member.hunger > 80:
          damage = 1.0 - (member.hunger.float / 100.0)
          result = result - (baseSkillLevel - (baseSkillLevel.float * damage).int)
        if member.morale[1] < 25:
          damage = member.morale[1].float / 100.0
          result = result - (baseSkillLevel - (baseSkillLevel.float * damage).int)
        if result < 1:
          result = 1
        elif result > 100:
          result = 100
        if member.morale[1] > 90:
          damage = result.float / 100.0
          result = result + (baseSkillLevel.float * damage).int
          if result > 100:
            result = 100
        return

proc updateMorale*(ship: var ShipRecord; memberIndex: Natural;
    value: int) {.sideEffect, raises: [KeyError], tags: [], contractual.} =
  ## Update the morale of the selected crew member in the selected ship
  ##
  ## * ship        - the ship in which the crew member's morale will be changed
  ## * memberIndex - the index of the crew member which morale will be changed
  ## * value       - the value with which the morale will be changed
  ##
  ## Returns the modified ship parameter with updated morale of the selected crew
  ## member.
  require:
    memberIndex < ship.crew.len
  body:
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

proc updateOrders*(ship: var ShipRecord; combat: bool = false) {.sideEffect,
    raises: [CrewOrderError, KeyError, CrewNoSpaceError, Exception], tags: [
    RootEffect], contractual.}
  ## Update the orders of the crew of the selected ship, based on the crew
  ## members orders' priorities
  ##
  ## * ship   - the ship of which crew's orders will be updated
  ## * combat - if true, the orders update takes place in a combat. Default
  ##            value is false.
  ##
  ## Returns the modified parameter ship with updated info about the ship

proc giveOrders*(ship: var ShipRecord; memberIndex: Natural;
    givenOrder: CrewOrders; moduleIndex: int = -1;
    checkPriorities: bool = true) {.sideEffect, raises: [CrewOrderError,
    KeyError, CrewNoSpaceError, Exception], tags: [RootEffect], contractual.} =
  ## Give the selected order to the selected crew member and update orders of
  ## the whole crew if needed
  ##
  ## * ship            - the ship in which the crew member will have given order
  ## * memberIndex     - the index of the crew member which will have given order
  ## * givenOrder      - the order to give to the crew member
  ## * moduleIndex     - the index of module related to the order. Can be empty.
  ##                     Default value is -1.
  ## * checkPriorities - if true, update orders of the crew after succesfully
  ##                     assigned the selected order. Can be empty. Default value
  ##                     is true.
  ##
  ## Returns the modified parameter ship with updated info about the ship
  require:
    memberIndex < ship.crew.len
    moduleIndex < ship.modules.len
  body:
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
        moduleIndex > -1):
      var freePosition: bool = false
      for owner in ship.modules[moduleIndex].owner:
        if owner == -1:
          freePosition = true
          break
      if not freePosition:
        giveOrders(ship = ship, memberIndex = ship.modules[moduleIndex].owner[
            0], givenOrder = rest, moduleIndex = -1, checkPriorities = false)
    var moduleIndex2 = -1
    if moduleIndex == -1 and givenOrder in [pilot, engineer, rest]:
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
          if modulesList[module.protoIndex].mType == mType and
              module.durability > 0:
            if module.owner[0] > -1:
              giveOrders(ship = ship, memberIndex = module.owner[0],
                  givenOrder = rest, moduleIndex = -1, checkPriorities = false)
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
    if moduleIndex2 == -1 and ship.crew == playerShip.crew:
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
                  if owner == -1:
                    ship.modules[index].owner[i] = memberIndex;
                    addMessage(message = (memberName & " takes " & module.name &
                        " as their own cabin."),
                        mType = otherMessage)
                    break takeCabin
        else:
          discard
    block releaseModule:
      for index, module in ship.modules.pairs:
        if module.mType != ModuleType2.cabin:
          for i, owner in module.owner.pairs:
            if owner == memberIndex:
              ship.modules[index].owner[i] = -1
              break releaseModule
    var
      toolsIndex = -1
      requiredTool = ""
    if toolsIndex > -1 and ship.crew[memberIndex].equipment[tool] != toolsIndex:
      updateInventory(memberIndex = memberIndex, amount = 1,
          protoIndex = ship.cargo[toolsIndex].protoIndex,
              durability = ship.cargo[
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
      toolsIndex = -1
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
        if toolsIndex == -1:
          toolsIndex = findItem(inventory = ship.cargo, itemType = requiredTool,
              quality = toolQuality)
          if toolsIndex == -1:
            toolsIndex = findItem(inventory = ship.crew[memberIndex].inventory,
                itemType = requiredTool, quality = toolQuality)
            if toolsIndex > -1:
              ship.crew[memberIndex].equipment[tool] = toolsIndex
          else:
            ship.crew[memberIndex].equipment[tool] = -1
        if toolsIndex == -1:
          case givenOrder
            of repair:
              raise newException(exceptn = CrewOrderError,
                  message = memberName & " can't start repairing ship because you don't have the proper tools.")
            of clean:
              raise newException(exceptn = CrewOrderError,
                  message = memberName & " can't start cleaning ship because you don't have any cleaning tools.")
            of upgrading:
              raise newException(exceptn = CrewOrderError,
                  message = memberName & " can't start upgrading module because you don't have the proper tools.")
            of train:
              raise newException(exceptn = CrewOrderError,
                  message = memberName & " can't start training because you don't have the proper tools.")
            else:
              return
    if givenOrder == rest:
      ship.crew[memberIndex].previousOrder = rest
      if ship.crew[memberIndex].order in [repair, clean, upgrading, train]:
        toolsIndex = ship.crew[memberIndex].equipment[tool]
        if toolsIndex > -1:
          updateCargo(ship = ship, protoIndex = ship.crew[
              memberIndex].inventory[toolsIndex].protoIndex, amount = 1,
                  durability = ship.crew[
              memberIndex].inventory[toolsIndex].durability)
          updateInventory(memberIndex = memberIndex, amount = -1,
              inventoryIndex = toolsIndex, ship = ship)
    if ship.crew == playerShip.crew:
      case givenOrder
        of pilot:
          addMessage(message = memberName & " starts piloting.",
              mType = orderMessage)
          ship.modules[moduleIndex2].owner[0] = memberIndex
        of engineer:
          addMessage(message = memberName & " starts engineer's duty.",
              mType = orderMessage)
        of gunner:
          addMessage(message = memberName & " starts operating gun.",
              mType = orderMessage)
          ship.modules[moduleIndex2].owner[0] = memberIndex
        of rest:
          addMessage(message = memberName & " is going on break.",
              mType = orderMessage)
        of repair:
          addMessage(message = memberName & " starts repairing ship.",
              mType = orderMessage)
        of craft:
          addMessage(message = memberName & " starts manufacturing.",
              mType = orderMessage)
          for index, owner in ship.modules[moduleIndex2].owner.pairs:
            if owner == -1:
              ship.modules[moduleIndex2].owner[index] = memberIndex
              break
        of upgrading:
          addMessage(message = memberName & " starts upgrading " & ship.modules[
              ship.upgradeModule].name & ".",
              mType = orderMessage)
        of talk:
          addMessage(message = memberName &
              " is now assigned to talking in bases.",
              mType = orderMessage)
        of heal:
          addMessage(message = memberName &
              " starts healing wounded crew members.",
              mType = orderMessage)
          if moduleIndex > -1:
            for index, owner in ship.modules[moduleIndex].owner.pairs:
              if owner == -1:
                ship.modules[moduleIndex2].owner[index] = memberIndex
                break
        of clean:
          addMessage(message = memberName & " starts cleaning ship.",
              mType = orderMessage)
        of boarding:
          addMessage(message = memberName &
              " starts boarding the enemy ship.",
              mType = orderMessage)
        of defend:
          addMessage(message = memberName &
              " starts defending the ship.",
              mType = orderMessage)
        of train:
          addMessage(message = memberName &
              " starts personal training.", mType = orderMessage)
          for index, owner in ship.modules[moduleIndex2].owner.pairs:
            if owner == -1:
              ship.modules[moduleIndex2].owner[index] = memberIndex
              break
    ship.crew[memberIndex].order = givenOrder
    ship.crew[memberIndex].orderTime = 15
    if givenOrder != rest:
      updateMorale(ship = ship, memberIndex = memberIndex, value = -1)
    if checkPriorities:
      updateOrders(ship = ship)

proc updateOrders*(ship: var ShipRecord; combat: bool = false) {.sideEffect,
    raises: [CrewOrderError, KeyError, CrewNoSpaceError, Exception], tags: [
    RootEffect], contractual.} =
  ## Update the orders of the crew of the selected ship, based on the crew
  ## members orders' priorities
  ##
  ## * ship   - the ship of which crew's orders will be updated
  ## * combat - if true, the orders update takes place in a combat. Default
  ##            value is false.
  ##
  ## Returns the modified parameter ship with updated info about the ship

  proc updatePosition(ship: var ShipRecord; order: CrewOrders;
      maxPriority: bool = true): bool {.sideEffect, raises: [CrewOrderError,
      KeyError, CrewNoSpaceError, Exception], tags: [RootEffect],
          contractual.} =
    ## Change the crew member for the selected order
    ##
    ## * ship        - the ship of which crew's orders will be updated
    ## * order       - the order which will be updated
    ## * maxPriority - if true, get the crew member which high priority for
    ##                 the order
    ##
    ## Returns true if order was updated, otherwise false
    var
      orderIndex: Natural
      memberIndex, moduleIndex: int = -1
    orderIndex = (if order < defend: (order.ord + 1) else: order.ord)
    if maxPriority:
      for index, member in ship.crew.pairs:
        if member.orders[orderIndex] == 2 and member.order != order and
            member.previousOrder != order:
          memberIndex = index
          break
    else:
      for index, member in ship.crew.pairs:
        if member.orders[orderIndex] == 1 and member.order == rest and
            member.previousOrder == rest:
          memberIndex = index
          break
    if memberIndex == -1:
      return false
    if order in [gunner, craft, heal, pilot, engineer, train]:
      for index, module in ship.modules.pairs:
        if module.durability > 0:
          case module.mType
            of ModuleType2.gun:
              if order == gunner and module.owner[0] == -1:
                moduleIndex = index
                break
            of ModuleType2.workshop:
              if order == craft and module.craftingIndex.len > 0:
                for owner in module.owner:
                  if owner == -1:
                    moduleIndex = index
                    break
            of ModuleType2.medicalRoom:
              if order == heal:
                for owner in module.owner:
                  if owner == -1:
                    moduleIndex = index
                    break
            of ModuleType2.cockpit:
              if order == pilot:
                moduleIndex = index
                break
            of ModuleType2.engine:
              if order == engineer:
                moduleIndex = index
                break
            of ModuleType2.trainingRoom:
              if order == train and module.trainedSkill > 0:
                for owner in module.owner:
                  if owner == -1:
                    moduleIndex = index
                    break
            else:
              discard
        if moduleIndex > -1:
          break
      if moduleIndex == -1:
        return false
    if ship.crew[memberIndex].order != rest:
      giveOrders(ship = ship, memberIndex = memberIndex, givenOrder = rest,
          moduleIndex = -1, checkPriorities = false)
    giveOrders(ship = ship, memberIndex = memberIndex, givenOrder = order,
        moduleIndex = moduleIndex)
    return true

  var havePilot, haveEngineer, haveUpgrade, haveTrader, canHeal, needGunners,
    needCrafters, needClean, needRepairs, needTrader: bool = false
  for member in ship.crew:
    case member.order
      of pilot:
        havePilot = true
      of engineer:
        haveEngineer = true
      of upgrading:
        haveUpgrade = true
      of talk:
        haveTrader = true
      else:
        discard
    if member.health < 100:
      if findItem(inventory = ship.cargo, itemType = factionsList[
          member.faction].healingTools) > -1:
        canHeal = true
  for module in ship.modules:
    if module.durability > 0:
      case module.mType
        of ModuleType2.gun:
          if module.owner[0] == -1 and not needGunners:
            needGunners = true
        of ModuleType2.workshop:
          if module.craftingIndex.len > 0 and not needCrafters:
            for owner in module.owner:
              if owner == -1:
                needCrafters = true
                break
        of ModuleType2.cabin:
          if module.cleanliness < module.quality:
            needClean = true
        else:
          discard
    if module.durability < module.maxDurability and not needRepairs:
      for item in ship.cargo:
        if itemsList[item.protoIndex].itemType == modulesList[
            module.protoIndex].repairMaterial:
          needRepairs = true
          break
  if skyMap[ship.skyX][ship.skyY].baseIndex > 0:
    needTrader = true
  let eventIndex = skyMap[ship.skyX][ship.skyY].eventIndex
  if not needTrader and eventIndex > 0 and eventsList[eventIndex].eType in [
      trader, friendlyShip]:
    needTrader = true
  if not havePilot and updatePosition(ship = ship, order = pilot):
    updateOrders(ship = ship)
  if not haveEngineer and updatePosition(ship = ship, order = engineer):
    updateOrders(ship = ship)
  if needGunners and updatePosition(ship = ship, order = gunner):
    updateOrders(ship = ship)
  if needCrafters and updatePosition(ship = ship, order = craft):
    updateOrders(ship = ship)
  if not haveUpgrade and ship.upgradeModule > -1 and findItem(
      inventory = ship.cargo, itemType = repairTools) > -1:
    if findItem(inventory = ship.cargo, itemType = modulesList[ship.modules[
        ship.upgradeModule].protoIndex].repairMaterial) > -1 and updatePosition(
            ship = ship, order = upgrading):
      updateOrders(ship = ship)
  if (not haveTrader and needTrader) and updatePosition(ship = ship, order = talk):
    updateOrders(ship = ship)
  if (needClean and findItem(inventory = ship.cargo, itemType = cleaningTools) >
      -1) and updatePosition(ship = ship, order = clean):
    updateOrders(ship = ship)
  if canHeal and updatePosition(ship = ship, order = heal):
    updateOrders(ship = ship)
  if (needRepairs and findItem(inventory = ship.cargo, itemType = repairTools) >
      -1) and updatePosition(ship = ship, order = repair):
    updateOrders(ship = ship)
  if combat:
    if updatePosition(ship = ship, order = defend):
      updateOrders(ship = ship)
    if updatePosition(ship = ship, order = boarding):
      updateOrders(ship = ship)
  if updatePosition(ship = ship, order = train):
    updateOrders(ship = ship)
  if not havePilot and updatePosition(ship = ship, order = pilot,
      maxPriority = false):
    updateOrders(ship = ship)
  if not haveEngineer and updatePosition(ship = ship, order = engineer,
      maxPriority = false):
    updateOrders(ship = ship)
  if needGunners and updatePosition(ship = ship, order = gunner,
      maxPriority = false):
    updateOrders(ship = ship)
  if needCrafters and updatePosition(ship = ship, order = craft,
      maxPriority = false):
    updateOrders(ship = ship)
  if not haveUpgrade and ship.upgradeModule > -1 and findItem(
      inventory = ship.cargo, itemType = repairTools) > -1:
    if findItem(inventory = ship.cargo, itemType = modulesList[ship.modules[
        ship.upgradeModule].protoIndex].repairMaterial) > -1 and updatePosition(
            ship = ship, order = upgrading, maxPriority = false):
      updateOrders(ship = ship)
  if (not haveTrader and needTrader) and updatePosition(ship = ship,
      order = talk, maxPriority = false):
    updateOrders(ship = ship)
  if (needClean and findItem(inventory = ship.cargo, itemType = cleaningTools) >
      -1) and updatePosition(ship = ship, order = clean, maxPriority = false):
    updateOrders(ship = ship)
  if canHeal and updatePosition(ship = ship, order = heal, maxPriority = false):
    updateOrders(ship = ship)
  if (needRepairs and findItem(inventory = ship.cargo, itemType = repairTools) >
      -1) and updatePosition(ship = ship, order = repair, maxPriority = false):
    updateOrders(ship = ship)
  if combat:
    if updatePosition(ship = ship, order = defend, maxPriority = false):
      updateOrders(ship = ship)
    if updatePosition(ship = ship, order = boarding, maxPriority = false):
      updateOrders(ship = ship)
  if updatePosition(ship = ship, order = train, maxPriority = false):
    updateOrders(ship = ship)

proc findMember*(order: CrewOrders; shipCrew: seq[
    MemberData] = playerShip.crew): int {.sideEffect, raises: [], tags: [],
    contractual.} =
  ## Find the first member of the selected crew with the selected order
  ##
  ## * order    - the order for which looking for
  ## * shipCrew - the crew in which the crew member will be looking for
  ##
  ## Returns the index of the crew member with the selected order or -1 if
  ## nothing found.
  for index, member in shipCrew.pairs:
    if member.order == order:
      return index
  return -1

proc gainExp*(amount: Natural; skillNumber: Positive;
    crewIndex: Natural) {.sideEffect, raises: [], tags: [], contractual.} =
  ## Raise the crew member experience in the selected skill and associated
  ## attribute
  ##
  ## * amount      - the amount of experience gained by the crew mmeber
  ## * skillNumber - the index of the skill in which the experience is gained
  ## * crewIndex   - the index of the crew member who gains experience
  require:
    skillsList.contains(key = skillNumber)
    crewIndex < playerShip.crew.len
  body:
    let attributeIndex = try:
        skillsList[skillNumber].attribute
      except KeyError:
        Positive.high
    if attributeIndex == Positive.high:
      return
    var
      skillExp, newAmount, skillLevel = 0
      skillIndex = -1

    proc gainExpInAttribute(attribute: Natural) {.sideEffect, raises: [],
        tags: [], contractual.} =
      ## Raise the crew member experience in the attribute associated with the
      ## skill
      ##
      ## * attribute - the index of the attribute in which experience will be
      ##               gained
      require:
        attribute < playerShip.crew[crewIndex].attributes.len
      body:
        var memberAttribute = playerShip.crew[crewIndex].attributes[attribute]
        if memberAttribute.level == 50:
          return
        var
          attributeExp = memberAttribute.experience + newAmount
          attributeLevel = memberAttribute.level
        if attributeExp >= attributeLevel * 250:
          attributeExp = attributeExp - (attributeLevel * 250)
          attributeLevel.inc
        playerShip.crew[crewIndex].attributes[attribute].level = attributeLevel
        playerShip.crew[crewIndex].attributes[
            attribute].experience = attributeExp

    newAmount = try:
        if skillsList[skillNumber].name in careersList[playerCareer].skills:
          amount + (amount / 2).int
        else:
          amount
      except KeyError:
        -1
    if newAmount == -1:
      return
    newAmount = (newAmount.float * newGameSettings.experienceBonus).int
    if newAmount == 0:
      return
    gainExpInAttribute(attribute = conditionIndex)
    gainExpInAttribute(attribute = attributeIndex)
    for i in playerShip.crew[crewIndex].skills.low..playerShip.crew[
        crewIndex].skills.high:
      if playerShip.crew[crewIndex].skills[i].index == skillNumber:
        skillIndex = i
        break
    if skillIndex > -1:
      if playerShip.crew[crewIndex].skills[skillIndex].level == SkillRange.high:
        return
      skillLevel = playerShip.crew[crewIndex].skills[skillIndex].level
      skillExp = playerShip.crew[crewIndex].skills[skillIndex].experience + newAmount
    if skillExp >= skillLevel * 25:
      skillExp = skillExp - (skillLevel * 25)
      skillLevel.inc
    if skillIndex > -1:
      playerShip.crew[crewIndex].skills[skillIndex] = SkillInfo(
          index: skillNumber, level: skillLevel, experience: skillExp)
    else:
      playerShip.crew[crewIndex].skills.add(y = SkillInfo(index: skillNumber,
          level: skillLevel, experience: skillExp))

# Temporary code for interfacing with Ada

proc generateAdaMemberName(gender: char;
    factionIndex: cstring): cstring {.raises: [], tags: [], exportc,
        contractual.} =
  ## Temporary C binding
  return generateMemberName(gender = gender,
      factionIndex = $factionIndex).cstring

proc giveAdaOrders(isPlayerShip, memberIndex, givenOrder, moduleIndex,
  checkPriorities: cint): cstring {.raises: [], tags: [RootEffect], exportc,
      contractual.} =
  ## Temporary C binding
  try:
    if isPlayerShip == 1:
      giveOrders(ship = playerShip, memberIndex = memberIndex - 1,
          givenOrder = givenOrder.CrewOrders, moduleIndex = moduleIndex - 1,
          checkPriorities = (if checkPriorities == 1: true else: false))
    else:
      giveOrders(ship = npcShip, memberIndex = memberIndex - 1,
          givenOrder = givenOrder.CrewOrders, moduleIndex = moduleIndex - 1,
          checkPriorities = (if checkPriorities == 1: true else: false))
  except:
    return getCurrentExceptionMsg().cstring
  return "".cstring

proc updateAdaOrders(isPlayerShip, combat: cint) {.raises: [], tags: [
    RootEffect], exportc, contractual.} =
  ## Temporary C binding
  try:
    if isPlayerShip == 1:
      updateOrders(ship = playerShip, combat = (if combat ==
          1: true else: false))
    else:
      updateOrders(ship = npcShip, combat = (if combat == 1: true else: false))
  except KeyError, Exception:
    discard

proc getAdaSkillLevel(member: AdaMemberData;
    skillIndex: cint): cint {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  try:
    return getSkillLevel(member = adaMemberToNim(adaMember = member),
        skillIndex = skillIndex.Positive).cint
  except KeyError:
    return 0

proc findAdaMember(order, inPlayerShip: cint): cint {.raises: [], tags: [],
    exportc, contractual.} =
  ## Temporary C binding
  if inPlayerShip == 1:
    return findMember(order = order.CrewOrders).cint + 1
  else:
    return findMember(order = order.CrewOrders, shipCrew = npcShip.crew).cint + 1

proc updateAdaMorale(isPlayerShip, memberIndex, value: cint) {.raises: [],
    tags: [], exportc, contractual.} =
  ## Temporary C binding
  try:
    if isPlayerShip == 1:
      updateMorale(ship = playerShip, memberIndex = memberIndex - 1, value = value)
    else:
      updateMorale(ship = npcShip, memberIndex = memberIndex - 1, value = value)
  except KeyError:
    discard

proc gainAdaExp(amount, skillNumber, crewIndex: cint) {.raises: [], tags: [],
    exportc, contractual.} =
  ## Temporary C binding
  gainExp(amount = amount.Natural, skillNumber = skillNumber.Positive,
      crewIndex = crewIndex.Natural - 1)
