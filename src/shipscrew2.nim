# Copyright 2023-2026 Bartek thindil Jasicki
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

## Provides code related to managing ship's crews like deleting members,
## killing them or getting their current order . Split from shipscrew
## module to avoid circular dependencies.

import std/tables
import contracts
import crafts, crewinventory, game, halloffame, items, messages, missions,
    shipscargo, shipscrew, types, utils

proc deleteMember*(memberIndex: Natural; ship: var ShipRecord) {.raises: [
    KeyError, ReputationError], tags: [], contractual.} =
  ## Delete the selected member from the selected ship crew list, update
  ## the ship modules with the new crew list and delete accepted missions
  ## if neccessary.
  ##
  ## * memberIndex - the crew index of the member to delete
  ## * ship        - the ship from which the crew member will be deleted
  ##
  ## Returns parameter ship with the updated list of crew members and modules
  require:
    memberIndex < ship.crew.len
  body:
    var deleted: bool = false
    if ship.crew == playerShip.crew:
      for index, mission in acceptedMissions:
        if mission.mType == passenger and mission.data == memberIndex:
          deleteMission(missionIndex = index)
          deleted = true
          break
      for mission in acceptedMissions.mitems:
        if mission.mType == passenger and mission.data > memberIndex:
          mission.data.inc
      if deleted:
        return
    ship.crew.delete(i = memberIndex)
    for module in ship.modules.mitems:
      for owner in module.owner.mitems:
        if owner == memberIndex:
          owner = 0
        elif owner > memberIndex:
          owner.dec

proc death*(memberIndex: Natural; reason: string; ship: var ShipRecord;
    createBody: bool = true) {.raises: [KeyError, IOError, ReputationError],
        tags: [
    WriteIOEffect], contractual.} =
  ## Handle the death of a crew member in ships
  ##
  ## * memberIndex - the crew index of the member which died
  ## * reason      - the reason of death of the crew member
  ## * ship        - the ship to which the crew member belongs
  ## * createBody  - if true, create the body for the crew member. Default
  ##                 value is true
  ##
  ## Returns the parameter ship with updated crew, modules and cargo lists
  require:
    memberIndex < ship.crew.len
    reason.len > 0
  body:
    let memberName: string = ship.crew[memberIndex].name
    if ship.crew == playerShip.crew:
      if memberIndex == 0:
        addMessage(message = "You died from " & reason & ".",
            mType = combatMessage, color = red)
        playerShip.crew[memberIndex].order = rest
        playerShip.crew[memberIndex].health = 0
        updateHallOfFame(playerName = playerShip.crew[memberIndex].name,
            deathReason = reason)
        return
      addMessage(message = memberName & " died from " & reason & ".",
          mType = combatMessage, color = red)
    if createBody:
      ship.cargo.add(y = InventoryData(protoIndex: corpseIndex, amount: 1,
          name: memberName & "'s corpse", durability: 100, price: 0))
    deleteMember(memberIndex = memberIndex, ship = ship)
    for index, _ in ship.crew:
      updateMorale(ship = ship, memberIndex = index, value = getRandom(
          min = -25, max = -10))

proc getCurrentOrder*(memberIndex: Natural): string {.raises: [
    ValueError], tags: [], contractual.} =
  ## Get the full information about the order of the selected crew member
  ##
  ## * memberIndex - the crew index of the member which order will be checked
  ##
  ## Returns a string with detailed information about the current order of the
  ## selected player's ship's crew member.
  require:
    memberIndex < playerShip.crew.len
  body:
    proc getModuleName(mType: ModuleType2): string {.raises: [],
        tags: [], contractual.} =
      ## Get the name of the selected module
      ##
      ## * mType - the type of the module which will be looking for
      ##
      ## Returns the name of the selected module
      result = ""
      for module in playerShip.modules:
        if module.mType == mType:
          for owner in module.owner:
            if owner == memberIndex:
              return module.name

    let member: MemberData = playerShip.crew[memberIndex]
    case member.order
    of pilot:
      result = "Piloting the ship"
    of engineer:
      result = "Engineering the ship"
    of gunner:
      result = "Operating " & getModuleName(mType = ModuleType2.gun)
    of repair:
      result = "Repairing the ship"
    of craft:
      for index, module in playerShip.modules:
        if module.mType == ModuleType2.workshop:
          for owner in module.owner:
            if owner == memberIndex:
              result = getWorkshopRecipeName(workshop = index) & " in " & module.name
              break
    of upgrading:
      result = "Upgrading " & playerShip.modules[playerShip.upgradeModule].name
    of talk:
      result = "Talking with others"
    of heal:
      result = "Healig the wounded in " & getModuleName(
          mType = ModuleType2.medicalRoom)
    of clean:
      result = "Cleaning the ship"
    of rest:
      result = "Resting in " & getModuleName(mType = ModuleType2.cabin) & ", no order"
    of defend:
      result = "Defending the ship"
    of boarding:
      result = "Boarding the enemy's ship"
    of train:
      block findTrainingRoom:
        for module in playerShip.modules:
          if module.mType == ModuleType2.trainingRoom:
            for owner in module.owner:
              if owner == memberIndex:
                result = "Training " & skillsList[module.trainedSkill].name &
                    " in " & module.name
                break findTrainingRoom

proc moveItem*(itemIndex: Natural; amount: Positive;
    memberIndex: Natural) {.raises: [NoFreeCargoError, KeyError,
    CrewNoSpaceError, CrewOrderError, Exception], tags: [RootEffect],
    contractual.} =
  ## Move the selected item to the player's ship's cargo
  ##
  ## * itemIndex   - the index in the crew member's inventory of item to move
  ## * amount      - the amount of the item to move
  ## * memberIndex - the index of the crew member from which inventory item
  ##                 will be moved
  require:
    memberIndex in playerShip.crew.low..playerShip.crew.high
  body:
    let item: InventoryData = playerShip.crew[memberIndex].inventory[itemIndex]
    if freeCargo(amount = 0 - (itemsList[item.protoIndex].weight * amount)) < 0:
      raise newException(exceptn = NoFreeCargoError,
          message = "No free space in ship cargo for tha amout of " &
          getItemName(item = item))
    updateCargo(ship = playerShip, protoIndex = item.protoIndex,
        amount = amount, durability = item.durability, price = item.price,
        quality = item.quality, maxDurability = item.maxDurability,
        weight = item.weight)
    updateInventory(memberIndex = memberIndex, amount = -amount,
        inventoryIndex = itemIndex, ship = playerShip,
        quality = item.quality)
    if (playerShip.crew[memberIndex].order == clean and findItem(
        inventory = playerShip.crew[memberIndex].inventory,
        itemType = cleaningTools, itemQuality = item.quality) == -1) or (
        playerShip.crew[memberIndex].order in {upgrading, repair} and findItem(
        inventory = playerShip.crew[memberIndex].inventory,
        itemType = repairTools, itemQuality = item.quality) == -1):
      giveOrders(ship = playerShip, memberIndex = memberIndex,
          givenOrder = rest)
