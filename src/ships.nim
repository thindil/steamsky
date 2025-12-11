# Copyright 2022-2025 Bartek thindil Jasicki
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

## Provides code related to ships, like getting a cabin's quality, loading
## the ships' prototypes from a file, damagin a module in a ship, creating
## a new ship, etc.

import std/[logging, paths, strutils, tables, xmlparser, xmltree]
import contracts
import game, log, maps, mobs, shipscrew2, types, utils

proc getCabinQuality*(quality: Natural): string {.raises: [], tags: [],
    contractual.} =
  ## Get the description of quality of the selected cabin in the player's ship
  ##
  ## * quality - The numerical value of the cabin's quality which will be
  ##             converted to string
  ##
  ## Returns the string with the description of the cabin's quality
  ensure:
    result.len > 0
  body:
    case quality
    of 0..10:
      return "Empty room"
    of 11..20:
      return "Minimal quality"
    of 21..30:
      return "Basic quality"
    of 31..40:
      return "Second class"
    of 41..50:
      return "Medium quality"
    of 51..60:
      return "First class"
    of 61..70:
      return "Extended quality"
    of 71..80:
      return "Encrusted room"
    of 81..90:
      return "Luxury quality"
    else:
      return "Palace room"

proc loadShipModules(shipNode: XmlNode; shipAction: DataAction;
    shipIndex: Natural; ship: var ProtoShipData) {.raises: [
    DataLoadingError], tags: [], contractual.} =
  ## Load the prototype's ship's modules from a file
  ##
  ## * shipNode   - the XML node with information about the prototype
  ## * shipAction - the action to do with the ship, like add, delete
  ## * shipIndex  - the index of the prototype of the ship
  ## * ship       - the prototype of the ship
  ##
  ## Returns the modified parameter ship.
  body:
    for module in shipNode.findAll(tag = "module"):
      let
        moduleAmount: int = try:
            module.attr(name = "amount").parseInt
          except ValueError:
            1
        moduleIndex: int = try:
            module.attr(name = "index").parseInt
          except ValueError:
          raise newException(exceptn = DataLoadingError,
            message = "Can't " & $shipAction & " ship '" & $shipIndex &
                "', invalid value for module index.")
        moduleAction: DataAction = try:
            parseEnum[DataAction](s = module.attr(
                name = "action").toLowerAscii)
          except ValueError:
            DataAction.add
      if moduleAction == DataAction.add:
        for i in 1 .. moduleAmount:
          ship.modules.add(y = moduleIndex)
      else:
        for mIndex, pModule in ship.modules:
          if pModule == moduleIndex:
            {.warning[UnsafeSetLen]: off.}
            ship.modules.delete(i = mIndex)
            {.warning[UnsafeSetLen]: on.}
            break

proc loadShipCargo(shipNode: XmlNode; shipAction: DataAction;
    shipIndex: Natural; ship: var ProtoShipData) {.raises: [
    DataLoadingError], tags: [], contractual.} =
  ## Load the prototype's ship's cargo from a file
  ##
  ## * shipNode   - the XML node with information about the prototype
  ## * shipAction - the action to do with the ship, like add, delete
  ## * shipIndex  - the index of the prototype of the ship
  ## * ship       - the prototype of the ship
  ##
  ## Returns the modified parameter ship.
  body:
    for item in shipNode.findAll(tag = "cargo"):
      let itemIndex: int = try:
            item.attr(name = "index").parseInt
          except ValueError:
          raise newException(exceptn = DataLoadingError,
            message = "Can't " & $shipAction & " ship '" & $shipIndex &
                "', invalid value for cargo item index.")
      if not itemsList.contains(key = itemIndex):
        raise newException(exceptn = DataLoadingError,
          message = "Can't " & $shipAction & " ship '" & $shipIndex &
              "', invalid value for cargo item index.")
      let
        itemAction: DataAction = try:
            parseEnum[DataAction](s = item.attr(name = "action").toLowerAscii)
          except ValueError:
            DataAction.add
        itemAmount: int = try:
            item.attr(name = "amount").parseInt
          except ValueError:
            0
      var minAmount, maxAmount: int = 0
      if itemAmount == 0:
        minAmount = try:
              item.attr(name = "minamount").parseInt
            except ValueError:
            raise newException(exceptn = DataLoadingError,
              message = "Can't " & $shipAction & " ship '" & $shipIndex &
                  "', invalid value for cargo item minamount.")
        maxAmount = try:
            item.attr(name = "maxamount").parseInt
          except ValueError:
          raise newException(exceptn = DataLoadingError,
            message = "Can't " & $shipAction & " ship '" & $shipIndex &
                "', invalid value for cargo item maxamount.")
        if minAmount > maxAmount:
          raise newException(exceptn = DataLoadingError,
            message = "Can't " & $shipAction & " ship '" & $shipIndex &
                "', invalid value for cargo item amount range.")
      case itemAction
      of DataAction.add:
        if itemAmount > 0:
          ship.cargo.add(y = MobInventoryRecord(protoIndex: itemIndex,
              minAmount: itemAmount, maxAmount: 0))
        else:
          ship.cargo.add(y = MobInventoryRecord(protoIndex: itemIndex,
              minAmount: minAmount, maxAmount: maxAmount))
      of DataAction.update:
        for cargoItem in ship.cargo.mitems:
          if cargoItem.protoIndex == itemIndex:
            if itemAmount > 0:
              cargoItem.minAmount = itemAmount
              cargoItem.maxAmount = 0
            else:
              cargoItem.minAmount = minAmount
              cargoItem.maxAmount = maxAmount
            break
      of DataAction.remove:
        var cargoIndex: Natural = 0
        while cargoIndex < ship.cargo.len:
          if ship.cargo[cargoIndex].protoIndex == itemIndex:
            ship.cargo.delete(i = cargoIndex)
            break
          cargoIndex.inc

proc loadShipCrew(shipNode: XmlNode; shipAction: DataAction;
    shipIndex: Natural; ship: var ProtoShipData) {.raises: [
    DataLoadingError], tags: [], contractual.} =
  ## Load the prototype's ship's crew from a file
  ##
  ## * shipNode   - the XML node with information about the prototype
  ## * shipAction - the action to do with the ship, like add, delete
  ## * shipIndex  - the index of the prototype of the ship
  ## * ship       - the prototype of the ship
  ##
  ## Returns the modified parameter ship.
  body:
    for member in shipNode.findAll(tag = "member"):
      let memberIndex: int = try:
            member.attr(name = "index").parseInt
          except ValueError:
            raise newException(exceptn = DataLoadingError,
                message = "Can't " & $shipAction & " ship '" & $shipIndex & "', invalid value for crew member index.")
      if not protoMobsList.contains(key = memberIndex):
        raise newException(exceptn = DataLoadingError,
          message = "Can't " & $shipAction & " ship '" & $shipIndex &
              "', invalid value for crew member index.")
      let memberAction: DataAction = try:
            parseEnum[DataAction](s = member.attr(
                name = "action").toLowerAscii)
          except ValueError:
            DataAction.add
      var memberAmount: Natural = try:
            member.attr(name = "amount").parseInt
          except ValueError:
            0
      var minAmount, maxAmount: Natural = 0
      if memberAmount == 0:
        minAmount = try:
              member.attr(name = "minamount").parseInt
            except ValueError:
              0
        maxAmount = try:
            member.attr(name = "maxamount").parseInt
          except ValueError:
            1
        if minAmount > maxAmount:
          raise newException(exceptn = DataLoadingError, message = "Can't " &
              $shipAction & " ship '" & $shipIndex & "', invalid value for crew member amount range.")
        if minAmount == 0:
          memberAmount = 1
      case memberAction
      of DataAction.add:
        if memberAmount > 0:
          ship.crew.add(y = ProtoMemberData(protoIndex: memberIndex,
              minAmount: memberAmount, maxAmount: 0))
        else:
          ship.crew.add(y = ProtoMemberData(protoIndex: memberIndex,
              minAmount: minAmount, maxAmount: maxAmount))
      of DataAction.update:
        for crewMember in ship.crew.mitems:
          if crewMember.protoIndex == memberIndex:
            if memberAmount > 0:
              crewMember.minAmount = memberAmount
              crewMember.maxAmount = 0
            else:
              crewMember.minAmount = minAmount
              crewMember.maxAmount = maxAmount
            break
      of DataAction.remove:
        var crewIndex: Natural = 0
        while crewIndex < ship.crew.len:
          if ship.crew[crewIndex].protoIndex == memberIndex:
            {.warning[UnsafeSetLen]: off.}
            ship.crew.delete(i = crewIndex)
            {.warning[UnsafeSetLen]: on.}
            break
          crewIndex.inc

proc loadShips*(fileName: Path) {.raises: [DataLoadingError],
    tags: [WriteIOEffect, ReadIOEffect, RootEffect], contractual.} =
  ## Load the ships data from the file
  ##
  ## * fileName - the name of the file to load
  require:
    ($fileName).len > 0
  body:
    let shipsXml: XmlNode = try:
        loadXml(path = $fileName)
      except XmlError, ValueError, IOError, OSError, Exception:
        raise newException(exceptn = DataLoadingError,
            message = "Can't load ships data file. Reason: " &
            getCurrentExceptionMsg())
    for shipNode in shipsXml:
      if shipNode.kind != xnElement:
        continue
      let
        shipIndex: Natural = try:
            shipNode.attr(name = "index").parseInt()
          except ValueError:
            raise newException(exceptn = DataLoadingError,
                message = "Can't add ship '" & shipNode.attr(name = "index") & "', invalid index.")
        shipAction: DataAction = try:
            parseEnum[DataAction](s = shipNode.attr(
                name = "action").toLowerAscii)
          except ValueError:
            DataAction.add
      if shipAction in [update, remove]:
        if shipIndex > protoShipsList.len:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $shipAction & " ship '" & $shipIndex & "', there is no ship with that index.")
      elif shipIndex < protoShipsList.len:
        raise newException(exceptn = DataLoadingError,
            message = "Can't add ship '" & $shipIndex & "', there is an ship with that index.")
      if shipAction == DataAction.remove:
        {.warning[ProveInit]: off.}
        {.warning[UnsafeDefault]: off.}
        protoShipsList.del(key = shipIndex)
        {.warning[ProveInit]: on.}
        {.warning[UnsafeDefault]: on.}
        logMessage(message = "Ship removed: '" & $shipIndex & "'",
            messageLevel = lvlInfo)
        continue
      var ship: ProtoShipData = if shipAction == DataAction.update:
          try:
            protoShipsList[shipIndex]
          except ValueError:
            ProtoShipData(combatValue: 1)
        else:
          ProtoShipData(combatValue: 1)
      var attribute: string = shipNode.attr(name = "name")
      if attribute.len > 0:
        ship.name = attribute
      loadShipModules(shipNode = shipNode, shipAction = shipAction,
          shipIndex = shipIndex, ship = ship)
      attribute = shipNode.attr(name = "accuracy")
      if attribute.len() > 0:
        ship.accuracy.minValue = try:
            attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $shipAction & " ship '" & $shipIndex & "', invalid value for ship bonus accuracy.")
        ship.accuracy.maxValue = 0
      attribute = shipNode.attr(name = "minaccuracy")
      if attribute.len() > 0:
        ship.accuracy.minValue = try:
            attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $shipAction & " ship '" & $shipIndex & "', invalid value for ship bonus minimum accuracy.")
        attribute = shipNode.attr(name = "maxaccuracy")
        ship.accuracy.maxValue = try:
            attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $shipAction & " ship '" & $shipIndex & "', invalid value for ship bonus maximum accuracy.")
        if ship.accuracy.maxValue < ship.accuracy.minValue:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $shipAction & " ship '" & $shipIndex & "', invalid range for ship bonus accuracy.")
      attribute = shipNode.attr(name = "combatai")
      if attribute.len() > 0:
        ship.combatAi = try:
            parseEnum[ShipCombatAi](s = attribute.toLowerAscii)
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $shipAction & " ship '" & $shipIndex & "', invalid value for ship combat AI.")
      attribute = shipNode.attr(name = "evasion")
      if attribute.len() > 0:
        ship.evasion.minValue = try:
            attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $shipAction & " ship '" & $shipIndex & "', invalid value for ship bonus evasion.")
        ship.evasion.maxValue = 0
      attribute = shipNode.attr(name = "minevasion")
      if attribute.len() > 0:
        ship.evasion.minValue = try:
            attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $shipAction & " ship '" & $shipIndex & "', invalid value for ship bonus minimum evasion.")
        attribute = shipNode.attr(name = "maxevasion")
        ship.evasion.maxValue = try:
            attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $shipAction & " ship '" & $shipIndex & "', invalid value for ship bonus maximum evasion.")
        if ship.evasion.maxValue < ship.evasion.minValue:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $shipAction & " ship '" & $shipIndex & "', invalid range for ship bonus evasion.")
      attribute = shipNode.attr(name = "loot")
      if attribute.len() > 0:
        ship.loot.minValue = try:
            attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $shipAction & " ship '" & $shipIndex & "', invalid value for ship loot.")
        ship.loot.maxValue = 0
      attribute = shipNode.attr(name = "minloot")
      if attribute.len() > 0:
        ship.loot.minValue = try:
            attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $shipAction & " ship '" & $shipIndex & "', invalid value for ship minimum loot.")
        attribute = shipNode.attr(name = "maxloot")
        ship.loot.maxValue = try:
            attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $shipAction & " ship '" & $shipIndex & "', invalid value for ship maximum loot.")
        if ship.loot.maxValue < ship.loot.minValue:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $shipAction & " ship '" & $shipIndex & "', invalid range for ship loot.")
      attribute = shipNode.attr(name = "perception")
      if attribute.len() > 0:
        ship.perception.minValue = try:
            attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $shipAction & " ship '" & $shipIndex & "', invalid value for ship bonus perception.")
        ship.perception.maxValue = 0
      attribute = shipNode.attr(name = "minperception")
      if attribute.len() > 0:
        ship.perception.minValue = try:
            attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $shipAction & " ship '" & $shipIndex & "', invalid value for ship bonus minimum perception.")
        attribute = shipNode.attr(name = "maxperception")
        ship.perception.maxValue = try:
            attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $shipAction & " ship '" & $shipIndex & "', invalid value for ship bonus maximum perception.")
        if ship.perception.maxValue < ship.perception.minValue:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $shipAction & " ship '" & $shipIndex & "', invalid range for ship bonus perception.")
      attribute = shipNode.attr(name = "reputation")
      if attribute.len() > 0:
        ship.reputation = try:
            attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $shipAction & " ship '" & $shipIndex & "', invalid value for ship reputation.")
      loadShipCargo(shipNode = shipNode, shipAction = shipAction,
          shipIndex = shipIndex, ship = ship)
      attribute = shipNode.attr(name = "owner")
      if attribute.len > 0:
        ship.owner = attribute
      for recipe in shipNode.findAll(tag = "recipe"):
        let recipeIndex: string = recipe.attr(name = "index")
        if not recipesList.contains(key = recipeIndex):
          raise newException(exceptn = DataLoadingError,
            message = "Can't " & $shipAction & " ship '" & $shipIndex &
                "', invalid value for known recipe index.")
        let
          recipeAction: DataAction = try:
              parseEnum[DataAction](s = recipe.attr(
                  name = "action").toLowerAscii)
            except ValueError:
              DataAction.add
        if recipeAction == DataAction.add:
          ship.knownRecipes.add(y = recipeIndex)
        else:
          for rIndex, knownRecipe in ship.knownRecipes:
            if knownRecipe == recipeIndex:
              ship.knownRecipes.delete(i = rIndex)
              break
      loadShipCrew(shipNode = shipNode, shipAction = shipAction,
          shipIndex = shipIndex, ship = ship)
      for description in shipNode.findAll(tag = "description"):
        ship.description = description.innerText()

      proc countAmmoValue(itemTypeIndex, multiple: Positive) {.raises: [
          KeyError], tags: [], contractual.} =
        ## Add the combat value of the selected ammunition to the ship's combat
        ## value
        ##
        ## * itemTypeIndex - the index of the ammunition's type
        ## * multiple      - the multiplier to count the ship's combat value
        for item in ship.cargo:
          if itemsList[item.protoIndex].itemType == itemsTypesList[
              itemTypeIndex - 1]:
            ship.combatValue += (itemsList[item.protoIndex].value[1] * multiple)

      for moduleIndex in ship.modules:
        try:
          let module: BaseModuleData = modulesList[moduleIndex]
          case module.mType
          of ModuleType.hull, ModuleType.gun, ModuleType.batteringRam:
            ship.combatValue = ship.combatValue + module.durability + (
                module.maxValue * 10)
            if module.mType == ModuleType.gun:
              countAmmoValue(itemTypeIndex = module.value, multiple = 10)
          of ModuleType.armor:
            ship.combatValue += module.durability
          of ModuleType.harpoonGun:
            ship.combatValue += module.durability + (
                module.maxValue * 5)
            countAmmoValue(itemTypeIndex = module.value, multiple = 5)
          else:
            discard
        except KeyError:
          raise newException(exceptn = DataLoadingError,
            message = "Can't " & $shipAction & " ship '" & $shipIndex &
                "', invalid index for module during counting the ship's combat value.")
      ship.combatValue.dec
      if shipAction == DataAction.add:
        logMessage(message = "Ship added: '" & $shipIndex & "'",
            messageLevel = lvlInfo)
      else:
        logMessage(message = "Ship updated: '" & $shipIndex & "'",
            messageLevel = lvlInfo)
      protoShipsList[shipIndex] = ship

proc damageModule*(ship: var ShipRecord; moduleIndex: Natural; damage: Positive;
    deathReason: string) {.raises: [KeyError, IOError, ReputationError], tags: [
    WriteIOEffect], contractual.} =
  ## Damage the selected module, kill its owner if the module was destroyed
  ##
  ## * ship        - the ship in which the module will be damaged
  ## * moduleIndex - the index of the module to damage
  ## * damage      - the amount of damage which the module will taken
  ## * deathReason - the reason of damage, used to inform about the module's
  ##                 owner death
  ##
  ## Returns the updated parameter ship
  require:
    moduleIndex < ship.modules.len
    deathReason.len > 0
  body:

    proc removeGun(moduleIndex2: Natural; ship: var ShipRecord) {.raises: [
        KeyError, IOError, ReputationError], tags: [WriteIOEffect], contractual.} =
      ## Remove a gun from the ship and kill a gunner in it.
      ##
      ## * moduleIndex2 - the index of the gun to remove
      ## * ship         - the ship in which the gun will be removed
      ##
      ## Returns the modified parameter ship.
      require:
        moduleIndex2 < ship.modules.len
      body:
        if ship.modules[moduleIndex2].owner[0] > -1:
          death(memberIndex = ship.modules[moduleIndex2].owner[0],
              reason = deathReason, ship = ship)

    let realDamage: Natural = if damage > ship.modules[moduleIndex].durability:
        ship.modules[moduleIndex].durability
      else:
        damage
    ship.modules[moduleIndex].durability -= realDamage
    if ship.modules[moduleIndex].durability == 0:
      case modulesList[ship.modules[moduleIndex].protoIndex].mType
      of ModuleType.hull, ModuleType.engine:
        if ship.crew == playerShip.crew:
          death(memberIndex = 0, reason = deathReason, ship = playerShip)
      of ModuleType.turret:
        let weaponIndex: int = ship.modules[moduleIndex].gunIndex
        if weaponIndex > -1:
          ship.modules[weaponIndex].durability = 0
          removeGun(moduleIndex2 = weaponIndex, ship = ship)
      of ModuleType.gun:
        removeGun(moduleIndex2 = moduleIndex, ship = ship)
      of ModuleType.cabin:
        for owner in ship.modules[moduleIndex].owner:
          if owner > -1 and ship.crew[owner].order == rest:
            death(memberIndex = owner, reason = deathReason, ship = ship)
      else:
        if ship.modules[moduleIndex].owner.len > 0:
          if ship.modules[moduleIndex].owner[0] > -1 and ship.crew[ship.modules[
              moduleIndex].owner[0]].order != rest:
            death(memberIndex = ship.modules[moduleIndex].owner[0],
                reason = deathReason, ship = ship)

proc countShipWeight*(ship: ShipRecord): Natural {.raises: [
    KeyError], tags: [], contractual.} =
  ## Count the weight of the ship, its modules and cargo
  ##
  ## * ship - the ship which weight will be counted
  ##
  ## Returns the total weight of the ship
  for module in ship.modules:
    result += module.weight
  for item in ship.cargo:
    result += (item.amount * itemsList[item.protoIndex].weight)

proc addModulesToShip(randomUpgrades: bool; protoShip: ProtoShipData;
    ship: var ShipRecord) {.raises: [KeyError], tags: [],
    contractual.} =
  ## Add modules to the currently created ship
  ##
  ## * randomUpgrades - if true, create the ship with random upgrades, otherwise
  ##                    use default values for modules. Default value is true
  ## * protoShip      - the prototype of the ship which will be created
  ## * ship           - the currently created ship
  ##
  ## Returns the updated ship parameter
  body:
    var upgradesAmount: Natural = (if randomUpgrades: getRandom(min = 0,
        max = protoShip.modules.len) else: 0)
    for moduleIndex in protoShip.modules:
      var module: BaseModuleData = modulesList[moduleIndex]
      if upgradesAmount > 0 and getRandom(min = 1, max = 100) > 50:
        var weightGain: Natural = (module.weight / module.durability).Natural
        if weightGain < 1:
          weightGain = 1
        let roll: Positive = getRandom(min = 1, max = 100)
        case roll
        of 1..50:
          let maxUpgradeValue: Positive = (module.durability.float * 1.5).Positive
          module.durability = getRandom(min = module.durability,
              max = maxUpgradeValue)
          module.weight += (weightGain * module.durability -
              modulesList[moduleIndex].durability)
        of 51..75:
          if modulesList[moduleIndex].mType == ModuleType.engine:
            weightGain *= 10
            let maxUpgradeValue: Positive = (module.value.float / 2.0).Positive
            module.value = getRandom(min = maxUpgradeValue, max = modulesList[
                moduleIndex].value)
            module.weight += (weightGain * modulesList[
                moduleIndex].value - module.value)
        of 76..100:
          case modulesList[moduleIndex].mType
          of ModuleType.hull:
            weightGain *= 10
          of ModuleType.engine:
            weightGain = 1
          else:
            discard
          if module.mType in {ModuleType.engine, ModuleType.cabin,
              ModuleType.gun, ModuleType.batteringRam, ModuleType.hull,
                  ModuleType.harpoonGun}:
            let maxUpgradeValue: Positive = (module.maxValue.float * 1.5).Positive
            module.maxValue = getRandom(min = module.maxValue,
                max = maxUpgradeValue)
            module.weight += (weightGain * module.maxValue -
                modulesList[moduleIndex].maxValue)
        else:
          discard
        upgradesAmount.dec
      var owners: seq[int] = @[]
      if module.maxOwners > 0:
        for i in 1 .. module.maxOwners:
          owners.add(y = -1)
      case module.mType
      of ModuleType.engine:
        ship.modules.add(y = ModuleData(mType: ModuleType2.engine,
            name: module.name, protoIndex: moduleIndex, weight: module.weight,
            durability: module.durability, maxDurability: module.durability,
            owner: owners, upgradeProgress: 0, upgradeAction: ShipUpgrade.none,
            fuelUsage: module.value, power: module.maxValue, disabled: false))
      of ModuleType.cabin:
        ship.modules.add(y = ModuleData(mType: ModuleType2.cabin,
            name: module.name, protoIndex: moduleIndex, weight: module.weight,
            durability: module.durability, maxDurability: module.durability,
            owner: owners, upgradeProgress: 0, upgradeAction: ShipUpgrade.none,
            cleanliness: module.value, quality: module.value))
      of ModuleType.alchemyLab .. ModuleType.greenhouse:
        ship.modules.add(y = ModuleData(mType: ModuleType2.workshop,
            name: module.name, protoIndex: moduleIndex, weight: module.weight,
            durability: module.durability, maxDurability: module.durability,
            owner: owners, upgradeProgress: 0, upgradeAction: ShipUpgrade.none,
            craftingTime: 0, craftingAmount: 0))
      of ModuleType.medicalRoom:
        ship.modules.add(y = ModuleData(mType: ModuleType2.medicalRoom,
            name: module.name, protoIndex: moduleIndex, weight: module.weight,
            durability: module.durability, maxDurability: module.durability,
            owner: owners, upgradeProgress: 0, upgradeAction: ShipUpgrade.none))
      of ModuleType.cockpit:
        ship.modules.add(y = ModuleData(mType: ModuleType2.cockpit,
            name: module.name, protoIndex: moduleIndex, weight: module.weight,
            durability: module.durability, maxDurability: module.durability,
            owner: owners, upgradeProgress: 0, upgradeAction: ShipUpgrade.none))
      of ModuleType.trainingRoom:
        ship.modules.add(y = ModuleData(mType: ModuleType2.trainingRoom,
            name: module.name, protoIndex: moduleIndex, weight: module.weight,
            durability: module.durability, maxDurability: module.durability,
            owner: owners, upgradeProgress: 0, upgradeAction: ShipUpgrade.none,
            trainedSkill: 0))
      of ModuleType.turret:
        ship.modules.add(y = ModuleData(mType: ModuleType2.turret,
            name: module.name, protoIndex: moduleIndex, weight: module.weight,
            durability: module.durability, maxDurability: module.durability,
            owner: owners, upgradeProgress: 0, upgradeAction: ShipUpgrade.none,
            gunIndex: -1))
      of ModuleType.gun:
        ship.modules.add(y = ModuleData(mType: ModuleType2.gun,
            name: module.name, protoIndex: moduleIndex, weight: module.weight,
            durability: module.durability, maxDurability: module.durability,
            owner: owners, upgradeProgress: 0, upgradeAction: ShipUpgrade.none,
            damage: module.maxValue, ammoIndex: -1))
      of ModuleType.cargo:
        ship.modules.add(y = ModuleData(mType: ModuleType2.cargoRoom,
            name: module.name, protoIndex: moduleIndex, weight: module.weight,
            durability: module.durability, maxDurability: module.durability,
            owner: owners, upgradeProgress: 0, upgradeAction: ShipUpgrade.none))
      of ModuleType.hull:
        ship.modules.add(y = ModuleData(mType: ModuleType2.hull,
            name: module.name, protoIndex: moduleIndex, weight: module.weight,
            durability: module.durability, maxDurability: module.durability,
            owner: owners, upgradeProgress: 0, upgradeAction: ShipUpgrade.none,
            installedModules: module.value, maxModules: module.maxValue))
      of ModuleType.armor:
        ship.modules.add(y = ModuleData(mType: ModuleType2.armor,
            name: module.name, protoIndex: moduleIndex, weight: module.weight,
            durability: module.durability, maxDurability: module.durability,
            owner: owners, upgradeProgress: 0, upgradeAction: ShipUpgrade.none))
      of ModuleType.batteringRam:
        ship.modules.add(y = ModuleData(mType: ModuleType2.batteringRam,
            name: module.name, protoIndex: moduleIndex, weight: module.weight,
            durability: module.durability, maxDurability: module.durability,
            owner: owners, upgradeProgress: 0, upgradeAction: ShipUpgrade.none,
            damage2: module.maxValue, coolingDown: false))
      of ModuleType.harpoonGun:
        ship.modules.add(y = ModuleData(mType: ModuleType2.harpoonGun,
            name: module.name, protoIndex: moduleIndex, weight: module.weight,
            durability: module.durability, maxDurability: module.durability,
            owner: owners, upgradeProgress: 0, upgradeAction: ShipUpgrade.none,
            duration: module.maxValue, harpoonIndex: -1))
      of ModuleType.any:
        discard

proc createShip*(protoIndex: Positive; name: string; x: MapXRange; y: MapYRange;
    speed: ShipSpeed; randomUpgrades: bool = true): ShipRecord {.raises: [
    KeyError], tags: [], contractual.} =
  ## Create a new ship from the selected prototype
  ##
  ## * protoIndex     - the index of the ships' prototype used as base for the new
  ##                    ship
  ## * name           - the name of the ship. if empty, use the name of the prototype
  ## * x              - the X position on the map where the ship will be created
  ## * y              - the Y position on the map whene thw ship will be created
  ## * speed          - the speed level with which the ship will be created
  ## * randomUpgrades - if true, create the ship with random upgrades, otherwise
  ##                    use default values for modules. Default value is true
  ##
  ## Returns the newly created ship with the selected parameters
  require:
    protoShipsList.contains(key = protoIndex)
  body:
    let protoShip: ProtoShipData = protoShipsList[protoIndex]
    result = ShipRecord(skyX: x, skyY: y, name: (if name.len ==
      0: protoShip.name else: name), upgradeModule: -1, repairModule: -1, speed: speed)
    # Add modules to ship
    addModulesToShip(randomUpgrades = randomUpgrades, protoShip = protoShip, ship = result)
    # Set the ship crew
    for protoMember in protoShip.crew:
      let amount: Natural = (if protoMember.maxAmount ==
          0: protoMember.minAmount else: getRandom(min = protoMember.minAmount,
          max = protoMember.maxAmount))
      for i in 1..amount:
        let member: MemberData = generateMob(mobIndex = protoMember.protoIndex,
            factionIndex = protoShip.owner)
        result.crew.add(y = member)
        block setCabin:
          for module in result.modules.mitems:
            if module.mType == ModuleType2.cabin:
              for index, owner in module.owner.mpairs:
                if owner == -1:
                  owner = result.crew.len - 1
                  if index == 0:
                    module.name = member.name & "'s Cabin"
                  break setCabin
        for module in result.modules.mitems:
          if module.owner.len > 0:
            if module.owner[0] == -1 and module.mType in {ModuleType2.gun,
                ModuleType2.harpoonGun} and member.order == gunner:
              module.owner[0] = result.crew.len - 1
              break
            elif module.mType == ModuleType2.cockpit and member.order == pilot:
              module.owner[0] = result.crew.len - 1
    # Set ship cargo
    for item in protoShip.cargo:
      let amount: Natural = (if item.maxAmount > 0: getRandom(
          min = item.minAmount, max = item.maxAmount) else: item.minAmount)
      result.cargo.add(y = InventoryData(protoIndex: item.protoIndex,
          amount: amount, name: "", durability: 100, price: 0))
    var
      gunAssigned: bool = false
      amount: Natural = 0
      hullIndex: int = -1
    for index, module in result.modules.mpairs:
      if module.mType == ModuleType2.turret:
        for index2, module2 in result.modules:
          if module2.mType in {ModuleType2.gun, ModuleType2.harpoonGun}:
            gunAssigned = false
            for module3 in result.modules:
              if module2.mType == ModuleType2.turret and module3.gunIndex == index2:
                gunAssigned = true
                break
            if not gunAssigned:
              module.gunIndex = index2
      elif module.mType == ModuleType2.hull:
        hullIndex = index
      if modulesList[module.protoIndex].mType notin {ModuleType.gun,
          ModuleType.harpoonGun, ModuleType.armor, ModuleType.hull}:
        amount += modulesList[module.protoIndex].size
    result.modules[hullIndex].installedModules = amount
    # Set known crafting recipes
    for recipe in protoShip.knownRecipes:
      knownRecipes.add(y = recipe)
    # Set home base for ship
    if skyMap[x][y].baseIndex > 0:
      result.homeBase = skyMap[x][y].baseIndex
    else:
      var startX, startY, endX, endY: int
      startX = x - 100
      normalizeCoord(coord = startX)
      startY = y - 100
      normalizeCoord(coord = startY, isXAxis = false)
      endX = x + 100
      normalizeCoord(coord = endX)
      endY = y + 100
      normalizeCoord(coord = endY, isXAxis = false)
      block basesLoop:
        for skyX in startX .. endX:
          for skyY in startY .. endY:
            if skyMap[skyX][skyY].baseIndex > 0:
              if skyBases[skyMap[skyX][skyY].baseIndex].owner ==
                  protoShip.owner:
                result.homeBase = skyMap[skyX][skyY].baseIndex
                break basesLoop
      if result.homeBase == 0:
        for index, base in skyBases:
          if base.owner == protoShip.owner:
            result.homeBase = index
            break
        if result.homeBase == 0:
          result.homeBase = getRandom(min = BasesRange.low,
              max = BasesRange.high)
    # Set home base for crew members
    for member in result.crew.mitems:
      member.homeBase = (if getRandom(min = 1, max = 100) <
          99: result.homeBase else: getRandom(min = BasesRange.low,
          max = BasesRange.high))
