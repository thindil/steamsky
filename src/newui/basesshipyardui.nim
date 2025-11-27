# Copyright 2025 Bartek thindil Jasicki
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
# along with Steam Sky.  if, see <http://www.gnu.org/licenses/>.

## Provides code related to installing or removing modules from the player's
## ship, like showing the lists of modules, buying or selling them, etc.

import std/[algorithm, strutils, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[bases, basesship2, config, game, shipmodules, shipscrew, types]
import coreui, dialogs, errordialog, header, messagesui, setui, table, themes

type LocalModuleData = object
  name: string
  mType: string
  size: Natural
  material: string
  price: Positive = 1
  id: Natural

proc sortModules(x, y: LocalModuleData): int {.raises: [], tags: [],
    contractual.} =
  ## Compare two modules and return which should go first, based on the sort
  ## order of the modules
  ##
  ## * x - the first module to compare
  ## * y - the second module to compare
  ##
  ## Returns 1 if the first module should go first, -1 if the second module
  ## should go first.
  case modulesSortOrder
  of nameAsc:
    if x.name < y.name:
      return 1
    return -1
  of nameDesc:
    if x.name > y.name:
      return 1
    return -1
  of typeAsc:
    if x.mType < y.mType:
      return 1
    return -1
  of typeDesc:
    if x.mType > y.mType:
      return 1
    return -1
  of sizeAsc:
    if x.size < y.size:
      return 1
    return -1
  of sizeDesc:
    if x.size > y.size:
      return 1
    return -1
  of materialAsc:
    if x.material < y.material:
      return 1
    return -1
  of materialDesc:
    if x.material > y.material:
      return 1
    return -1
  of priceAsc:
    if x.price < y.price:
      return 1
    return -1
  of priceDesc:
    if x.price > y.price:
      return 1
    return -1
  of none:
    return -1

proc sortModules(sortAsc, sortDesc: ModulesSortOrders;
    dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Sort items on the trades list
  ##
  ## * sortAsc  - the sorting value for ascending sort
  ## * sortDesc - the sorting value for descending sort
  ## * dialog   - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  if modulesSortOrder == sortAsc:
    modulesSortOrder = sortDesc
  else:
    modulesSortOrder = sortAsc
  var localModules: seq[LocalModuleData] = @[]
  if currentTab == 0:
    for index, module in modulesList:
      if index notin modulesIndexes:
        continue
      var cost: Natural = module.price
      try:
        countPrice(price = cost, traderIndex = findMember(order = talk))
      except:
        dialog = setError(message = "Can't count install cost.")
      if cost == 0:
        cost = 1
      try:
        localModules.add(y = LocalModuleData(name: module.name,
            mType: getModuleType(moduleIndex = index), size: (if module.mType ==
            ModuleType.hull: module.maxValue else: module.size),
            material: module.repairMaterial, price: cost, id: index))
      except:
        dialog = setError(message = "Can't add module to install.")
  else:
    for index, module in playerShip.modules:
      let damage: float = 1.0 - (module.durability.float /
          module.maxDurability.float)
      var cost: Natural = try:
          modulesList[module.protoIndex].price - (modulesList[
              module.protoIndex].price.float * damage).Natural
        except:
          dialog = setError(message = "Can't set price for module.")
          return
      if cost == 0:
        cost = 1
      try:
        countPrice(price = cost, traderIndex = findMember(order = talk),
            reduce = false)
      except:
        dialog = setError(message = "Can't count price for module.")
      try:
        localModules.add(y = LocalModuleData(name: module.name,
            mType: getModuleType(moduleIndex = module.protoIndex),
            size: modulesList[module.protoIndex].size, material: modulesList[
            module.protoIndex].repairMaterial, price: cost, id: index))
      except:
        dialog = setError(message = "Can't add module to remove.")
  localModules.sort(cmp = sortModules)
  modulesIndexes = @[]
  for module in localModules:
    modulesIndexes.add(y = module.id)

var
  moduleIndex: int = -1
  compareList: seq[string] = @[]
  compareIndex: Natural = 0

proc setInstallInfo(data: int; dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the selected module information
  ##
  ## * data   - the index of the selected item
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog.
  moduleIndex = data
  compareList = @[]
  compareIndex = 0
  try:
    for module in playerShip.modules:
      if modulesList[module.protoIndex].mType == modulesList[moduleIndex].mType:
        compareList.add(y = module.name)
  except:
    dialog = setError(message = "Can't set module iterator.")
    return
  dialog = moduleDialog

proc setHullInfo(dialog: var GameDialog; installing: bool; shipModuleIndex: int;
    value, maxValue: Natural) {.raises: [], tags: [WriteIOEffect, TimeEffect,
    RootEffect], contractual.} =
  ## Show information about the selected hull module
  ##
  ## * dialog          - the current in-game dialog displayed on the screen
  ## * installing      - If true, player looking at installing modules list
  ## * shipModuleIndex - The index of the module in the player's ship to show
  ## * value           - The max size of the modules which can be installed
  ## * maxValue        - The amount of modules which can be installed
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  if installing:
    setLayoutRowDynamic(height = 30, cols = 1)
    colorLabel(str = "Ship hull can be only replaced.", color = theme.colors[goldenColor])
    setLayoutRowDynamic(height = 30, cols = 2)
    label(str = "Modules space:")
    if maxValue < playerShip.modules[shipModuleIndex].maxModules:
      colorLabel(str = $maxValue & " (smaller)", color = theme.colors[redColor])
    elif maxValue > playerShip.modules[shipModuleIndex].maxModules:
      colorLabel(str = $maxValue & " (bigger)", color = theme.colors[greenColor])
    else:
      colorLabel(str = $maxValue, color = theme.colors[goldenColor])
    label(str = "Max module size:")
    try:
      if value < modulesList[playerShip.modules[
          shipModuleIndex].protoIndex].value:
        colorLabel(str = $value & " (smaller)", color = theme.colors[redColor])
      elif value > modulesList[playerShip.modules[
          shipModuleIndex].protoIndex].value:
        colorLabel(str = $value & " (bigger)", color = theme.colors[greenColor])
      else:
        colorLabel(str = $value, color = theme.colors[goldenColor])
    except:
      dialog = setError(message = "Can't show module size")
      return

proc setEngineInfo(installing: bool; shipModuleIndex: int; value,
    maxValue: Natural) {.raises: [], tags: [], contractual.} =
  ## Show information about the selected engine module
  ##
  ## * installing      - If true, player looking at installing modules list
  ## * shipModuleIndex - The index of the module in the player's ship to show
  ## * value           - The power of the engime
  ## * maxValue        - The fuel usage of the engine
  label(str = "Max power:")
  if installing and shipModuleIndex > -1:
    if maxValue < playerShip.modules[shipModuleIndex].power:
      colorLabel(str = $maxValue & " (weaker)", color = theme.colors[redColor])
    elif maxValue > playerShip.modules[shipModuleIndex].power:
      colorLabel(str = $maxValue & " (stronger)", color = theme.colors[greenColor])
    else:
      colorLabel(str = $maxValue, color = theme.colors[goldenColor])
    label(str = "Fuel usage:")
    if value < playerShip.modules[shipModuleIndex].fuelUsage:
      colorLabel(str = $value & " (less)", color = theme.colors[greenColor])
    elif value > playerShip.modules[shipModuleIndex].fuelUsage:
      colorLabel(str = $value & " (more)", color = theme.colors[redColor])
    else:
      colorLabel(str = $value, color = theme.colors[goldenColor])
  else:
    colorLabel(str = $maxValue, color = theme.colors[goldenColor])
    label(str = "Fuel usage:")
    colorLabel(str = $value, color = theme.colors[goldenColor])

proc setCargoInfo(dialog: var GameDialog; installing: bool;
  shipModuleIndex: int; maxValue: Natural) {.raises: [],
  tags: [WriteIOEffect, TimeEffect, RootEffect], contractual.} =
  ## Show information about the selected cargo module
  ##
  ## * dialog          - the current in-game dialog displayed on the screen
  ## * installing      - If true, player looking at installing modules list
  ## * shipModuleIndex - The index of the module in the player's ship to show
  ## * maxValue        - The max capacity of the cargo
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  label(str = "Max cargo:")
  if installing and shipModuleIndex > -1:
    try:
      if maxValue > modulesList[playerShip.modules[
          shipModuleIndex].protoIndex].maxValue:
        colorLabel(str = $maxValue & " (bigger)", color = theme.colors[greenColor])
      elif maxValue < modulesList[playerShip.modules[
          shipModuleIndex].protoIndex].maxValue:
        colorLabel(str = $maxValue & " (smaller)", color = theme.colors[redColor])
      else:
        colorLabel(str = $maxValue, color = theme.colors[goldenColor])
    except:
      dialog = setError(message = "Can't show module weight")
      return
  else:
    colorLabel(str = $maxValue, color = theme.colors[goldenColor])

proc setCabinInfo(dialog: var GameDialog; installing: bool;
    shipModuleIndex: int; maxValue, maxOwners: Natural) {.raises: [], tags: [
    WriteIOEffect, TimeEffect, RootEffect], contractual.} =
  ## Set the information about the selected cabin to install or remove
  ##
  ## * dialog          - the current in-game dialog displayed on the screen
  ## * installing      - if true, show installation information, otherwise show
  ##                     removing info.
  ## * shipModuleIndex - The index of the module in the player's ship to show
  ## * maxValue        - The quality of the cabin
  ## * maxOwners       - The maximum amount of owners of the cabin
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  label(str = "Quality:")
  if installing and shipModuleIndex > -1:
    case maxValue
    of 0..29:
      if playerShip.modules[shipModuleIndex].quality > maxValue:
        colorLabel(str = "minimal (worse)", color = theme.colors[redColor])
      elif playerShip.modules[shipModuleIndex].quality < maxValue:
        colorLabel(str = "minimal (better)", color = theme.colors[greenColor])
      else:
        colorLabel(str = "minimal", color = theme.colors[goldenColor])
    of 30..59:
      if playerShip.modules[shipModuleIndex].quality > maxValue:
        colorLabel(str = "basic (worse)", color = theme.colors[redColor])
      elif playerShip.modules[shipModuleIndex].quality < maxValue:
        colorLabel(str = "basic (better)", color = theme.colors[greenColor])
      else:
        colorLabel(str = "basic", color = theme.colors[goldenColor])
    of 60..79:
      if playerShip.modules[shipModuleIndex].quality > maxValue:
        colorLabel(str = "extended (worse)", color = theme.colors[redColor])
      elif playerShip.modules[shipModuleIndex].quality < maxValue:
        colorLabel(str = "extended (better)", color = theme.colors[greenColor])
      else:
        colorLabel(str = "extended", color = theme.colors[goldenColor])
    else:
      if playerShip.modules[shipModuleIndex].quality > maxValue:
        colorLabel(str = "luxury (worse)", color = theme.colors[redColor])
      elif playerShip.modules[shipModuleIndex].quality < maxValue:
        colorLabel(str = "luxury (better)", color = theme.colors[greenColor])
      else:
        colorLabel(str = "luxury", color = theme.colors[goldenColor])
  else:
    case maxValue
    of 0..29:
      colorLabel(str = "minimal", color = theme.colors[goldenColor])
    of 30..59:
      colorLabel(str = "basic", color = theme.colors[goldenColor])
    of 60..79:
      colorLabel(str = "extended", color = theme.colors[goldenColor])
    else:
      colorLabel(str = "luxury", color = theme.colors[goldenColor])
  label(str = "Max owners:")
  if installing and shipModuleIndex > -1:
    try:
      if modulesList[playerShip.modules[
          shipModuleIndex].protoIndex].maxOwners > maxOwners:
        colorLabel(str = $maxOwners & " (less)", color = theme.colors[redColor])
      elif modulesList[playerShip.modules[
          shipModuleIndex].protoIndex].maxOwners < maxOwners:
        colorLabel(str = $maxOwners & " (more)", color = theme.colors[greenColor])
      else:
        colorLabel(str = $maxOwners, color = theme.colors[goldenColor])
    except:
      dialog = setError(message = "Can't show module owners")
      return
  else:
    colorLabel(str = $maxOwners, color = theme.colors[goldenColor])

proc setWorkshopInfo(dialog: var GameDialog; installing: bool;
  shipModuleIndex: int; maxOwners: Natural) {.raises: [],
  tags: [WriteIOEffect, TimeEffect, RootEffect], contractual.} =
  ## Show information about the selected workshop module
  ##
  ## * dialog          - the current in-game dialog displayed on the screen
  ## * installing      - If true, player looking at installing modules list
  ## * shipModuleIndex - The index of the module in the player's ship to show
  ## * maxOwners       - The maximum amount of workers in the workshop
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  label(str = "Max workers:")
  if installing and shipModuleIndex > -1:
    try:
      if modulesList[playerShip.modules[
          shipModuleIndex].protoIndex].maxOwners > maxOwners:
        colorLabel(str = $maxOwners & " (less)", color = theme.colors[redColor])
      elif modulesList[playerShip.modules[
          shipModuleIndex].protoIndex].maxOwners < maxOwners:
        colorLabel(str = $maxOwners & " (more)", color = theme.colors[greenColor])
      else:
        colorLabel(str = $maxOwners, color = theme.colors[goldenColor])
    except:
      dialog = setError(message = "Can't show module workers")
      return
  else:
    colorLabel(str = $maxOwners, color = theme.colors[goldenColor])

proc setGunInfo(dialog: var GameDialog; installing: bool;
  shipModuleIndex: int; speed: int; value, maxValue: Natural;
  mType: ModuleType) {.raises: [],
  tags: [WriteIOEffect, TimeEffect, RootEffect], contractual.} =
  ## Show information about the selected gun or harpoon gun module
  ##
  ## * dialog          - the current in-game dialog displayed on the screen
  ## * installing      - If true, player looking at installing modules list
  ## * shipModuleIndex - The index of the module in the player's ship to show
  ## * speed           - The shooting speed of the gun
  ## * value           - The type of ammunition used
  ## * maxValue        - The damage done by the gun
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  label(str = "Strength:")
  if installing and shipModuleIndex > -1:
    if mType == ModuleType.gun:
      if playerShip.modules[shipModuleIndex].damage > maxValue:
        colorLabel(str = $maxValue & " (weaker)", color = theme.colors[redColor])
      elif playerShip.modules[shipModuleIndex].damage < maxValue:
        colorLabel(str = $maxValue & " (stronger)", color = theme.colors[greenColor])
      else:
        colorLabel(str = $maxValue, color = theme.colors[goldenColor])
    else:
      if playerShip.modules[shipModuleIndex].duration > maxValue:
        colorLabel(str = $maxValue & " (weaker)", color = theme.colors[redColor])
      elif playerShip.modules[shipModuleIndex].duration < maxValue:
        colorLabel(str = $maxValue & " (stronger)", color = theme.colors[greenColor])
      else:
        colorLabel(str = $maxValue, color = theme.colors[goldenColor])
  else:
    colorLabel(str = $maxValue, color = theme.colors[goldenColor])
  label(str = "Ammunition:")
  for item in itemsList.values:
    if item.itemType == itemsTypesList[value - 1]:
      colorLabel(str = "Any" & item.name[item.name.find(sub = ' ')..^1],
          color = theme.colors[goldenColor])
      break
  if mType == ModuleType.gun:
    label(str = "Max fire rate:")
    if installing and shipModuleIndex > -1:
      try:
        if modulesList[playerShip.modules[shipModuleIndex].protoIndex].speed > speed:
          if speed > 0:
            colorLabel(str = $speed & "/round (slower)", color = theme.colors[redColor])
          else:
            colorLabel(str = $(speed.abs) & " rounds (slower)",
                color = theme.colors[redColor])
        elif modulesList[playerShip.modules[
            shipModuleIndex].protoIndex].speed < speed:
          if speed > 0:
            colorLabel(str = $speed & "/round (faster)", color = theme.colors[greenColor])
          else:
            colorLabel(str = $(speed.abs) & " rounds (faster)",
                color = theme.colors[greenColor])
        else:
          if speed > 0:
            colorLabel(str = $speed & "/round", color = theme.colors[goldenColor])
          else:
            colorLabel(str = $(speed.abs) & " rounds", color = theme.colors[goldenColor])
      except:
        dialog = setError(message = "Can't show fire rate")
        return
    else:
      if speed > 0:
        colorLabel(str = $speed & "/round", color = theme.colors[goldenColor])
      else:
        colorLabel(str = $(speed.abs) & " rounds", color = theme.colors[goldenColor])

proc setBatteringRamInfo(installing: bool; shipModuleIndex: int;
    maxValue: Natural) {.raises: [], tags: [], contractual.} =
  ## Show information about the selected battering ram module
  ##
  ## * installing      - If true, player looking at installing modules list
  ## * row             - The current row in the dialog
  ## * newInfo         - If true, create the new UI for the info, otherwise reuse
  ##                     old one. Default value is true.
  ## * shipModuleIndex - The index of the module in the player's ship to show
  ## * maxValue        - The damage done by the battering ram
  label(str = "Strength:")
  if installing and shipModuleIndex > -1:
    if playerShip.modules[shipModuleIndex].damage2 > maxValue:
      colorLabel(str = $maxValue & " (weaker)", color = theme.colors[redColor])
    elif playerShip.modules[shipModuleIndex].damage2 < maxValue:
      colorLabel(str = $maxValue & " (stronger)", color = theme.colors[greenColor])
    else:
      colorLabel(str = $maxValue, color = theme.colors[goldenColor])
  else:
    colorLabel(str = $maxValue, color = theme.colors[goldenColor])

proc setModuleInfo(dialog: var GameDialog; installing: bool) {.raises: [],
    tags: [WriteIOEffect, TimeEffect, RootEffect], contractual.} =
  ## Set the information about the selected module to install or remove
  ##
  ## * dialog     - the current in-game dialog displayed on the screen
  ## * installing - if true, show installation information, otherwise show
  ##                removing info.
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  var
    mType: ModuleType = ModuleType.any
    shipModuleIndex, speed: int = -1
    maxValue, maxOwners, weight, value: Natural = 0
    size: Positive = 1
  if installing:
    mType = try:
        modulesList[moduleIndex].mType
      except:
        dialog = setError(message = "Can't get protomodule type")
        return
    maxValue = try:
        modulesList[moduleIndex].maxValue
      except:
        dialog = setError(message = "Can't get protomodule max value")
        return
    value = try:
        modulesList[moduleIndex].value
      except:
        dialog = setError(message = "Can't get protomodule value")
        return
    size = try:
        modulesList[moduleIndex].size
      except:
        dialog = setError(message = "Can't get protomodule size")
        return
    weight = try:
        modulesList[moduleIndex].weight
      except:
        dialog = setError(message = "Can't get protomodule weight")
        return
    maxOwners = try:
        modulesList[moduleIndex].maxOwners
      except:
        dialog = setError(message = "Can't get protomodule max owners")
        return
    speed = try:
        modulesList[moduleIndex].speed
      except:
        dialog = setError(message = "Can't get protomodule speed")
        return
    var moduleIterator: Natural = compareIndex + 1
    for index, module in playerShip.modules:
      try:
        if modulesList[module.protoIndex].mType == mType:
          moduleIterator.dec
          if moduleIterator == 0:
            shipModuleIndex = index
            break
      except:
        dialog = setError(message = "Can't get ship module index")
        return
  else:
    mType = try:
        modulesList[playerShip.modules[moduleIndex].protoIndex].mType
      except:
        dialog = setError(message = "Can't get module type")
        return
    case mType
    of harpoonGun:
      maxValue = playerShip.modules[moduleIndex].duration
      value = try:
          modulesList[playerShip.modules[moduleIndex].protoIndex].value
      except:
        dialog = setError(message = "Can't get module value")
        return
    of engine:
      maxValue = playerShip.modules[moduleIndex].power
      value = playerShip.modules[moduleIndex].fuelUsage
    of cabin:
      maxValue = playerShip.modules[moduleIndex].quality
      value = playerShip.modules[moduleIndex].cleanliness
    of gun:
      maxValue = playerShip.modules[moduleIndex].damage
      value = try:
          modulesList[playerShip.modules[moduleIndex].protoIndex].value
        except:
          dialog = setError(message = "Can't get module value2")
          return
    of cargo:
      maxValue = try:
          modulesList[playerShip.modules[moduleIndex].protoIndex].maxValue
        except:
          dialog = setError(message = "Can't get module max value")
          return
      value = try:
          modulesList[playerShip.modules[moduleIndex].protoIndex].value
        except:
          dialog = setError(message = "Can't get module value3")
          return
    of hull:
      maxValue = playerShip.modules[moduleIndex].maxModules
      value = try:
          modulesList[playerShip.modules[moduleIndex].protoIndex].value
        except:
          dialog = setError(message = "Can't get module value4")
          return
    of batteringRam:
      maxValue = playerShip.modules[moduleIndex].damage2
      value = 0
    else:
      maxValue = 0
      value = 0
    size = try:
        modulesList[playerShip.modules[moduleIndex].protoIndex].size
      except:
        dialog = setError(message = "Can't get module size")
        return
    weight = try:
        modulesList[playerShip.modules[moduleIndex].protoIndex].weight
      except:
        dialog = setError(message = "Can't get module weight")
        return
    maxOwners = try:
        modulesList[playerShip.modules[moduleIndex].protoIndex].maxOwners
      except:
        dialog = setError(message = "Can't get module max owners")
        return
    speed = try:
        modulesList[playerShip.modules[moduleIndex].protoIndex].speed
      except:
        dialog = setError(message = "Can't get module size")
        return
  case mType
  of hull:
    setHullInfo(dialog = dialog, installing = installing,
        shipModuleIndex = shipModuleIndex, value = value, maxValue = maxValue)
  of engine:
    setEngineInfo(installing = installing, shipModuleIndex = shipModuleIndex,
      value = value, maxValue = maxValue)
  of cargo:
    setCargoInfo(dialog = dialog, installing = installing,
      shipModuleIndex = shipModuleIndex, maxValue = maxValue)
  of cabin:
    setCabinInfo(dialog = dialog, installing = installing,
        shipModuleIndex = shipModuleIndex, maxValue = maxValue,
        maxOwners = maxOwners)
  of alchemyLab..greenhouse:
    setWorkshopInfo(dialog = dialog, installing = installing,
      shipModuleIndex = shipModuleIndex, maxOwners = maxOwners)
  of gun, harpoonGun:
    setGunInfo(dialog = dialog, installing = installing,
      shipModuleIndex = shipModuleIndex, speed = speed, value = value,
      maxValue = maxValue, mType = mType)
  of batteringRam:
    setBatteringRamInfo(installing = installing,
      shipModuleIndex = shipModuleIndex, maxValue = maxValue)
  else:
    discard
  if mType notin [ModuleType.hull, armor]:
    label(str = "Size:")
    var added: bool = false
    if installing:
      for module in playerShip.modules:
        try:
          if module.mType == ModuleType2.hull and size > modulesList[
              module.protoIndex].value:
            colorLabel(str = $size & " (need a bigger hull)",
                color = theme.colors[redColor])
            added = true
            break
        except:
          dialog = setError(message = "Can't show module's size")
          return
    if not added:
      colorLabel(str = $size, color = theme.colors[goldenColor])
  if weight > 0:
    label(str = "Weight:")
    if shipModuleIndex > -1:
      if weight > playerShip.modules[shipModuleIndex].weight:
        colorLabel(str = $weight & " kg (heavier)", color = theme.colors[goldenColor])
      elif weight < playerShip.modules[shipModuleIndex].weight:
        colorLabel(str = $weight & " kg (lighter)", color = theme.colors[goldenColor])
      else:
        colorLabel(str = $weight & " kg", color = theme.colors[goldenColor])
    else:
      colorLabel(str = $weight & " kg", color = theme.colors[goldenColor])
  if installing and shipModuleIndex > -1:
    label(str = "Durability:")
    if shipModuleIndex > -1:
      try:
        if playerShip.modules[shipModuleIndex].maxDurability > modulesList[
            moduleIndex].durability:
          colorLabel(str = "weaker", color = theme.colors[redColor])
        elif playerShip.modules[shipModuleIndex].maxDurability < modulesList[
            moduleIndex].durability:
          colorLabel(str = "stronger", color = theme.colors[greenColor])
        else:
          colorLabel(str = "same", color = theme.colors[goldenColor])
      except:
        dialog = setError(message = "Can't show module durability")
        return
    else:
      colorLabel(str = "same", color = theme.colors[goldenColor])
  if installing:
    label(str = "Repair/Upgrade material:")
    var
      mAmount: Natural = 0
      infoText: string = ""
    for item in itemsList.values:
      try:
        if item.itemType == modulesList[moduleIndex].repairMaterial:
          if mAmount > 0:
            infoText.add(y = " or ")
          infoText.add(y = item.name)
          mAmount.inc
      except:
        dialog = setError(message = "Can't show repair material")
        return
    colorLabel(str = infoText, color = theme.colors[goldenColor])
    label(str = "Repair/Upgrade skill:")
    try:
      colorLabel(str = skillsList[modulesList[moduleIndex].repairSkill].name &
          "/" & attributesList[skillsList[modulesList[
          moduleIndex].repairSkill].attribute].name, color = theme.colors[goldenColor])
    except:
      dialog = setError(message = "Can't show repair skill")
      return
    setLayoutRowDynamic(height = 30, cols = 1)
    try:
      if modulesList[moduleIndex].unique:
        colorLabel(str = " The module is unique. Only one module of that type can be installed on the ship.",
            color = theme.colors[goldenColor])
    except:
      dialog = setError(message = "Can't show module unique info")
      return
    try:
      if modulesList[moduleIndex].description.len > 0:
        label(str = "")
        setLayoutRowDynamic(height = 90, cols = 1)
        wrapLabel(str = modulesList[moduleIndex].description)
    except:
      dialog = setError(message = "Can't show module's description")
      return

proc manipulateModule(dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Install or remove the selected module
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  try:
    if currentTab == 0:
      upgradeShip(install = true, moduleIndex = moduleIndex)
    else:
      upgradeShip(install = false, moduleIndex = moduleIndex)
    setShipyard(dialog = dialog)
    dialog = none
  except NoMoneyError:
    dialog = setMessage(message = "You don't have " & moneyName & " to pay for modules.",
        title = "Can't install module.")
  except NotEnoughMoneyError:
    dialog = setMessage(message = "You don't have enough " & moneyName & " to pay for " &
        getCurrentExceptionMsg() & ".", title = "Can't install module.")
  except UniqueModuleError:
    dialog = setMessage(message = "You can't install another " & getCurrentExceptionMsg() &
        " because you have installed one module of that type. Remove the old first.",
        title = "Can't install module.")
  except InstallationError, RemovingError:
    dialog = setMessage(message = getCurrentExceptionMsg(), title = "Can't " &
      (if currentTab == 0: "install" else: "remove") & " module.")
  except NoFreeCargoError:
    dialog = setMessage(message = "You don't have enough free space for " & moneyName &
        " in ship cargo.", title = "Can't remove module")
  except NoMoneyInBaseError:
    dialog = setMessage(message = "Base don't haev enough " & moneyName &
        " for buy this module.", title = "Can't remove module")
  except:
    dialog = setError(message = "Can't " & (if currentTab == 0: "install"
      else: "remove") & " module.")

proc showInstallInfo(dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the selected module information
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  const
    width: float = 600
    height: float = 500

  let
    module: BaseModuleData = try:
        modulesList[moduleIndex]
      except:
        dialog = setError(message = "Can't get module data.")
        return
    windowName: string = module.name
  setDialog(x = windowWidth / 5, y = windowHeight / 9)
  updateDialog(width = width, height = height)
  window(name = windowName, x = dialogX, y = dialogY, w = width, h = height,
      flags = {windowBorder, windowTitle, windowMovable}):
    if compareList.len > 0:
      setLayoutRowDynamic(height = 30, cols = 2, ratio = [0.4.cfloat, 0.6])
      label(str = "Compare with:")
      compareIndex = comboList(items = compareList, selected = compareIndex,
        itemHeight = 25, x = 300, y = 150)
    setLayoutRowDynamic(height = 30, cols = 2)
    label(str = "Install cost:")
    var cost: Natural = try:
        modulesList[moduleIndex].price
      except:
        dialog = setError(message = "Can't set the cost.")
        return
    try:
      countPrice(price = cost, traderIndex = findMember(order = talk))
    except:
      dialog = setError(message = "Can't count the cost.")
    if cost <= moneyAmount:
      colorLabel(str = $cost & " " & moneyName, color = theme.colors[goldenColor])
    else:
      colorLabel(str = $cost & " " & moneyName, color = theme.colors[redColor])
    label(str = "Install time:")
    try:
      colorLabel(str = $modulesList[moduleIndex].installTime & " minutes",
          color = theme.colors[goldenColor])
    except:
      dialog = setError(message = "Can't show the module's install time.")
    setModuleInfo(dialog = dialog, installing = true)
    var
      maxSize, usedSpace, allSpace: Natural = 0
      freeTurretIndex: int = -1
    for index, module in playerShip.modules:
      case module.mType
      of hull:
        try:
          maxSize = modulesList[module.protoIndex].value
        except:
          dialog = setError(message = "Can't get max module size.")
        usedSpace = module.installedModules
        allSpace = module.maxModules
      of turret:
        try:
          if module.gunIndex == -1 and modulesList[module.protoIndex].size >=
              modulesList[moduleIndex].size:
            freeTurretIndex = index
        except:
          dialog = setError(message = "Can't get free turret index.")
      else:
        discard
    var hasUnique: bool = false
    for module in playerShip.modules:
      try:
        if modulesList[module.protoIndex].mType == modulesList[
            moduleIndex].mType and modulesList[moduleIndex].unique:
          hasUnique = true
          break
      except:
        dialog = setError(message = "Can't check unique module.")
    var btnAmount: Positive = 2
    setLayoutRowDynamic(height = 60, cols = 1)
    if moneyAmount == 0:
      colorWrapLabel(str = "You don't have any money to buy the module.",
          color = theme.colors[redColor])
      btnAmount = 1
    else:
      try:
        if moneyAmount < cost:
          colorWrapLabel(str = "You don't have enough money to buy the module.",
              color = theme.colors[redColor])
          btnAmount = 1
        elif hasUnique:
          colorWrapLabel(str = "Only one module of that type can be installed on the ship.",
              color = theme.colors[redColor])
          btnAmount = 1
        elif modulesList[moduleIndex].mType notin {ModuleType.gun, harpoonGun, hull}:
          if modulesList[moduleIndex].size > maxSize:
            colorWrapLabel(str = "The selected module is too big for your's ship's hull.",
                color = theme.colors[redColor])
            btnAmount = 1
          elif allSpace - usedSpace < modulesList[moduleIndex].size and
              modulesList[moduleIndex].mType != ModuleType.armor:
            colorWrapLabel(str = "You don't have enough space in your ship's hull to install the module.",
                color = theme.colors[redColor])
            btnAmount = 1
          elif modulesList[moduleIndex].mType == ModuleType.hull and
              modulesList[moduleIndex].maxValue < usedSpace:
            colorWrapLabel(str = "The selected hull is too small to replace your current hull.",
                color = theme.colors[redColor])
            btnAmount = 1
          elif modulesList[moduleIndex].mType in {ModuleType.gun,
              harpoonGun} and freeTurretIndex == -1:
            colorWrapLabel(str = "You don't have a free turret to install the selected gun.",
                color = theme.colors[redColor])
            btnAmount = 1
      except:
        dialog = setError(message = "Can't set error label.")
    setLayoutRowDynamic(height = 30, cols = btnAmount)
    if btnAmount == 2:
      setButtonStyle(field = textNormal, color = theme.colors[greenColor])
      imageLabelButton(image = images[buyIcon], text = "Install", alignment = right):
        manipulateModule(dialog = dialog)
      restoreButtonStyle()
      addCloseButton(dialog = dialog, icon = cancelIcon, color = redColor,
          isPopup = false, label = "Cancel")
    else:
      addCloseButton(dialog = dialog, isPopup = false)

  windowSetFocus(name = windowName)

proc setRemoveInfo(data: int; dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the selected module information
  ##
  ## * data   - the index of the selected item
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog.
  moduleIndex = modulesIndexes[data]
  dialog = moduleDialog

proc showRemoveInfo(dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the selected module information
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  const
    width: float = 600
    height: float = 500

  let windowName: string = playerShip.modules[moduleIndex].name
  setDialog(x = windowWidth / 5, y = windowHeight / 9)
  updateDialog(width = width, height = height)
  window(name = windowName, x = dialogX, y = dialogY, w = width, h = height,
      flags = {windowBorder, windowTitle, windowMovable}):
    setLayoutRowDynamic(height = 30, cols = 2)
    label(str = "Remove gain:")
    let damagePercent: float = (playerShip.modules[moduleIndex].durability.float /
          playerShip.modules[moduleIndex].maxDurability.float)
    var cost: Natural = try:
          modulesList[playerShip.modules[moduleIndex].protoIndex].price - (
              modulesList[playerShip.modules[
              moduleIndex].protoIndex].price.float * (1.0 -
              damagePercent)).Natural
        except:
          dialog = setError(message = "Can't set the cost.")
          return
    if cost == 0:
      cost = 1
    try:
      countPrice(price = cost, traderIndex = findMember(order = talk),
          reduce = false)
    except:
      dialog = setError(message = "Can't count the cost.")
      return
    colorLabel(str = $cost & " " & moneyName, color = theme.colors[greenColor])
    label(str = "Removing time:")
    try:
      colorLabel(str = $modulesList[playerShip.modules[
          moduleIndex].protoIndex].installTime & " minutes",
          color = theme.colors[goldenColor])
    except:
      dialog = setError(message = "Can't show install time.")
      return
    setModuleInfo(dialog = dialog, installing = false)
    if damagePercent < 1.0:
      var
        statusTooltip: string = ""
        statusColor: ColorsNames = redColor
      if damagePercent < 1.0 and damagePercent > 0.79:
        statusColor = greenColor
        statusTooltip = "Slightly damaged"
      elif damagePercent < 0.8 and damagePercent > 0.49:
        statusColor = yellowColor
        statusTooltip = "Damaged"
      elif damagePercent < 0.5 and damagePercent > 0.19:
        statusColor = yellowColor
        statusTooltip = "Heavily damaged"
      elif damagePercent < 0.2 and damagePercent > 0.0:
        statusTooltip = "Almost destroyed"
      elif damagePercent == 0.0:
        statusTooltip = "Destroyed"
      label(str = "Status:")
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(), text = statusTooltip)
      var val: Natural = (damagePercent * 100.0).int
      changeStyle(field = progressbar, color = theme.colors[statusColor]):
        progressBar(value = val, maxValue = 100, modifyable = false)
    setLayoutRowDynamic(height = 30, cols = 1)
    try:
      if modulesList[playerShip.modules[
          moduleIndex].protoIndex].description.len > 0:
        label(str = "")
        setLayoutRowDynamic(height = 90, cols = 1)
        wrapLabel(str = modulesList[playerShip.modules[
            moduleIndex].protoIndex].description)
    except:
      dialog = setError(message = "Can't show module's description")
      return
    setLayoutRowDynamic(height = 30, cols = 2)
    setButtonStyle(field = textNormal, color = theme.colors[greenColor])
    imageLabelButton(image = images[sellIcon], text = "Remove", alignment = right):
      manipulateModule(dialog = dialog)
    restoreButtonStyle()
    addCloseButton(dialog = dialog, icon = cancelIcon, color = redColor,
        isPopup = false, label = "Cancel")

  windowSetFocus(name = windowName)

proc showModuleInfo*(dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the selected module information
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  if currentTab == 0:
    showInstallInfo(dialog = dialog)
  else:
    showRemoveInfo(dialog = dialog)

var
  headers: array[5, HeaderData[ModulesSortOrders]] = [
    HeaderData[ModulesSortOrders](label: "Name", sortAsc: nameAsc,
        sortDesc: nameDesc),
    HeaderData[ModulesSortOrders](label: "Type", sortAsc: typeAsc,
        sortDesc: typeDesc),
    HeaderData[ModulesSortOrders](label: "Size", sortAsc: sizeAsc,
        sortDesc: sizeDesc),
    HeaderData[ModulesSortOrders](label: "Material", sortAsc: materialAsc,
        sortDesc: materialDesc),
    HeaderData[ModulesSortOrders](label: "Cost", sortAsc: priceAsc,
        sortDesc: priceDesc)]
  hasOptions: bool = true
const
  ratio: array[5, cfloat] = [300.cfloat, 200, 200, 200, 200]

proc showShipyard*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the UI with the list of modules to install or remove from the
  ## player's ship
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  if showHeader(dialog = dialog, close = CloseDestination.map, state = state,
      options = hasOptions):
    return
  # Show tab buttons
  changeStyle(field = spacing, x = 0, y = 0):
    changeStyle(field = buttonRounding, value = 0):
      setLayoutRowDynamic(height = 30, cols = 2)
      const tabs: array[2, string] = ["Install modules", "Remove modules"]
      for index, tab in tabs:
        try:
          if currentTab == index:
            changeStyle(src = active, dest = normal):
              labelButton(title = tab):
                discard
          else:
            labelButton(title = tab):
              currentTab = index.cint
              setModulesList(dialog = dialog)
              if index == 0:
                headers[4].label = "Cost"
                hasOptions = true
              else:
                headers[4].label = "Price"
                hasOptions = false
        except:
          dialog = setError(message = "Can't set the tabs buttons.")
  # Show information about money owned by the player
  setLayoutRowStatic(height = 30, cols = moneyWidth.len, ratio = moneyWidth)
  for index, text in moneyText:
    if index mod 2 == 0:
      label(str = text)
    else:
      colorLabel(str = text, color = theme.colors[goldenColor])
  # Show information about installed modules
  setLayoutRowStatic(height = 30, cols = 5, ratio = modulesWidth)
  label(str = modulesText[0])
  colorLabel(str = modulesText[1], color = if modulesAmount.installed <
      modulesAmount.max: theme.colors[greenColor] else: theme.colors[redColor])
  label(str = modulesText[2])
  colorLabel(str = modulesText[3], color = theme.colors[goldenColor])
  label(str = modulesText[4])
  # Show advanced options if needed
  if showOptions:
    setLayoutRowDynamic(height = 30, cols = 3, ratio = [0.2.cfloat, 0.3, 0.5])
    label(str = "Show modules:")
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Show only modules of the selected type")
    let newType: Natural = comboList(items = typesList,
        selected = typeIndex, itemHeight = 25, x = 200, y = 150)
    if newType != typeIndex:
      typeIndex = newType
      setModulesList(dialog = dialog)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Enter a name of a module which you looking for")
    editString(text = nameSearch, maxLen = 64)
  let tableHeight: float = windowHeight - gameSettings.messagesPosition.float -
      80 - (if showOptions: 45 else: 0)
  setLayoutRowDynamic(height = tableHeight, cols = 1)
  group(title = "ShipyardGroup", flags = {windowNoFlags}):
    if dialog != none:
      windowDisable()
    # Show the list of modules
    addHeader(headers = headers, ratio = ratio, tooltip = "items",
      code = sortModules, dialog = dialog)
    saveButtonStyle()
    setButtonStyle(field = borderColor, a = 0)
    try:
      setButtonStyle(field = normal, color = theme.colors[tableRowColor])
      setButtonStyle(field = textNormal, color = theme.colors[tableTextColor])
    except:
      dialog = setError(message = "Can't set table color")
      return
    setButtonStyle(field = rounding, value = 0)
    setButtonStyle(field = border, value = 0)
    var currentRow, row: Positive = 1
    let startRow: Positive = ((currentPage - 1) * gameSettings.listsLimit) + 1
    for index in modulesIndexes:
      if currentRow < startRow:
        currentRow.inc
        continue
      # Show modules to install
      if currentTab == 0:
        try:
          if nameSearch.len > 0 and modulesList[index].name.toLowerAscii.find(
              sub = nameSearch.toLowerAscii) == -1:
            continue
        except:
          dialog = setError(message = "Can't check the module's name.")
        try:
          addButton(label = modulesList[index].name,
              tooltip = "Show the module's info", data = index,
              code = setInstallInfo, dialog = dialog)
        except:
          dialog = setError(message = "Can't add button with name.")
        try:
          addButton(label = getModuleType(moduleIndex = index),
              tooltip = "Show the module's info", data = index,
              code = setInstallInfo, dialog = dialog)
        except:
          dialog = setError(message = "Can't add button with type.")
        let moduleSize: int = try:
            (if modulesList[index].mType ==
              ModuleType.hull: modulesList[index].maxValue else: modulesList[index].size)
          except:
            dialog = setError(message = "Can't get size of the module.")
            0
        try:
          addButton(label = $moduleSize, tooltip = "Show the module's info",
              data = index, code = setInstallInfo, color = (if modulesList[
              index].mType == ModuleType.hull: (if moduleSize <
              modulesAmount.max: redColor elif moduleSize >
              modulesAmount.max: greenColor else: tableTextColor) else: (
              if moduleSize > maxModuleSize: redColor else: tableTextColor)),
              dialog = dialog)
        except:
          dialog = setError(message = "Can't add button with size.")
        try:
          addButton(label = modulesList[index].repairMaterial,
              tooltip = "Show the module's info", data = index,
              code = setInstallInfo, dialog = dialog)
        except:
          dialog = setError(message = "Can't add button with repair material.")
        var cost: Natural = try:
            modulesList[index].price
          except:
            dialog = setError(message = "Can't get cost.")
            return
        try:
          countPrice(price = cost, traderIndex = findMember(order = talk))
        except:
          dialog = setError(message = "Can't count price.")
        addButton(label = $cost, tooltip = "Show the module's info",
          data = index, code = setInstallInfo, color =
          (if cost <= moneyAmount: tableTextColor else: redColor),
          dialog = dialog)
      # Show modules to remove
      else:
        try:
          if modulesList[playerShip.modules[index].protoIndex].mType ==
              ModuleType.hull:
            continue
        except:
          dialog = setError(message = "Can't check module type.")
        if currentRow < startRow:
          currentRow.inc
          continue
        addButton(label = playerShip.modules[index].name,
            tooltip = "Show the module's info", data = index,
            code = setRemoveInfo, dialog = dialog)
        try:
          addButton(label = getModuleType(moduleIndex = playerShip.modules[
              index].protoIndex), tooltip = "Show the module's info",
              data = index, code = setRemoveInfo, dialog = dialog)
        except:
          dialog = setError(message = "Can't add button with player's ship module type.")
        try:
          addButton(label = $modulesList[playerShip.modules[
              index].protoIndex].size, tooltip = "Show the module's info",
              data = index, code = setRemoveInfo, dialog = dialog)
        except:
          dialog = setError(message = "Can't add button with player's ship module size.")
        try:
          addButton(label = $modulesList[playerShip.modules[
              index].protoIndex].repairMaterial,
              tooltip = "Show the module's info", data = index,
              code = setRemoveInfo, dialog = dialog)
        except:
          dialog = setError(message = "Can't add button with player's ship repair material.")
        let damage: float = 1.0 - (playerShip.modules[index].durability.float /
            playerShip.modules[index].maxDurability.float)
        var cost: Natural = try:
            modulesList[playerShip.modules[
              index].protoIndex].price - (modulesList[playerShip.modules[
              index].protoIndex].price.float * damage).int
          except:
            dialog = setError(message = "Can't get cost of player's ship module.")
            return
        if cost == 0:
          cost = 1
        try:
          countPrice(price = cost, traderIndex = findMember(order = talk),
              reduce = false)
        except:
          dialog = setError(message = "Can't count cost of player's ship module.")
        addButton(label = $cost, tooltip = "Show the module's info",
            data = index, code = setRemoveInfo, dialog = dialog)
      row.inc
      if row == gameSettings.listsLimit + 1:
        break
    restoreButtonStyle()
    addPagination(page = currentPage, row = row)
  showLastMessages(theme = theme, dialog = dialog, height = windowHeight - tableHeight)
