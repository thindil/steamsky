# Copyright 2022 Bartek thindil Jasicki
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

{.used.}

import std/tables
import crafts, crew, factions, items, utils

proc getRandomItem*(itemsIndexes: seq[Positive], equipIndex: EquipmentLocations,
    highestLevel, weaponSkillLevel: Positive, factionIndex: string): Natural =
  var
    itemIndex, maxIndex: Natural
    newIndexes: seq[Positive]
    added: bool
  if equipIndex > weapon:
    for index in itemsIndexes:
      added = false
      for j in 0..<newIndexes.len:
        if itemsList[index].price < itemsList[newIndexes[j]].price:
          {.warning[UnsafeSetLen]: off.}
          newIndexes.insert(item = index, i = j)
          {.warning[UnsafeSetLen]: on.}
          added = true
          break
      if not added:
        newIndexes.add(y = index)
    maxIndex = ((newIndexes.len - 1).float * (highestLevel.float / 100.0) + 1.0).Positive
    if maxIndex > newIndexes.len - 1:
      maxIndex = newIndexes.len - 1
    itemIndex = getRandom(min = 0, max = maxIndex)
  else:
    for index in itemsIndexes:
      added = false
      for j in 0..<newIndexes.len:
        if itemsList[index].price < itemsList[newIndexes[j]].price and
            itemsList[index].value[2] == factionsList[factionIndex].weaponSkill:
          {.warning[UnsafeSetLen]: off.}
          newIndexes.insert(item = index, i = j)
          {.warning[UnsafeSetLen]: on.}
          added = true
          break
      if not added and itemsList[index].value[2] == factionsList[
          factionIndex].weaponSkill:
        newIndexes.add(y = index)
    if newIndexes.len == 0:
      return 0
    maxIndex = ((newIndexes.len - 1).float * (weaponSkillLevel.float / 100.0) + 1.0).Positive
    if maxIndex > newIndexes.len - 1:
      maxIndex = newIndexes.len - 1
    while true:
      itemIndex = getRandom(min = 0, max = maxIndex)
      if itemsList[newIndexes[itemIndex]].value[2] == factionsList[
          factionIndex].weaponSkill:
        break
  for index in itemsIndexes:
    if index == newIndexes[itemIndex]:
      return newIndexes[itemIndex]
  return 0

# Temporary code for interfacing with Ada

proc getAdaRandomItem(items: cstring, equipIndex, highestLevel,
    weaponSkillLevel: cint; factionIndex: cstring;
        highestSkill: cint): cint {.exportc.} =
  case $items
  of "weapon":
    return getRandomItem(itemsIndexes = weaponsList,
        equipIndex = equipIndex.EquipmentLocations, highestLevel = highestLevel,
        weaponSkillLevel = weaponSkillLevel, factionIndex = $factionIndex).cint
  of "shield":
    return getRandomItem(itemsIndexes = shieldsList,
        equipIndex = equipIndex.EquipmentLocations, highestLevel = highestLevel,
        weaponSkillLevel = weaponSkillLevel, factionIndex = $factionIndex).cint
  of "helmet":
    return getRandomItem(itemsIndexes = headArmorsList,
        equipIndex = equipIndex.EquipmentLocations, highestLevel = highestLevel,
        weaponSkillLevel = weaponSkillLevel, factionIndex = $factionIndex).cint
  of "torso":
    return getRandomItem(itemsIndexes = chestArmorsList,
        equipIndex = equipIndex.EquipmentLocations, highestLevel = highestLevel,
        weaponSkillLevel = weaponSkillLevel, factionIndex = $factionIndex).cint
  of "arms":
    return getRandomItem(itemsIndexes = armsArmorsList,
        equipIndex = equipIndex.EquipmentLocations, highestLevel = highestLevel,
        weaponSkillLevel = weaponSkillLevel, factionIndex = $factionIndex).cint
  of "legs":
    return getRandomItem(itemsIndexes = legsArmorsList,
        equipIndex = equipIndex.EquipmentLocations, highestLevel = highestLevel,
        weaponSkillLevel = weaponSkillLevel, factionIndex = $factionIndex).cint
  of "tool":
    var tempToolsList: seq[Positive]
    for recipe in recipesList.values:
      if highestSkill == recipe.skill:
        for j in 0..<itemsList.len:
          if itemsList[j].itemType == recipe.tool:
            tempToolsList.add(y = j)
        break
    return getRandomItem(itemsIndexes = tempToolsList,
        equipIndex = equipIndex.EquipmentLocations, highestLevel = highestLevel,
        weaponSkillLevel = weaponSkillLevel, factionIndex = $factionIndex).cint
