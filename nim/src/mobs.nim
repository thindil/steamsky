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
import crew, items, utils

proc getRandomItem*(itemsIndexes: seq[Positive], equipIndex: EquipmentLocations,
    highestLevel, weaponSkillLevel: Positive, factionIndex: string): Natural =
  var
    itemIndex, maxIndex: Positive
    newIndexes: seq[Positive]
    added: bool
  if equipIndex > weapon:
    for i in 0..<itemsIndexes.len:
      added = false
      for j in 0..<newIndexes.len:
        if itemsList[itemsIndexes[i]].price < itemsList[newIndexes[j]].price:
          {.warning[UnsafeSetLen]: off.}
          newIndexes.insert(item = i, i = j)
          {.warning[UnsafeSetLen]: on.}
          added = true
          break
      if not added:
        newIndexes.add(y = i)
    maxIndex = ((newIndexes.len - 1).float * (highestLevel.float / 100.0) + 1.0).Positive
    if maxIndex > newIndexes.len - 1:
      maxIndex = newIndexes.len - 1
    itemIndex = getRandom(min = 0, max = maxIndex)
