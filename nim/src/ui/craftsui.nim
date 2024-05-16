# Copyright 2024 Bartek thindil Jasicki
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
import ../[crafts, crewinventory, game, tk, types]

proc checkTool(toolNeeded: string): bool {.sideEffect, raises: [], tags: [].} =
  ##  Check if the player has needed tool for the crafting recipe
  ##
  ##  * toolNeeded - The type of tool needed for the recipe
  ##
  ## Returns true if the tool is in the player ship cargo, otherwise false
  result = true
  if toolNeeded != "None":
    result = false
    for index, item in itemsList:
      if item.itemType == toolNeeded:
        let cargoIndex = findItem(inventory = playerShip.cargo,
            protoIndex = index)
        if cargoIndex > -1:
          result = true
          break

proc isCraftable(recipe: CraftData; canCraft, hasWorkplace, hasTool,
    hasMaterials: var bool) =
  canCraft = false
  hasWorkplace = false
  hasMaterials = false
  hasTool = false
  for module in playerShip.modules:
    if modulesList[module.protoIndex].mType == recipe.workplace and
        module.durability > 0:
      hasWorkplace = true
      break
  hasTool = checkTool(toolNeeded = recipe.tool)
  for materialIndex, material in recipe.materialTypes:
    hasMaterials = false
    for itemIndex, item in itemsList:
      if item.itemType == material:
        var cargoIndex = findItem(inventory = playerShip.cargo,
            protoIndex = itemIndex)
        if cargoIndex > -1 and playerShip.cargo[cargoIndex].amount >=
            recipe.materialAmounts[materialIndex]:
          hasMaterials = true
  if hasTool and hasMaterials and hasWorkplace:
    canCraft = true

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the crew UI
  try:
    discard
  except:
    showError(message = "Can't add a Tcl command.")

# Temporary code for interfacing with Ada

proc checkAdaTool(toolNeeded: cstring): int {.sideEffect, raises: [], tags: [], exportc.} =
  if checkTool(toolNeeded = $toolNeeded):
    return 1
  return 0

proc isAdaCraftable(adaRecipe: AdaCraftData; canCraft, hasWorkplace, hasTool,
    hasMaterials: var cint) {.sideEffect, raises: [], tags: [], exportc.} =
  var
    materials: seq[string]
    amounts: seq[Positive]
  for material in adaRecipe.materialTypes:
    if material.len > 0:
      materials.add(y = $material)
  for amount in adaRecipe.materialAmounts:
    if amount > 0:
      amounts.add(y = amount)
  let recipe = CraftData(workplace: adaRecipe.workplace.ModuleType,
      tool: $adaRecipe.tool, materialTypes: materials, materialAmounts: amounts)
  var cCraft, hWorkplace, hTool, hMaterials: bool = false
  try:
    isCraftable(recipe = recipe, canCraft = cCraft, hasWorkplace = hWorkplace,
        hasTool = hTool, hasMaterials = hMaterials)
  except:
    echo getCurrentExceptionMsg()
  canCraft = (if cCraft: 1 else: 0)
  hasWorkplace = (if hWorkplace: 1 else: 0)
  hasTool = (if hTool: 1 else: 0)
  hasMaterials = (if hMaterials: 1 else: 0)
