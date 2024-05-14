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
import ../[crewinventory, game, tk]

proc checkTool(toolNeeded: string): bool {.sideEffect, raises: [], tags: [].} =
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
