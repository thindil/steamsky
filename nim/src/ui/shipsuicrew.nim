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

import std/strutils
import ../[game, tk, types]
import coreui

proc hasSelection(): bool {.sideEffect, raises: [], tags: [].} =
  ## Check if there is any crew member selected on the list
  ##
  ## Returns true if there is any crew member selected on the list, otherwise
  ## false
  for i in playerShip.crew.low .. playerShip.crew.high:
    if tclGetVar(varName = "crewindex" & $(i + 1)) == "1":
      return true
  return false

proc updateCrewInfo*(page: Positive = 1; skill: Natural = 0) =
  let
    crewInfoFrame = mainPaned & ".shipinfoframe.crew.canvas.frame"
    gridSize = tclEval2(script = "grid size " & crewInfoFrame).split(' ')
    rows = gridSize[2].parseInt
  deleteWidgets(startIndex = 1, endIndex = rows - 1, frame = crewInfoFrame)
  var needRepair, needClean = false
  for module in playerShip.modules:
    if module.durability < module.maxDurability:
      needRepair = true
    if module.durability > 0 and module.mType == ModuleType2.cabin and
        module.cleanliness < module.quality:
      needClean = true
    if needRepair and needClean:
      break
  let
    buttonsFrame = crewInfoFrame & ".ordersbuttons"
    ordersLabel = buttonsFrame & ".label"
  tclEval(script = "ttk::label " & ordersLabel & " -text {Orders for all:}")
  tclEval(script = "grid " & ordersLabel & " -padx {5 2}")
  var button = buttonsFrame & ".rest"
  tclEval(script = "ttk::button " & button & " -image goresticon -command {OrderForAll Rest}")
  tclEval(script = "grid " & button & " -row 0 -column 1 -padx {0 2}")
  if needClean:
    button = buttonsFrame & ".clean"
    tclEval(script = "ttk::button " & button & " -image cleanordericon -command {OrderForAll Clean}")
    tclEval(script = "grid " & button & " -row 0 -column 2 -padx {0 2}")
  if needRepair:
    button = buttonsFrame & ".repair"
    tclEval(script = "ttk::button " & button & " -image repairordericon -command {OrderForAll Repair}")
    if needClean:
      tclEval(script = "grid " & button & " -row 0 -column 3")
    else:
      tclEval(script = "grid " & button & " -row 0 -column 2")

# Temporary code for interfacing with Ada

proc hasAdaSelection(): cint {.raises: [], tags: [], exportc.} =
  return (if hasSelection(): 1 else: 0)
