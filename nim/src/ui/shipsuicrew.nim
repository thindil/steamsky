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

import std/[strutils, tables]
import ../[config, crew, game, shipscrew, tk, types]
import coreui, table

var
  crewTable: TableWidget
    ## The UI table with all members of the player's ship's crew
  crewIndexes: seq[Natural]
    ## The list of indexes of the crew members

proc hasSelection(): bool {.sideEffect, raises: [], tags: [].} =
  ## Check if there is any crew member selected on the list
  ##
  ## Returns true if there is any crew member selected on the list, otherwise
  ## false
  for i in playerShip.crew.low .. playerShip.crew.high:
    if tclGetVar(varName = "crewindex" & $(i + 1)) == "1":
      return true
  return false

proc updateTooltips() {.sideEffect, raises: [], tags: [].} =
  ## Update the tooltips of the button with order for everyone/selected crew
  ## member in dependency on the selection of the crew members on the list.
  let
    buttonsFrame = mainPaned & ".shipinfoframe.crew.canvas.frame.ordersbuttons"
    selection = hasSelection()
  var button = buttonsFrame & ".label"
  if tclEval2(script = "winfo exists " & button) == "1":
    tclEval(script = button & " configure -text {Orders for " & (
        if selection: "selected" else: "all") & ":}")
    tclEval(script = "tooltip::tooltip " & button &
        " \"Give the selected order to the " & (
        if selection: "selected crew members" else: "whole crew") & ".\"")
  button = buttonsFrame & ".rest"
  if tclEval2(script = "winfo exists " & button) == "1":
    tclEval(script = "tooltip::tooltip " & button & " \"Go rest " & (
        if selection: "selected crew members" else: "everyone") & "\"")
  button = buttonsFrame & ".clean"
  if tclEval2(script = "winfo exists " & button) == "1":
    tclEval(script = "tooltip::tooltip " & button & " \"Clean the ship " & (
        if selection: "selected crew members" else: "everyone") & "\"")
  button = buttonsFrame & ".repair"
  if tclEval2(script = "winfo exists " & button) == "1":
    tclEval(script = "tooltip::tooltip " & button & " \"Repair the ship " & (
        if selection: "selected crew members" else: "everyone") & "\"")

proc getHighestSkill(memberIndex: Natural): string {.sideEffect, raises: [
    KeyError], tags: [].} =
  ## Get the name of the highest skill of the selected crew member
  ##
  ## * memberIndex - the crew index of the member which the highest skill will
  ##                 be get
  ##
  ## Returns the name of the highest skill of the selected crew member
  var
    highestLevel = 1
    highestIndex = 1
  for skill in playerShip.crew[memberIndex].skills:
    if skill.level > highestLevel:
      highestLevel = skill.level
      highestIndex = skill.index
  return skillsList[highestIndex].name

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
  var
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
  updateTooltips()
  tclEval(script = "grid " & buttonsFrame & " -sticky w")
  buttonsFrame = crewInfoFrame & ".selectskill"
  ordersLabel = buttonsFrame & ".label"
  tclEval(script = "ttk::label " & ordersLabel & " -text {Skill:}")
  tclEval(script = "tooltip::tooltip " & ordersLabel & " \"Show the level of the selected skill for the crew\nmembers.If selected option 'Highest', show the\nhighest skill of the crew members.\"")
  tclEval(script = "grid " & ordersLabel & " -padx {5 2}")
  var skills = " {Highest}"
  for skill in skillsList.values:
    skills.add(" {" & skill.name & "}")
  let skillBox = crewInfoFrame & ".selectskill.combox"
  tclEval(script = "ttk::combobox " & skillBox &
      " -state readonly -values [list" & skills & "]")
  tclEval(script = "bind " & skillBox & " <<ComboboxSelected>> SelectCrewSkill")
  tclEval(script = skillBox & " current " & $skill)
  tclEval(script = "tooltip::tooltip " & skillBox & " \"Show the level of the selected skill for the crew\nmembers.If selected option 'Highest', show the\nhighest skill of the crew members.\"")
  tclEval(script = "grid " & skillBox & " -row 0 -column 1")
  button = buttonsFrame & ".selectallbutton"
  tclEval(script = "ttk::button " & button & " -image selectallicon -command {ToggleAllCrew select} -style Small.TButton")
  tclEval(script = "tooltip::tooltip " & button & " \"Select all crew members.\"")
  tclEval(script = "grid " & button & " -padx {5 2}")
  button = buttonsFrame & ".unselectallbutton"
  tclEval(script = "ttk::button " & button & " -image unselectallicon -command {ToggleAllCrew unselect} -style Small.TButton")
  tclEval(script = "tooltip::tooltip " & button & " \"Unselect all crew members.\"")
  tclEval(script = "grid " & button & " -sticky w -row 1 -column 1")
  tclEval(script = "grid " & buttonsFrame & " -sticky w")
  crewTable = createTable(parent = crewInfoFrame, headers = @["", "Name",
      "Order", "Skill", "Health", "Fatigue", "Thirst", "Hunger", "Morale"],
      scrollbar = ".gameframe.paned.shipinfoframe.crew.scrolly",
      command = "SortShipCrew",
      tooltipText = "Press mouse button to sort the crew.")
  if crewIndexes.len != playerShip.crew.len:
    crewIndexes = @[]
    for i in playerShip.crew.low .. playerShip.crew.high:
      crewIndexes.add(i)
  var currentRow = 1
  let startRow = ((page - 1) * gameSettings.listsLimit) + 1
  for index, mIndex in crewIndexes:
    if currentRow < startRow:
      currentRow.inc
      continue
    addCheckButton(table = crewTable, tooltip = "Select the crew member to give orders to them.",
        command = "ToggleCrewMember " & $(index + 1) & " " & $(mIndex + 1),
        checked = tclGetVar(varName = "crewindex" & $(mIndex + 1)) == "1",
        column = 1, emptyUnchecked = true)
    addButton(table = crewTable, text = playerShip.crew[mIndex].name,
        tooltip = "Show available crew member's options",
        command = "ShowMemberInfo " & $(mIndex + 1), column = 2)
    addButton(table = crewTable, text = ($playerShip.crew[
        mIndex].order).capitalizeAscii,
        tooltip = "The current order for the selected crew member.\nPress the mouse button to change it.",
        command = "ShowCrewOrder " & $(mIndex + 1), column = 3)
    if skill == 0:
      addButton(table = crewTable, text = getHighestSkill(memberIndex = mIndex),
          tooltip = "The highest skill of the selected crew member",
          command = "ShowMemberInfo " & $(mIndex + 1), column = 4)
    else:
      addButton(table = crewTable, text = getSkillLevelName(
          skillLevel = getSkillLevel(member = playerShip.crew[mIndex],
          skillIndex = skill)), tooltip = "The level of " & tclEval2(
          script = skillBox & " get") & " of the selected crew member",
          command = "ShowMemberInfo " & $(mIndex + 1), column = 4)
    addProgressbar(table = crewTable, value = playerShip.crew[mIndex].health,
        maxValue = SkillRange.high,
        tooltip = "The current health level of the selected crew member",
        command = "ShowMemberInfo " & $(mIndex + 1), column = 5)
    var tiredLevel = playerShip.crew[mIndex].tired - playerShip.crew[
        mIndex].attributes[conditionIndex].level
    if tiredLevel < 0:
      tiredLevel = 0
    addProgressbar(table = crewTable, value = tiredLevel,
        maxValue = SkillRange.high,
        tooltip = "The current tired level of the selected crew member",
        command = "ShowMemberInfo " & $(mIndex + 1), column = 6, newRow = false,
        invertColors = true)
    addProgressbar(table = crewTable, value = playerShip.crew[mIndex].thirst,
        maxValue = SkillRange.high,
        tooltip = "The current thirst level of the selected crew member",
        command = "ShowMemberInfo " & $(mIndex + 1), column = 7, newRow = false,
        invertColors = true)
    addProgressbar(table = crewTable, value = playerShip.crew[mIndex].hunger,
        maxValue = SkillRange.high,
        tooltip = "The current hunger level of the selected crew member",
        command = "ShowMemberInfo " & $(mIndex + 1), column = 8, newRow = false,
        invertColors = true)
    addProgressbar(table = crewTable, value = playerShip.crew[mIndex].morale[1],
        maxValue = SkillRange.high,
        tooltip = "The current morale level of the selected crew member",
        command = "ShowMemberInfo " & $(mIndex + 1), column = 9, newRow = true)
    if crewTable.row == gameSettings.listsLimit + 1:
      break
  if page > 1:
    addPagination(table = crewTable, previousCommand = "ShowCrew " & $(page -
        1) & " " & $skill, nextCommand = (if crewTable.row <
        gameSettings.listsLimit + 1: "" else: "ShowCrew " & $(page + 1) & " " & $skill))
  elif crewTable.row == gameSettings.listsLimit + 1:
    addPagination(table = crewTable, nextCommand = "ShowCrew " & $(page + 1) &
        " " & $skill)
  updateTable(table = crewTable)
  tclEval(script = "update")
  let shipCanvas = mainPaned & ".shipinfoframe.crew.canvas"
  tclEval(script = shipCanvas & " configure -scrollregion [list " & tclEval2(
      script = shipCanvas & " bbox all") & "]")
  tclEval(script = shipCanvas & " xview moveto 0.0")
  tclEval(script = shipCanvas & " yview moveto 0.0")

# Temporary code for interfacing with Ada

proc hasAdaSelection(): cint {.raises: [], tags: [], exportc.} =
  return (if hasSelection(): 1 else: 0)

proc updateAdaTooltips() {.raises: [], tags: [], exportc.} =
  updateTooltips()

proc getAdaHighestSkill(memberIndex: cint): cstring {.raises: [], tags: [], exportc.} =
  try:
    return getHighestSkill(memberIndex.int - 1).cstring
  except:
    return "".cstring

proc updateAdaCrewInfo(page, skill: cint; cIndexes: array[50, cint];
    columnsWidth: var array[10, cint]) {.raises: [], tags: [], exportc.} =
  crewIndexes = @[]
  for index in cIndexes:
    if index == 0:
      break
    crewIndexes.add(index - 1)
  try:
    updateCrewInfo(page = page, skill = skill)
  except:
    echo "error"
  for index, width in crewTable.columnsWidth:
    columnsWidth[index] = width.cint
