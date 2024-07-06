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

import std/[strutils, tables]
import ../[config, game, maps, tk, types]
import coreui, dialogs, mapsui, table, utilsui2

proc getHighestAttribute(baseIndex: BasesRange;
    memberIndex: Natural): string {.sideEffect, raises: [], tags: [].} =
  ## Get the highest attribute's name of the selected recruit
  ##
  ## * baseIndex   - The index of the base in which the recruit's attributes
  ##                 will be check
  ## * memberIndex - The index of the recruit which attributes will be check
  ##
  ## Returns the name of the attribute with the highest level of the selected
  ## recruit
  var
    highestLevel = 1
    highestIndex = 0
  for index, attrib in skyBases[baseIndex].recruits[memberIndex].attributes:
    if attrib.level > highestLevel:
      highestLevel = attrib.level
      highestIndex = index
  return attributesList[highestIndex].name

proc getHighestSkill(baseIndex: BasesRange;
    memberIndex: Natural): string {.sideEffect, raises: [], tags: [].} =
  ## Get the highest skill's name of the selected recruit
  ##
  ## * baseIndex   - The index of the base in which the recruit's skill will
  ##                 be check
  ## * memberIndex - The index of the recruit which skills will be check
  ##
  ## Returns the name of the skill with the highest level of the selected
  ## recruit
  var
    highestLevel = 1
    highestIndex = 0
  for skill in skyBases[baseIndex].recruits[memberIndex].skills:
    if skill.level > highestLevel:
      highestLevel = skill.level
      highestIndex = skill.index
  try:
    return skillsList[highestIndex].name
  except:
    return ""

var
  recruitTable: TableWidget
  recruitsIndexes: seq[Natural]

proc showRecruitCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
    RootEffect], exportc.} =
  ## Show the selected base available recruits
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowRecruit
  var recruitFrame = mainPaned & ".recruitframe"
  let baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  if tclEval2(script = "winfo exists " & recruitFrame) == "0":
    tclEval(script = "ttk::frame " & recruitFrame)
    recruitTable = createTable(parent = recruitFrame, headers = @[
        "Name", "Gender", "Faction", "Base cost", "Highest stat",
        "Highest skill"], command = "SortRecruits",
        tooltipText = "Press mouse button to sort the recruits.")
    tclEval(script = "bind " & recruitFrame & " <Configure> {ResizeCanvas " &
        recruitTable.canvas & " %w %h}")
  elif tclEval2(script = "winfo ismapped " & recruitFrame) == "1" and (argc ==
      1 or skyBases[baseIndex].recruits.len == 0):
    tclEval(script = "grid remove " & closeButton)
    showSkyMap(clear = true)
    return tclOk
  tclSetVar(varName = "gamestate", newValue = "recruit")
  tclEval(script = "grid " & closeButton & " -row 0 -column 1")
  if recruitsIndexes.len != skyBases[baseIndex].recruits.len:
    recruitsIndexes = @[]
    for index, _ in skyBases[baseIndex].recruits:
      recruitsIndexes.add(y = index)
  clearTable(table = recruitTable)
  let
    page = try:
        (if argc == 2: ($argv[1]).parseInt else: 1)
      except:
        return showError(message = "Can't get the page.")
    startRow = ((page - 1) * gameSettings.listsLimit) + 1
  var currentRow = 1
  for index in recruitsIndexes:
    if currentRow < startRow:
      currentRow.inc
      continue
    addButton(table = recruitTable, text = skyBases[baseIndex].recruits[
        index].name, tooltip = "Show recruit's details",
        command = "ShowRecruitInfo " & $(index + 1), column = 1)
    addButton(table = recruitTable, text = (if skyBases[baseIndex].recruits[
        index].gender == 'F': "Female" else: "Male"),
        tooltip = "Show recruit's details", command = "ShowRecruitInfo " & $(
        index + 1), column = 2)
    try:
      addButton(table = recruitTable, text = factionsList[skyBases[
          baseIndex].recruits[index].faction].name,
          tooltip = "Show recruit's details", command = "ShowRecruitInfo " & $(
          index + 1), column = 3)
    except:
      return showError(message = "Can't get the recruit name.")
    addButton(table = recruitTable, text = $skyBases[baseIndex].recruits[
        index].price, tooltip = "Show recruit's details",
        command = "ShowRecruitInfo " & $(index + 1), column = 4)
    addButton(table = recruitTable, text = getHighestAttribute(
        baseIndex = baseIndex, memberIndex = index),
        tooltip = "Show recruit's details", command = "ShowRecruitInfo " & $(
        index + 1), column = 5)
    addButton(table = recruitTable, text = getHighestSkill(
        baseIndex = baseIndex, memberIndex = index),
        tooltip = "Show recruit's details", command = "ShowRecruitInfo " & $(
        index + 1), column = 6, newRow = true)
    if recruitTable.row == gameSettings.listsLimit + 1:
      break
  if page > 1:
    if recruitTable.row < gameSettings.listsLimit + 1:
      addPagination(table = recruitTable, previousCommand = "ShowRecruit " & $(
          page - 1), nextCommand = "")
    else:
      addPagination(table = recruitTable, previousCommand = "ShowRecruit " & $(
          page - 1), nextCommand = "ShowRecruit " & $(page + 1))
  elif recruitTable.row == gameSettings.listsLimit + 1:
    addPagination(table = recruitTable, previousCommand = "",
        nextCommand = "ShowRecruit " & $(page + 1))
  updateTable(table = recruitTable)
  tclEval(script = recruitTable.canvas & " configure -scrollregion [list " &
      tclEval2(script = recruitTable.canvas & " bbox all") & "]")
  showScreen(newScreenName = "recruitframe")
  return tclOk

var recruitIndex: Natural

proc showRecruitInfoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.exportc.} =
  recruitIndex = ($argv[1]).parseInt - 1
  let
    baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
    recruit = skyBases[baseIndex].recruits[recruitIndex]
    recruitDialog = createDialog(name = ".recruitdialog", title = recruit.name)
  const tabNames: array[4, string] = ["General", "Attributes", "Skills", "Inventory"]
  tclSetVar(varName = "newtab", newValue = tabNames[0])
  var frame = recruitDialog & " .buttonbox"
  tclEval(script = "ttk::frame " & frame)
  for index, tab in tabNames:
    let tabButton = frame & "." & tab.toLowerAscii
    tclEval(script = "ttk::radiobutton " & tabButton & " -text " & tab &
        " -style Radio.Toolbutton -value " & tab.toLowerAscii & " -variable newtab -command ShowRecruitTab")
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the trades UI
  try:
    discard
#    addCommand("ShowRecruit", showRecruitCommand)
#    addCommand("ShowRecruitInfo", showRecruitInfoCommand)
  except:
    showError(message = "Can't add a Tcl command.")

# Temporary code for interfacing with Ada

proc getAdaHighestAttribute(baseIndex, memberIndex: cint): cstring {.exportc.} =
  return getHighestAttribute(baseIndex = baseIndex, memberIndex = memberIndex - 1).cstring

proc getAdaHighestRecSkill(baseIndex, memberIndex: cint): cstring {.exportc.} =
  return getHighestSkill(baseIndex = baseIndex, memberIndex = memberIndex - 1).cstring
