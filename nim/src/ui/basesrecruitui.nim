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
import ../[config, crew, game, maps, tk, types]
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
  var tabButton = ""
  for index, tab in tabNames:
    tabButton = frame & "." & tab.toLowerAscii
    tclEval(script = "ttk::radiobutton " & tabButton & " -text " & tab &
        " -style Radio.Toolbutton -value " & tab.toLowerAscii & " -variable newtab -command ShowRecruitTab")
    tclEval(script = "grid " & tabButton & " -column " & $index & " -row 0")
    tclEval(script = "bind " & tabButton & " <Escape> {" & recruitDialog & ".buttonbox2.button invoke;break}")
  tclEval(script = "bind " & tabButton & " <Tab> {focus " & recruitDialog & ".buttonbox2.hirebutton;break}")
  tclEval(script = "grid " & frame & " -pady {5 0} -columnspan 2")
  let
    recruitCanvas = recruitDialog & ".canvas"
    yScroll = recruitDialog & ".yscroll"
  tclEval(script = "ttk::scrollbar " & yScroll &
      " -orient vertical -command [" & recruitCanvas & " yview]")
  tclEval(script = "canvas " & recruitCanvas & " -yscrollcommand [list " &
      yScroll & " set]")
  tclEval(script = "grid " & recruitCanvas & " -sticky nwes -pady 5 -padx 5")
  tclEval(script = "grid " & yScroll & " -sticky ns -pady 5 -padx {0 5} -row 1 -column 1")
  frame = recruitDialog & ".buttonbox2"
  tclEval(script = "ttk::frame " & frame)
  let button = recruitDialog & ".buttonbox2.hirebutton"
  tclEval(script = "ttk::button " & button &
      " -text Negotiate -command {CloseDialog " & recruitDialog & ";Negotiate} -image negotiateicon -style Dialog.TButton")
  tclEval(script = "grid " & button)
  tclEval(script = "tooltip::tooltip " & button & " \"Start hiring negotiating.\"")
  let dialogCloseButton = recruitDialog & ".buttonbox2.button"
  tclEval(script = "ttk::button " & dialogCloseButton &
      " -text Close -command {CloseDialog " & recruitDialog & "} -image exiticon -style Dialog.TButton")
  tclEval(script = "grid " & dialogCloseButton & " -row 0 -column 1")
  tclEval(script = "tooltip::tooltip " & button & " \"Close dialog \\[Escape key\\]\"")
  tclEval(script = "grid " & frame & "  -pady {0 5}")
  tclEval(script = "focus " & dialogCloseButton)
  tclEval(script = "::autoscroll::autoscroll " & yScroll)
  # General info about the selected recruit
  frame = recruitCanvas & ".general"
  tclEval(script = "ttk::frame " & frame)
  var recruitText = frame & ".label"
  tclEval(script = "text " & recruitText & " -height 3 -width 30")
  tclEval(script = recruitText & " tag configure gold -foreground " & tclGetVar(
      varName = "ttk::theme::" & gameSettings.interfaceTheme &
      "::colors(-goldenyellow)"))
  let faction = factionsList[recruit.faction]
  if "nogender" notin faction.flags:
    tclEval(script = recruitText & " insert end {Gender: }")
    tclEval(script = recruitText & " insert end {" & (if recruit.gender ==
        'M': "Male" else: "Female") & "} [list gold]")
  tclEval(script = recruitText & " insert end {\nFaction: }")
  tclEval(script = recruitText & " insert end {" & faction.name & "} [list gold]")
  tclEval(script = recruitText & " insert end {\nHome base: }")
  tclEval(script = recruitText & " insert end {" & skyBases[
      recruit.homeBase].name & "} [list gold]")
  tclEval(script = recruitText & " configure -state disabled")
  tclEval(script = "grid " & recruitText & " -sticky w")
  tclEval(script = "grid " & frame)
  # Statistics of the selected recruit
  frame = recruitCanvas & ".attributes"
  tclEval(script = "ttk::frame " & frame)
  for index, attrib in recruit.attributes:
    let progressFrame = frame & ".statinfo" & $(index + 1)
    tclEval(script = "ttk::frame " & progressFrame)
    var recruitLabel = progressFrame & ".label"
    tclEval(script = "ttk::label " & recruitLabel & " -text {" & attributesList[
        index].name & ": }")
    tclEval(script = "grid " & recruitLabel & " -sticky w")
    recruitLabel = progressFrame & ".label2"
    tclEval(script = "ttk::label " & recruitLabel & " -text {" &
        getAttributeLevelName(attributeLevel = attrib.level) & "} -style Golden.TLabel")
    tclEval(script = "grid " & recruitLabel & " -sticky we -column 1 -row 0 -padx {5 0}")
    tclEval(script = "grid columnconfigure " & progressFrame & " " &
        recruitLabel & " -weight 1")
    tclEval(script = "grid rowconfigure " & progressFrame & " " & recruitLabel & " -weight 1")
    let infoButton = progressFrame & ".button"
    tclEval(script = "ttk::button " & infoButton &
        " -image helpicon -style Header.Toolbutton -command {ShowCrewStatsInfo " &
        $(index + 1) & " .recruitdialog}")
    tclEval(script = "tooltip::tooltip " & infoButton & " \"Show detailed information about the selected attribute.\"")
    tclEval(script = "grid " & infoButton & " -column 2 -row 0")
    tclEval(script = "grid " & progressFrame & " -sticky we -padx 5 -pady {5 0}")
    tclEval(script = "update")
    let progressBar = frame & ".level" & $(index + 1)
    tclEval(script = "ttk::progressbar " & progressBar & " -value " & $(
        attrib.level * 2) & " -length 360")
    tclEval(script = "tooltip::tooltip " & progressBar & " \"The current level of the attribute.\"")
    tclEval(script = "grid " & progressBar)
  # Skills of the selected recruit
  frame = recruitCanvas & ".skills"
  tclEval(script = "ttk:frame " & frame)
  for index, skill in recruit.skills:
    let progressFrame = frame & ".skillinfo" & $(index + 1)
    tclEval(script = "ttk::frame " & progressFrame)
    var recruitLabel = progressFrame & ".label"
    tclEval(script = "ttk::label " & recruitLabel & " -text {" & skillsList[
        skill.index].name & ": }")
    tclEval(script = "grid " & recruitLabel & " -sticky w")
    recruitLabel = progressFrame & ".label2"
    tclEval(script = "ttk::label " & recruitLabel & " -text {" &
        getSkillLevelName(skillLevel = skill.level) & "} -style Golden.TLabel")
    tclEval(script = "grid " & recruitLabel & " -sticky we -column 1 -row 0 -padx {5 0}")
    tclEval(script = "grid columnconfigure " & progressFrame & " " &
        recruitLabel & " -weight 1")
    tclEval(script = "grid rowconfigure " & progressFrame & " " & recruitLabel & " -weight 1")
    var toolQuality = 100
    for quality in skillsList[skill.index].toolsQuality:
      if skill.level <= quality.level:
        toolQuality = quality.quality
        break
    let infoButton = progressFrame & ".button"
    tclEval(script = "ttk::button " & infoButton &
        " -image helpicon -style Header.Toolbutton -command {ShowCrewSkillInfo " &
        $(index + 1) & " .recruitdialog}")
    tclEval(script = "tooltip::tooltip " & infoButton & " \"Show detailed information about the selected skill.\"")
    tclEval(script = "grid " & infoButton & " -column 2 -row 0")
    tclEval(script = "grid " & progressFrame & " -sticky we")
    let progressBar = frame & ".level" & $(index + 1)
    tclEval(script = "ttk::progressbar " & progressBar & " -value " &
        $skill.level & " -length 360")
    tclEval(script = "tooltip::tooltip " & progressBar & " \"The current level of the skill.\"")
    tclEval(script = "grid " & progressBar)
  # Equipment of the selected recruit
  frame = recruitCanvas & ".inventory"
  tclEval(script = "ttk::frame " & frame)
  recruitText = frame & ".label"
  tclEval(script = "text " & recruitText & " -height " &
      $recruit.equipment.len & " -width 30")
  tclEval(script = recruitText & " tag configure gold -foreground " & tclGetVar(
      varName = "ttk::theme::" & gameSettings.interfaceTheme &
      "::colors(-goldenyellow)"))
  for index, item in recruit.equipment:
    if item > -1:
      tclEval(script = recruitText & " insert end {" & (
          $index).capitalizeAscii & ": }")
      tclEval(script = recruitText & " insert end {" & itemsList[
          recruit.inventory[item]].name & "\n} [list gold]")
  tclEval(script = recruitText & " configure -state disabled")
  tclEval(script = "grid " & recruitText & " -sticky w")
  frame = recruitCanvas & ".general"
  tclEval(script = recruitCanvas & " create window 32 0 -anchor nw -window " &
      frame & " -tag info")
  tclEval(script = "update")
  tclEval(script = recruitCanvas & " configure -scrollregion [list " & tclEval2(
      script = recruitCanvas & " bbox all") & "]")
  tclEval(script = "bind " & dialogCloseButton & " <Tab> {focus " &
      recruitDialog & ".buttonbox.general;break}")
  tclEval(script = "bind " & recruitDialog & " <Escape> {" & dialogCloseButton & " invoke;break}")
  tclEval(script = "bind " & dialogCloseButton & " <Escape> {" &
      dialogCloseButton & " invoke;break}")
  showDialog(dialog = recruitDialog, relativeY = 0.2)
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

proc getRecruitIndex(): cint {.exportc.} =
  return recruitIndex.cint + 1
