# Copyright 2024-2025 Bartek thindil Jasicki
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

## Provides code related to recruit new crew members in bases, like the list
## of recruits to hire, hiring them, information about them, etc.

import std/[algorithm, strutils, tables]
import contracts, nimalyzer
import ../[bases, basestrade, config, crew, game, items, maps, shipscrew, tk,
  types]
import coreui, dialogs, errordialog, mapsui, table, updateheader, utilsui2

proc getHighestAttribute(baseIndex: BasesRange;
    memberIndex: Natural): string {.raises: [], tags: [], contractual.} =
  ## Get the highest attribute's name of the selected recruit
  ##
  ## * baseIndex   - The index of the base in which the recruit's attributes
  ##                 will be check
  ## * memberIndex - The index of the recruit which attributes will be check
  ##
  ## Returns the name of the attribute with the highest level of the selected
  ## recruit
  var
    highestLevel: Positive = 1
    highestIndex: Natural = 0
  for index, attrib in skyBases[baseIndex].recruits[memberIndex].attributes:
    if attrib.level > highestLevel:
      highestLevel = attrib.level
      highestIndex = index
  return attributesList[highestIndex].name

proc getHighestSkill(baseIndex: BasesRange;
    memberIndex: Natural): string {.raises: [], tags: [], contractual.} =
  ## Get the highest skill's name of the selected recruit
  ##
  ## * baseIndex   - The index of the base in which the recruit's skill will
  ##                 be check
  ## * memberIndex - The index of the recruit which skills will be check
  ##
  ## Returns the name of the skill with the highest level of the selected
  ## recruit
  var
    highestLevel, highestIndex: Positive = 1
  for skill in skyBases[baseIndex].recruits[memberIndex].skills:
    if skill.level > highestLevel:
      highestLevel = skill.level
      highestIndex = skill.index
  try:
    return skillsList[highestIndex].name
  except:
    return ""

var
  recruitTable: TableWidget = TableWidget()
  recruitsIndexes: seq[Natural] = @[]

proc showRecruitCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    RootEffect], cdecl, contractual, ruleOff: "params".} =
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
  var recruitFrame: string = mainPaned & ".recruitframe"
  let baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
      playerShip.skyY].baseIndex
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
    page: Natural = try:
        (if argc == 2: ($argv[1]).parseInt else: 1)
      except:
        return showError(message = "Can't get the page.")
    startRow: Natural = ((page - 1) * gameSettings.listsLimit) + 1
  var currentRow: Natural = 1
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

var recruitIndex: Natural = 0

proc showRecruitInfoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Show information about the selected recruit
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowRecruitInfo recruitindex
  ## RecruitIndex is a index of the recruit which menu will be shown
  recruitIndex = try:
      ($argv[1]).parseInt - 1
    except:
      return showError(message = "Can't get recruit index.")
  let
    baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
        playerShip.skyY].baseIndex
    recruit: RecruitData = skyBases[baseIndex].recruits[recruitIndex]
    recruitDialog: string = createDialog(name = ".recruitdialog",
        title = recruit.name)
  const tabNames: array[4, string] = ["General", "Attributes", "Skills", "Inventory"]
  tclSetVar(varName = "newtab", newValue = tabNames[0])
  var frame: string = recruitDialog & ".buttonbox"
  tclEval(script = "ttk::frame " & frame)
  var tabButton: string = ""
  for index, tab in tabNames:
    tabButton = frame & "." & tab.toLowerAscii
    tclEval(script = "ttk::radiobutton " & tabButton & " -text " & tab &
        " -style Radio.Toolbutton -value " & tab.toLowerAscii & " -variable newtab -command ShowRecruitTab")
    tclEval(script = "grid " & tabButton & " -column " & $index & " -row 0")
    tclEval(script = "bind " & tabButton & " <Escape> {" & recruitDialog & ".buttonbox2.button invoke;break}")
  tclEval(script = "bind " & tabButton & " <Tab> {focus " & recruitDialog & ".buttonbox2.hirebutton;break}")
  tclEval(script = "grid " & frame & " -pady {5 0} -columnspan 2")
  let
    recruitCanvas: string = recruitDialog & ".canvas"
    yScroll: string = recruitDialog & ".yscroll"
  tclEval(script = "ttk::scrollbar " & yScroll &
      " -orient vertical -command [list " & recruitCanvas & " yview]")
  tclEval(script = "canvas " & recruitCanvas & " -yscrollcommand [list " &
      yScroll & " set]")
  tclEval(script = "grid " & recruitCanvas & " -sticky nwes -pady 5 -padx 5")
  tclEval(script = "grid " & yScroll & " -sticky ns -pady 5 -padx {0 5} -row 1 -column 1")
  frame = recruitDialog & ".buttonbox2"
  tclEval(script = "ttk::frame " & frame)
  let
    button: string = recruitDialog & ".buttonbox2.hirebutton"
    dialogCloseButton: string = recruitDialog & ".buttonbox2.button"
  tclEval(script = "ttk::button " & button &
      " -text Negotiate -command {CloseDialog " & recruitDialog & ";Negotiate} -image negotiateicon -style Dialog.TButton")
  tclEval(script = "grid " & button)
  tclEval(script = "bind " & button & " <Escape> {" & dialogCloseButton & " invoke;break}")
  tclEval(script = "tooltip::tooltip " & button & " \"Start hiring negotiating.\"")
  tclEval(script = "ttk::button " & dialogCloseButton &
      " -text Close -command {CloseDialog " & recruitDialog & "} -image exiticon -style Dialog.TButton")
  tclEval(script = "grid " & dialogCloseButton & " -row 0 -column 1")
  tclEval(script = "tooltip::tooltip " & dialogCloseButton & " \"Close dialog \\[Escape key\\]\"")
  tclEval(script = "grid " & frame & "  -pady {0 5}")
  tclEval(script = "focus " & dialogCloseButton)
  tclEval(script = "::autoscroll::autoscroll " & yScroll)
  # General info about the selected recruit
  frame = recruitCanvas & ".general"
  tclEval(script = "ttk::frame " & frame)
  var recruitText: string = frame & ".label"
  tclEval(script = "text " & recruitText & " -height 3 -width 30")
  tclEval(script = recruitText & " tag configure gold -foreground " & tclGetVar(
      varName = "ttk::theme::" & gameSettings.interfaceTheme &
      "::colors(-goldenyellow)"))
  let faction: FactionData = try:
      factionsList[recruit.faction]
    except:
      return showError(message = "Can't get the recruit's faction.")
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
    let progressFrame: string = frame & ".statinfo" & $(index + 1)
    tclEval(script = "ttk::frame " & progressFrame)
    var recruitLabel: string = progressFrame & ".label"
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
    let infoButton: string = progressFrame & ".button"
    tclEval(script = "ttk::button " & infoButton &
        " -image helpicon -style Header.Toolbutton -command {ShowCrewStatsInfo " &
        $(index + 1) & " .recruitdialog}")
    tclEval(script = "tooltip::tooltip " & infoButton & " \"Show detailed information about the selected attribute.\"")
    tclEval(script = "grid " & infoButton & " -column 2 -row 0")
    tclEval(script = "grid " & progressFrame & " -sticky we -padx 5 -pady {5 0}")
    tclEval(script = "update")
    let progressBar: string = frame & ".level" & $(index + 1)
    tclEval(script = "ttk::progressbar " & progressBar & " -value " & $(
        attrib.level * 2) & " -length 360")
    tclEval(script = "tooltip::tooltip " & progressBar & " \"The current level of the attribute.\"")
    tclEval(script = "grid " & progressBar)
  # Skills of the selected recruit
  frame = recruitCanvas & ".skills"
  tclEval(script = "ttk::frame " & frame)
  for index, skill in recruit.skills:
    let progressFrame: string = frame & ".skillinfo" & $(index + 1)
    tclEval(script = "ttk::frame " & progressFrame)
    var recruitLabel: string = progressFrame & ".label"
    try:
      tclEval(script = "ttk::label " & recruitLabel & " -text {" & skillsList[
          skill.index].name & ": }")
    except:
      return showError(message = "Can't get skill name.")
    tclEval(script = "grid " & recruitLabel & " -sticky w")
    recruitLabel = progressFrame & ".label2"
    tclEval(script = "ttk::label " & recruitLabel & " -text {" &
        getSkillLevelName(skillLevel = skill.level) & "} -style Golden.TLabel")
    tclEval(script = "grid " & recruitLabel & " -sticky we -column 1 -row 0 -padx {5 0}")
    tclEval(script = "grid columnconfigure " & progressFrame & " " &
        recruitLabel & " -weight 1")
    tclEval(script = "grid rowconfigure " & progressFrame & " " & recruitLabel & " -weight 1")
    var toolQuality: int = 100
    try:
      for quality in skillsList[skill.index].toolsQuality:
        if skill.level <= quality.level:
          toolQuality = quality.quality
          break
    except:
      return showError(message = "Can't get tools for skill.")
    let infoButton: string = progressFrame & ".button"
    tclEval(script = "ttk::button " & infoButton &
        " -image helpicon -style Header.Toolbutton -command {ShowCrewSkillInfo " &
        $(skill.index) & " " & $toolQuality & "  .recruitdialog}")
    tclEval(script = "tooltip::tooltip " & infoButton & " \"Show detailed information about the selected skill.\"")
    tclEval(script = "grid " & infoButton & " -column 2 -row 0")
    tclEval(script = "grid " & progressFrame & " -sticky we")
    let progressBar: string = frame & ".level" & $(index + 1)
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
      try:
        tclEval(script = recruitText & " insert end {" & itemsList[
            recruit.inventory[item]].name & "\n} [list gold]")
      except:
        return showError(message = "Can't get the recruit's equipment.")
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

proc negotiateHireCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Update information about hiring of the selected recruit
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## NegotiateHire
  let
    dailyPayment: Natural = try:
        tclGetVar(varName = "daily").parseFloat.Natural
      except:
        return showError(message = "Can't get daily payment.")
    tradePayment: Natural = try:
        tclGetVar(varName = "percent").parseFloat.Natural
      except:
        return showError(message = "Can't get trade payment.")
  tclSetVar(varName = "daily", newValue = $dailyPayment)
  tclSetVar(varName = "percent", newValue = $tradePayment)
  let
    baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
        playerShip.skyY].baseIndex
    recruit: RecruitData = skyBases[baseIndex].recruits[recruitIndex]
  var newCost: int = recruit.price - ((dailyPayment - recruit.payment) * 50) -
      (tradePayment * 5_000)
  const dialogName: string = ".negotiatedialog"
  let
    contractBox: string = dialogName & ".contract"
    contractLength: int = try:
        tclEval2(script = contractBox & " current").parseInt
      except:
        return showError(message = "Can't get contract length.")
  newCost = case contractLength
    of 1:
      newCost - (recruit.price.float * 0.1).int
    of 2:
      newCost - (recruit.price.float * 0.5).int
    of 3:
      newCost - (recruit.price.float * 0.75).int
    of 4:
      newCost - (recruit.price.float * 0.9).int
    else:
      newCost
  if newCost < 1:
    newCost = 1
  var cost: Natural = newCost
  try:
    countPrice(price = cost, traderIndex = findMember(order = talk))
  except:
    return showError(message = "Can't count price.")
  let moneyInfo: string = dialogName & ".cost"
  tclEval(script = moneyInfo & " configure -state normal")
  tclEval(script = moneyInfo & " delete 2.0 end")
  tclEval(script = moneyInfo & " insert end {\nHire for }")
  tclEval(script = moneyInfo & " insert end {" & $cost & "} [list gold]")
  tclEval(script = moneyInfo & " insert end { " & moneyName & "}")
  tclEval(script = moneyInfo & " configure -state disabled")
  let
    moneyAmount: Natural = moneyAmount(inventory = playerShip.cargo)
    hireButton: string = dialogName & ".buttonbox.hirebutton"
  if moneyAmount < cost:
    tclEval(script = hireButton & " configure -state disabled")
  else:
    tclEval(script = hireButton & " configure -state !disabled")
  return tclOk

proc hireCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
        WriteIOEffect, TimeEffect, RootEffect, RootEffect], cdecl,
            contractual.} =
  ## Hire the selected recruit
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## Hire
  const dialogName: string = ".negotiatedialog"
  var scale: string = dialogName & ".percent"
  let tradePayment: Natural = try:
      tclEval2(script = scale & " cget -value").parseFloat.Natural
    except:
      return showError(message = "Can't get trade payment.")
  scale = dialogName & ".daily"
  let dailyPayment: Natural = try:
      tclEval2(script = scale & " cget -value").parseFloat.Natural
    except:
      return showError(message = "Can't get daily payment")
  let
    baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
        playerShip.skyY].baseIndex
    recruit: RecruitData = skyBases[baseIndex].recruits[recruitIndex]
    contractBox: string = dialogName & ".contract"
    contractLength: int = try:
        tclEval2(script = contractBox & " current").parseInt
      except:
        return showError(message = "Can't get contract length.")
  var
    newCost: int = recruit.price - ((dailyPayment - recruit.payment) * 50) -
      (tradePayment * 5_000)
    contractLength2: int = 0
  case contractLength
  of 1:
    newCost -= (recruit.price.float * 0.1).int
    contractLength2 = 100
  of 2:
    newCost -= (recruit.price.float * 0.5).int
    contractLength2 = 30
  of 3:
    newCost -= (recruit.price.float * 0.75).int
    contractLength2 = 20
  of 4:
    newCost -= (recruit.price.float * 0.9).int
    contractLength2 = 10
  else:
    contractLength2 = -1
  if newCost < 1:
    newCost = 1
  try:
    hireRecruit(recruitIndex = recruitIndex, cost = newCost,
        dailyPayment = dailyPayment, tradePayment = tradePayment,
        contractLength = contractLength2)
  except NoTraderError:
    showMessage(text = "You don't have a trader to hire the recruit.",
        title = "Can't hire the recruit.")
    return tclOk
  except:
    return showError(message = "Can't hire the recruit.")
  updateMessages()
  updateHeader()
  tclEval(script = "CloseDialog " & dialogName)
  return showRecruitCommand(clientData = clientData, interp = interp, argc = 2,
      argv = @["ShowRecruit", "1"].allocCStringArray)

proc showRecruitTabCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], cdecl,
        contractual.} =
  ## Show the selected information about the selected recruit
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowRecruitTab
  const recruitCanvas: string = ".recruitdialog.canvas"
  tclEval(script = recruitCanvas & " delete info")
  let frame: string = recruitCanvas & "." & tclGetVar(varName = "newtab")
  tclEval(script = recruitCanvas & " create window 32 0 -anchor nw -window " &
      frame & " -tag info")
  tclEval(script = "update")
  tclEval(script = recruitCanvas & " configure -scrollregion [list " & tclEval2(
      script = recruitCanvas & " bbox all") & "]")
  return tclOk

proc negotiateCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Show negotation UI to the player
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## Negotiate
  let
    baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
        playerShip.skyY].baseIndex
    recruit: RecruitData = skyBases[baseIndex].recruits[recruitIndex]
    negotiateDialog: string = createDialog(name = ".negotiatedialog",
        title = "Negotiate with " & recruit.name)
  var labelFrame: string = negotiateDialog & ".dailylbl"
  tclEval(script = "ttk::frame " & labelFrame)
  var label: string = labelFrame & ".label"
  tclEval(script = "ttk::label " & label & " -text {Daily payment:}")
  tclEval(script = "grid " & label & " -pady {5 0}")
  tclSetVar(varName = "daily", newValue = $recruit.payment)
  var spinBox: string = labelFrame & ".field"
  tclEval(script = "ttk::spinbox " & spinBox & " -from 0 -to " & $(
      recruit.payment * 2) &
      " -width 5 -textvariable daily -validate key -validatecommand {ValidateNegotiate %W %P} -command {ValidateNegotiate " &
      labelFrame & ".field}")
  let frame: string = negotiateDialog & ".buttonbox"
  tclEval(script = "ttk::frame " & frame)
  let dialogCloseButton: string = negotiateDialog & ".buttonbox.button"
  tclEval(script = "ttk::button " & dialogCloseButton &
      " -text Close -command {CloseDialog " & negotiateDialog & "} -image cancelicon -style Dialogred.TButton")
  tclEval(script = "bind " & spinBox & " <Escape> {" & dialogCloseButton & " invoke;break}")
  tclEval(script = "grid " & spinBox & " -row 0 -column 1")
  tclEval(script = "grid " & labelFrame)
  var scale: string = negotiateDialog & ".daily"
  tclEval(script = "ttk::scale " & scale &
      " -from 0 -command NegotiateHire -length 250 -to " & $(recruit.payment *
      2) & " -variable daily")
  tclEval(script = "bind " & scale & " <Escape> {" & dialogCloseButton & " invoke;break}")
  tclEval(script = "bind " & spinBox & " <Tab> {focus " & scale & ";break}")
  tclEval(script = "grid " & scale)
  labelFrame = negotiateDialog & ".percentlbl"
  tclEval(script = "ttk::frame " & labelFrame)
  label = labelFrame & ".label"
  tclEval(script = "ttk::label " & label & " -text {Percent of profit from trades:}")
  tclEval(script = "grid " & label & " -padx {5 0}")
  tclSetVar(varName = "percent", newValue = "0")
  spinBox = labelFrame & ".field"
  tclEval(script = "ttk::spinbox " & spinBox &
      " -from 0 -to 10 -width 2 -textvariable percent -validate key -validatecommand {ValidateNegotiate %W %P} -command {ValidateNegotiate " &
      labelFrame & ".field}")
  tclEval(script = "bind " & spinBox & " <Escape> {" & dialogCloseButton & " invoke;break}")
  tclEval(script = "grid " & spinBox & " -row 0 -column 1 -padx {0 5}")
  tclEval(script = "grid " & labelFrame & " -padx 5")
  scale = negotiateDialog & ".percent"
  tclEval(script = "ttk::scale " & scale & " -from 0 -to 10 -command NegotiateHire -length 250 -variable percent")
  tclEval(script = "grid " & scale)
  label = negotiateDialog & ".contractlbl"
  tclEval(script = "ttk::label " & label & " -text {Contract time:}")
  tclEval(script = "grid " & label)
  let contractBox: string = negotiateDialog & ".contract"
  tclEval(script = "ttk::combobox " & contractBox & " -state readonly -values [list {Pernament} {100 days} {30 days} {20 days} {10 days}]")
  tclEval(script = "grid " & contractBox)
  tclEval(script = "bind " & contractBox & " <<ComboboxSelected>> {NegotiateHire}")
  tclEval(script = "bind " & scale & " <Tab> {focus " & contractBox & ";break}")
  tclEval(script = "bind " & scale & " <Escape> {" & negotiateDialog & ".buttonbox.button invoke;break}")
  tclEval(script = contractBox & " current 0")
  let hireButton: string = frame & ".hirebutton"
  tclEval(script = "ttk::button " & hireButton & " -text Hire -command {Hire} -image negotiate2icon -style Dialoggreen.TButton")
  tclEval(script = "tooltip::tooltip " & hireButton & " \"Hire the selected recruit.\"")
  tclEval(script = "bind " & contractBox & " <Tab> {focus " & hireButton & ";break}")
  tclEval(script = "bind " & contractBox & " <Escape> {" & negotiateDialog & ".buttonbox.button invoke;break}")
  let moneyInfo: string = negotiateDialog & ".cost"
  tclEval(script = "text " & moneyInfo & " -height 2 -width 22 -wrap char")
  tclEval(script = "grid " & moneyInfo)
  var cost: Natural = recruit.price
  try:
    countPrice(price = cost, traderIndex = findMember(order = talk))
  except:
    return showError(message = "Can't count hire cost.")
  tclEval(script = moneyInfo & " tag configure red -foreground " & tclGetVar(
      varName = "ttk::theme::" & gameSettings.interfaceTheme &
      "::colors(-red)"))
  tclEval(script = moneyInfo & " tag configure gold -foreground " & tclGetVar(
      varName = "ttk::theme::" & gameSettings.interfaceTheme &
      "::colors(-goldenyellow)"))
  let moneyAmount: Natural = moneyAmount(inventory = playerShip.cargo)
  if moneyAmount > 0:
    tclEval(script = moneyInfo & " insert end {You have }")
    tclEval(script = moneyInfo & " insert end {" & $moneyAmount & "} [list gold]")
    tclEval(script = moneyInfo & " insert end { " & moneyName & "}")
    if moneyAmount < cost:
      tclEval(script = hireButton & " configure -state disabled")
    else:
      tclEval(script = hireButton & " configure -state !disabled")
  else:
    tclEval(script = moneyInfo & " insert end {You don't have enough money to recruit anyone} [list red]")
    tclEval(script = hireButton & " configure -state disabled")
  tclEval(script = moneyInfo & " insert end {\nHire for }")
  tclEval(script = moneyInfo & " insert end {" & $cost & "} [list gold]")
  tclEval(script = moneyInfo & " insert end { " & moneyName & "}")
  tclEval(script = moneyInfo & " configure -state disabled")
  tclEval(script = "grid " & hireButton)
  tclEval(script = "grid " & dialogCloseButton & " -row 0 -column 1")
  tclEval(script = "tooltip::tooltip " & dialogCloseButton & " \"Cancel negotiation \\[Escape key\\]\"")
  tclEval(script = "grid " & frame & " -pady {0 5}")
  tclEval(script = "focus " & dialogCloseButton)
  tclEval(script = "bind " & dialogCloseButton & " <Tab> {focus " &
      negotiateDialog & ".dailylbl.field;break}")
  tclEval(script = "bind " & hireButton & " <Tab> {focus " & dialogCloseButton & ";break}")
  tclEval(script = "bind " & negotiateDialog & " <Escape> {" &
      dialogCloseButton & " invoke;break}")
  tclEval(script = "bind " & dialogCloseButton & " <Escape> {" &
      dialogCloseButton & " invoke;break}")
  tclEval(script = "bind " & hireButton & " <Escape> {" &
      dialogCloseButton & " invoke;break}")
  showDialog(dialog = negotiateDialog, relativeY = 0.2)
  return tclOk

type RecruitsSortOrders = enum
  none, nameAsc, nameDesc, genderAsc, genderDesc, factionAsc, factionDesc,
    priceAsc, priceDesc, attributeAsc, attributeDesc, skillAsc, skillDesc

const defaultRecruitsSortOrder: RecruitsSortOrders = none

var recruitsSortOrder: RecruitsSortOrders = defaultRecruitsSortOrder

proc sortRecruitsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    RootEffect], cdecl, contractual.} =
  ## Sort the list of available recruits in base
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SortRecruits x
  ## X is X axis coordinate where the player clicked the mouse button
  let column: Natural = try:
        (if argv[1] == "-1": Positive.high else: getColumnNumber(
            table = recruitTable, xPosition = ($argv[1]).parseInt))
      except:
        return showError(message = "Can't get the column number.")
  case column
  of 1:
    if recruitsSortOrder == nameAsc:
      recruitsSortOrder = nameDesc
    else:
      recruitsSortOrder = nameAsc
  of 2:
    if recruitsSortOrder == genderAsc:
      recruitsSortOrder = genderDesc
    else:
      recruitsSortOrder = genderAsc
  of 3:
    if recruitsSortOrder == factionAsc:
      recruitsSortOrder = factionDesc
    else:
      recruitsSortOrder = factionAsc
  of 4:
    if recruitsSortOrder == priceAsc:
      recruitsSortOrder = priceDesc
    else:
      recruitsSortOrder = priceAsc
  of 5:
    if recruitsSortOrder == attributeAsc:
      recruitsSortOrder = attributeDesc
    else:
      recruitsSortOrder = attributeAsc
  of 6:
    if recruitsSortOrder == skillAsc:
      recruitsSortOrder = skillDesc
    else:
      recruitsSortOrder = skillAsc
  else:
    discard
  if recruitsSortOrder == none:
    return tclOk
  type LocalRecruitData = object
    name: string
    gender: char
    faction: string
    price: Positive = 1
    attribute: string
    skill: string
    id: Natural = 0
  let baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
      playerShip.skyY].baseIndex
  var localRecruits: seq[LocalRecruitData] = @[]
  for index, recruit in skyBases[baseIndex].recruits:
    localRecruits.add(y = LocalRecruitData(name: recruit.name,
        gender: recruit.gender, faction: recruit.faction, price: recruit.price,
        attribute: getHighestAttribute(baseIndex = baseIndex,
        memberIndex = index), skill: getHighestSkill(baseIndex = baseIndex,
        memberIndex = index), id: index))

  proc sortRecruits(x, y: LocalRecruitData): int {.raises: [], tags: [],
      contractual.} =
    ## Compare two recruits and return which should go first, based on the sort
    ## order of the recruits
    ##
    ## * x - the first recruit to compare
    ## * y - the second recruit to compare
    ##
    ## Returns 1 if the first recruit should go first, -1 if the second recruit
    ## should go first.
    case recruitsSortOrder
    of nameAsc:
      if x.name < y.name:
        return 1
      return -1
    of nameDesc:
      if x.name > y.name:
        return 1
      return -1
    of genderAsc:
      if x.gender < y.gender:
        return 1
      return -1
    of genderDesc:
      if x.gender > y.gender:
        return 1
      return -1
    of factionAsc:
      if x.faction < y.faction:
        return 1
      return -1
    of factionDesc:
      if x.faction > y.faction:
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
    of attributeAsc:
      if x.attribute < y.attribute:
        return 1
      return -1
    of attributeDesc:
      if x.attribute > y.attribute:
        return 1
      return -1
    of skillAsc:
      if x.skill < y.skill:
        return 1
      return -1
    of skillDesc:
      if x.skill > y.skill:
        return 1
      return -1
    of none:
      return -1

  localRecruits.sort(cmp = sortRecruits)
  recruitsIndexes = @[]
  for recruit in localRecruits:
    recruitsIndexes.add(y = recruit.id)
  return showRecruitCommand(clientData = clientData, interp = interp, argc = 2,
      argv = @["ShowRecruit", "1"].allocCStringArray)

proc validateNegotiateCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], cdecl,
        contractual.} =
  ## Validate value of numeric fields in negotiate dialog
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ValidateNegotiate field value
  ## Field is Tcl path to the field which will be validated, value is
  ## the new value of the field to validate
  let
    spinBox: string = $argv[1]
    value: string = (if argc == 3: $argv[2] else: tclEval2(script = spinBox & " get"))
  if value.len == 0:
    tclSetResult(value = "1")
    return tclOk
  if tclEval2(script = "ValidateSpinbox " & spinBox & " " & value & " {}") == "0":
    return tclOk
  tclEval(script = "NegotiateHire")
  tclSetResult(value = "1")
  return tclOk

proc addCommands*() {.raises: [], tags: [WriteIOEffect, TimeEffect, RootEffect],
    contractual.} =
  ## Adds Tcl commands related to the trades UI
  try:
    addCommand(name = "ShowRecruit", nimProc = showRecruitCommand)
    addCommand(name = "ShowRecruitInfo", nimProc = showRecruitInfoCommand)
    addCommand(name = "NegotiateHire", nimProc = negotiateHireCommand)
    addCommand(name = "Hire", nimProc = hireCommand)
    addCommand(name = "ShowRecruitTab", nimProc = showRecruitTabCommand)
    addCommand(name = "Negotiate", nimProc = negotiateCommand)
    addCommand(name = "SortRecruits", nimProc = sortRecruitsCommand)
    addCommand(name = "ValidateNegotiate", nimProc = validateNegotiateCommand)
  except:
    showError(message = "Can't add a Tcl command.")
