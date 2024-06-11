# Copyright 2023-2024 Bartek thindil Jasicki
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

import std/[algorithm, strutils, tables]
import ../[config, crew, crewinventory, game, messages, shipscrew, shipscrew2, tk, types]
import coreui, dialogs, shipsuicrewinventory, table, updateheader, utilsui2

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

proc getHighestSkill(memberIndex: Natural): string {.sideEffect, raises: [],
    tags: [].} =
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
  try:
    return skillsList[highestIndex].name
  except KeyError:
    showError(message = "Can't thge the highest skill. Index: " & $highestIndex )
    return "Unknown"

proc updateCrewInfo*(page: Positive = 1; skill: Natural = 0) {.sideEffect,
    raises: [], tags: [RootEffect].} =
  ## Update the list of the player's ship's crew members
  ##
  ## * page  - the current page of the list to show
  ## * skill - the currently selected skill on the list of skills to show
  let
    crewInfoFrame = mainPaned & ".shipinfoframe.crew.canvas.frame"
    gridSize = tclEval2(script = "grid size " & crewInfoFrame).split(' ')
    rows = try:
        gridSize[1].parseInt
      except ValueError:
        showError(message = "Can't get the size of the grid. Result: " & $gridSize)
        return
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
  tclEval(script = "ttk::frame " & buttonsFrame)
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
  tclEval(script = "ttk::frame " & buttonsFrame)
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
      try:
        addButton(table = crewTable, text = getSkillLevelName(
            skillLevel = getSkillLevel(member = playerShip.crew[mIndex],
            skillIndex = findSkillIndex(skillName = tclEval2(script = skillBox &
                " get")))),
            tooltip = "The level of " & tclEval2(
            script = skillBox & " get") & " of the selected crew member",
            command = "ShowMemberInfo " & $(mIndex + 1), column = 4)
      except KeyError:
        showError(message = "Can't get the level of the skill.")
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

proc orderForAllCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [RootEffect], exportc.} =
  ## Set the selected order for the whole crew or only to the selected crew
  ## members if any is selected
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## OrderForAll order
  ## Order is the name of the order which will be assigned to the whole
  ## player ship crew or to the selected crew members
  if hasSelection():
    for i in playerShip.crew.low .. playerShip.crew.high:
      if tclGetVar(varName = "crewindex" & $(i + 1)) == "1":
        try:
          giveOrders(ship = playerShip, memberIndex = i, givenOrder = parseEnum[
              CrewOrders](s = ($argv[1]).toLowerAscii))
        except CrewOrderError:
          addMessage(message = getCurrentExceptionMsg(), mType = orderMessage)
          updateHeader()
          updateMessages()
          return tclOk
        except:
          return showError(message = "Can't give orders.")
  else:
    for i in playerShip.crew.low .. playerShip.crew.high:
      try:
        giveOrders(ship = playerShip, memberIndex = i, givenOrder = parseEnum[
            CrewOrders](s = ($argv[1]).toLowerAscii))
      except CrewOrderError:
        addMessage(message = getCurrentExceptionMsg(), mType = orderMessage)
        updateHeader()
        updateMessages()
        return tclOk
      except:
        return showError(message = "Can't give orders.")
  updateHeader()
  updateMessages()
  updateCrewInfo()
  return tclOk

proc toggleCrewMemberCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Select or deselect the selected crew member
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ToggleCrewMember rowindex crewindex
  ## Rowindex is the index of the row in which is the selected crew member,
  ## crewindex is the index of the selected crew member.
  let row = try:
      ($argv[1]).parseInt
    except:
      return showError(message = "Can't get the row.")
  toggleCheckedButton(table = crewTable, row = row, column = 1)
  if isChecked(table = crewTable, row = row, column = 1):
    tclSetVar(varName = "crewindex" & $argv[2], newValue = "1")
  else:
    tclUnsetVar(varName = "crewindex" & $argv[2])
  updateTooltips()
  return tclOk

proc dismissCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Dismiss the selected crew member
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## Dismiss memberindex
  ## Memberindex is the index of the player ship crew member which will be
  ## dismissed
  let memberIndex = try:
      ($argv[1]).parseInt
    except:
      return showError(message = "Can't get the crew member index.")
  showQuestion(question = "Are you sure want to dismiss " & playerShip.crew[
      memberIndex - 1].name & "?", res = $argv[1])
  return tclOk

proc setCrewOrderCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [RootEffect], exportc.} =
  ## Set order for the selected crew member
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SetCrewOrder order memberindex ?moduleindex?
  ## Order is an index for the order which will be set, memberindex is an
  ## index of the member in the player ship crew which will be have order set
  ## and optional parameter moduleindex is index of module in player ship
  ## which will be assigned to the crew member
  var moduleIndex = -1
  if argc == 4:
    moduleIndex = try:
        ($argv[3]).parseInt - 1
      except:
        return showError(message = "Can't get the module index.")
  try:
    giveOrders(ship = playerShip, memberIndex = ($argv[2]).parseInt - 1,
        givenOrder = parseEnum[CrewOrders](s = ($argv[1]).toLowerAscii),
        moduleIndex = moduleIndex)
  except CrewOrderError, CrewNoSpaceError:
    addMessage(message = getCurrentExceptionMsg(), mType = orderMessage, color = red)
    updateMessages()
    return tclOk
  except:
    return showError(message = "Can't give order.")
  updateHeader()
  updateMessages()
  updateCrewInfo()
  return tclOk

proc showMemberTabCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Show the selected information about the selected crew member
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowMemberTab
  let memberCanvas = ".memberdialog.canvas"
  tclEval(script = memberCanvas & " delete info")
  let
    tabName = tclGetVar(varName = "newtab")
    frame = memberCanvas & "." & tabName
  tclEval(script = memberCanvas & " create window 32 0 -anchor nw -window " &
      frame & " -tag info")
  tclEval(script = "update")
  tclEval(script = memberCanvas & " configure -scrollregion [list " & tclEval2(
      script = memberCanvas & " bbox all") & "]")
  var tabButton = ".memberdialog.buttonbox.priorities"
  if tclEval2(script = "winfo ismapped " & tabButton) == "0":
    tabButton = ".memberdialog.buttonbox.general"
  tclEval(script = "bind " & tabButton & " <Tab> {}")
  if tabName == "general":
    tclEval(script = "bind " & tabButton & " <Tab> {focus .memberdialog.canvas.general.nameinfo.button;break}")
  elif tabName == "stats":
    tclEval(script = "bind " & tabButton & " <Tab> {focus .memberdialog.canvas.stats.statinfo1.button;break}")
  elif tabName == "skills":
    tclEval(script = "bind " & tabButton & " <Tab> {focus .memberdialog.canvas.skills.skillinfo1.button;break}")
  elif tabName == "priorities":
    tclEval(script = "bind " & tabButton & " <Tab> {focus .memberdialog.canvas.priorities.level1.button;break}")
  return tclOk

proc showMemberInfoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Show information about the selected crew member
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowMemberInfo memberindex
  ## MemberIndex is the index of the crew member to show
  let
    memberIndex = try:
        ($argv[1]).parseInt - 1
      except:
        return showError(message = "Can't get the member index.")
    member = playerShip.crew[memberIndex]
    memberDialog = createDialog(name = ".memberdialog", title = member.name &
        "'s details", columns = 2)
    yScroll = memberDialog & ".yscroll"
  tclEval(script = "ttk::scrollbar " & yScroll & " -orient vertical -command [list .memberdialog.canvas yview]")
  tclEval(script = "SetScrollbarBindings " & memberDialog & " " & yScroll)
  let memberCanvas = memberDialog & ".canvas"
  tclEval(script = "canvas " & memberCanvas & " -yscrollcommand [list " &
      yScroll & " set]")
  tclEval(script = "SetScrollbarBindings " & memberCanvas & " " & yScroll)
  var frame = memberDialog & ".buttonbox"
  tclEval(script = "ttk::frame " & frame)
  tclSetVar(varName = "newtab", newValue = "general")
  var tabButton = frame & ".general"
  tclEval(script = "ttk::radiobutton " & tabButton & " -text General -stat selected -style Radio.Toolbutton -value general -variable newtab -command ShowMemberTab")
  tclEval(script = "grid " & tabButton)
  let buttonsFrame = memberDialog & ".buttons"
  tclEval(script = "ttk::frame " & buttonsFrame)
  let closeButton = buttonsFrame & ".button"
  tclEval(script = "bind " & tabButton & " <Escape> {" & closeButton & " invoke;break}")
  if member.skills.len > 0 and member.contractLength != 0:
    tabButton = frame & ".stats"
    tclEval(script = "ttk::radiobutton " & tabButton & " -text Attributes -style Radio.Toolbutton -value stats -variable newtab -command ShowMemberTab")
    tclEval(script = "grid " & tabButton & " -column 1 -row 0")
    tclEval(script = "bind " & tabButton & " <Escape> {" & closeButton & " invoke;break}")
    tabButton = frame & ".skills"
    tclEval(script = "ttk::radiobutton " & tabButton & " -text Skills -style Radio.Toolbutton -value skills -variable newtab -command ShowMemberTab")
    tclEval(script = "grid " & tabButton & " -column 2 -row 0")
    tclEval(script = "bind " & tabButton & " <Escape> {" & closeButton & " invoke;break}")
    tabButton = frame & ".priorities"
    tclEval(script = "ttk::radiobutton " & tabButton & " -text Priorirites -style Radio.Toolbutton -value priorities -variable newtab -command ShowMemberTab")
    tclEval(script = "grid " & tabButton & " -column 3 -row 0")
    tclEval(script = "bind " & tabButton & " <Escape> {" & closeButton & " invoke;break}")
  else:
    tclEval(script = "bind " & tabButton & " <Tab> {focus " & closeButton & ";break}")
  tclEval(script = "grid " & frame & " -pady {5 0} -columnspan 2")
  tclEval(script = "grid " & memberCanvas & " -sticky nwes -pady 5 -padx 5")
  tclEval(script = "grid " & yScroll & " -sticky ns -pady 5 -padx {0 5} -row 2 -column 1")
  var button = buttonsFrame & ".button1"
  tclEval(script = "ttk::button " & button &
      " -text {Inventory} -image {inventoryicon} -command {" & closeButton &
      " invoke;ShowMemberInventory " & $argv[1] & "} -style Dialog.TButton")
  tclEval(script = "tooltip::tooltip " & button & " \"Show the crew member inventory\"")
  tclEval(script = "grid " & button & " -padx 5")
  tclEval(script = "bind " & button & " <Tab> {focus " & closeButton & ";break}")
  tclEval(script = "bind " & button & " <Escape> {" & closeButton & " invoke;break}")
  addCloseButton(name = buttonsFrame & ".button", text = "Close",
      command = "CloseDialog " & memberDialog, row = 0, column = 1)
  if argv[1] != "1" and playerShip.speed == docked:
    button = buttonsFrame & ".button2"
    tclEval(script = "ttk::button " & button &
        " -text {Dismiss} -image {dismissicon} -command {" & closeButton &
        " invoke;Dismiss " & $argv[1] & "} -style Dialog.TButton")
    tclEval(script = "tooltip::tooltip " & button & " \"Remove the crew member from the ship's crew.\"")
    tclEval(script = "grid " & button & " -padx 5 -row 0 -column 2")
    tclEval(script = "bind " & button & " <Tab> {focus " & memberDialog & ".buttonbox.general;break}")
    tclEval(script = "bind " & button & " <Escape> {" & closeButton & " invoke;break}")
  tclEval(script = "::autoscroll::autoscroll " & yScroll)
  # General info about the selected crew member
  frame = memberCanvas & ".general"
  tclEval(script = "ttk::frame " & frame)
  var memberLabel: string

  proc addLabel(name, text: string; text2: string = "") =
    let labelBox = name
    tclEval(script = "ttk::frame " & labelBox & " -width 440")
    memberLabel = labelBox & ".label1"
    tclEval(script = "ttk::label " & memberLabel & " -text {" & text & "} -wraplength 220")
    tclEval(script = "grid " & memberLabel & " -sticky w")
    tclEval(script = "SetScrollbarBindings " & memberLabel & " " & yScroll)
    if text2.len > 0:
      memberLabel = labelBox & ".label2"
      tclEval(script = "ttk::label " & memberLabel & " -text {" & text2 & "} -wraplength 220 -style Golden.TLabel")
      tclEval(script = "grid " & memberLabel & " -row 0 -column 1 -sticky w")
      tclEval(script = "SetScrollbarBindings " & memberLabel & " " & yScroll)
    tclEval(script = "grid " & labelBox & " -sticky w -padx 5")
    tclEval(script = "SetScrollbarBindings " & labelBox & " " & yScroll)

  addLabel(name = frame & ".nameinfo", text = "Name: ", text2 = member.name)
  var infoButton = frame & ".nameinfo.button"
  tclEval(script = "ttk::button " & infoButton & " -image editicon -command {" &
      closeButton & " invoke;GetString {Enter a new name for the " &
      member.name & ":} crewname" & $argv[1] & " {Renaming crew member} {Rename}} -style Small.TButton")
  tclEval(script = "tooltip::tooltip " & infoButton & " \"Set a new name for the crew member\"")
  tclEval(script = "grid " & infoButton & " -row 0 -column 2 -sticky n -padx {5 0}")
  tclEval(script = "bind " & infoButton & " <Escape> {" & closeButton & " invoke;break}")
  tclEval(script = "SetScrollbarBindings " & infoButton & " " & yScroll)
  if member.health < 100:
    if gameSettings.showNumbers:
      addLabel(name = frame & ".health", text = "Health: ",
          text2 = $member.health & "%")
    else:
      case member.health:
      of 81 .. 99:
        addLabel(name = frame & ".health", text = "Health: ",
            text2 = "Slightly wounded")
      of 51 .. 80:
        addLabel(name = frame & ".health", text = "Health: ", text2 = "Wounded")
      of 1 .. 50:
        addLabel(name = frame & ".health", text = "Health: ",
            text2 = "Heavily wounded")
      else:
        discard
  var tiredPoints = member.tired - member.attributes[conditionIndex].level
  if tiredPoints < 0:
    tiredPoints = 0
  if tiredPoints > 0:
    if gameSettings.showNumbers:
      addLabel(name = frame & ".tired", text = "Tiredness: ",
          text2 = $tiredPoints & "%")
    else:
      case tiredPoints
      of 1 .. 40:
        addLabel(name = frame & ".tired", text = "Tiredness: ",
            text2 = "Bit tired")
      of 41 .. 80:
        addLabel(name = frame & ".tired", text = "Tiredness: ", text2 = "Tired")
      of 81 .. 99:
        addLabel(name = frame & ".tired", text = "Tiredness: ",
            text2 = "Very tired")
      of 100:
        addLabel(name = frame & ".tired", text = "Tiredness: ",
            text2 = "Unconscious")
      else:
        discard
  if member.thirst > 0:
    if gameSettings.showNumbers:
      addLabel(name = frame & ".thirst", text = "Thirst: ",
          text2 = $member.thirst & "%")
    else:
      case member.thirst
      of 1 .. 40:
        addLabel(name = frame & ".thirst", text = "Thirst: ",
            text2 = "Bit thirsty")
      of 41 .. 80:
        addLabel(name = frame & ".thirst", text = "Thirst: ",
            text2 = "Thirsty")
      of 81 .. 99:
        addLabel(name = frame & ".thirst", text = "Thirst: ",
            text2 = "Very thirsty")
      of 100:
        addLabel(name = frame & ".thirst", text = "Thirst: ",
            text2 = "Dehydrated")
      else:
        discard
  if member.hunger > 0:
    if gameSettings.showNumbers:
      addLabel(name = frame & ".hunger", text = "Hunger: ",
          text2 = $member.thirst & "%")
    else:
      case member.hunger
      of 1 .. 40:
        addLabel(name = frame & ".hunger", text = "Hunger: ",
            text2 = "Bit hungry")
      of 41 .. 80:
        addLabel(name = frame & ".hunger", text = "Hunger: ",
            text2 = "Hungry")
      of 81 .. 99:
        addLabel(name = frame & ".hunger", text = "Hunger: ",
            text2 = "Very hungry")
      of 100:
        addLabel(name = frame & ".hunger", text = "Hungre: ",
            text2 = "Starving")
      else:
        discard
  if member.morale[1] != 50:
    if gameSettings.showNumbers:
      addLabel(name = frame & ".morale", text = "Morale: ",
          text2 = $member.morale[1] & "%")
    else:
      case member.morale[1]
      of 0 .. 24:
        addLabel(name = frame & ".morale", text = "Morale: ",
            text2 = "Upset")
      of 25 .. 49:
        addLabel(name = frame & ".morale", text = "Morale: ",
            text2 = "Unhappy")
      of 51 .. 74:
        addLabel(name = frame & ".morale", text = "Morale: ",
            text2 = "Happy")
      of 75 .. 100:
        addLabel(name = frame & ".morale", text = "Morale: ",
            text2 = "Excited")
      else:
        discard
  if member.skills.len > 0:
    try:
      addLabel(name = frame & ".orderinfo", text = "Order: ",
          text2 = getCurrentOrder(memberIndex = memberIndex))
    except:
      return showError(message = "Can't show the order label.")
    infoButton = frame & ".orderinfo.button"
    tclEval(script = "ttk::button " & infoButton &
        " -image giveordericon -command {" & closeButton &
        " invoke;ShowCrewOrder " & $(memberIndex + 1) & "} -style Small.TButton")
    tclEval(script = "tooltip::tooltip " & infoButton & " \"Set the new order for the crew member\"")
    tclEval(script = "grid " & infoButton & " -row 0 -column 2 -sticky n -padx {5 0}")
    tclEval(script = "bind " & infoButton & " <Escape> {" & closeButton & " invoke;break}")
    tclEval(script = "SetScrollbarBindings " & infoButton & " " & yScroll)
    tclEval(script = "bind " & infoButton & " <Tab> {focus " & buttonsFrame & ".button1;break}")
    infoButton = frame & ".nameinfo.button"
    tclEval(script = "bind " & infoButton & " <Tab> {focus " & frame & ".orderinfo.button;break}")
  else:
    tclEval(script = "bind " & infoButton & " <Tab> {focus " & closeButton & ";break}")
  let faction = try:
      factionsList[member.faction]
    except:
      return showError(message = "Can't get the crew member's faction.")
  if "nogender" notin faction.flags:
    addLabel(name = frame & ".gender", text = "Gender: ", text2 = (
        if member.gender == 'M': "Male" else: "Female"))
  addLabel(name = frame & ".faction", text = "Faction: ", text2 = faction.name)
  addLabel(name = frame & ".homebase", text = "Home base: ", text2 = skyBases[
      member.homeBase].name)
  if member.skills.len == 0 or member.contractLength == 0:
    addLabel(name = frame & ".passenger", text = "Passenger")
    if member.contractLength > 0:
      var memberInfo = ""
      minutesToDate(minutes = member.contractLength, infoText = memberInfo)
      addLabel(name = frame & ".timelimit", text = "Time limit: ",
          text2 = memberInfo)
  else:
    if memberIndex > 0:
      addLabel(name = frame & ".timelimit", text = "Contract length: ",
          text2 = (if member.contractLength > 0: $member.contractLength &
          " days" else: "pernament"))
      addLabel(name = frame & ".payment", text = "Payment: ",
          text2 = $member.payment[1] & " " & moneyName & " each day" & (
          if member.payment[2] > 0: " and " & $member.payment[2] &
          " percent of profit from each trade" else: ""))
  tclEval(script = "grid " & frame)
  tclEval(script = "SetScrollbarBindings " & frame & " " & yScroll)
  if member.skills.len > 0 and member.contractLength != 0:
    # Statistics of the selected crew member
    frame = memberCanvas & ".stats"
    tclEval(script = "ttk::frame " & frame)
    tclEval(script = "SetScrollbarBindings " & frame & " " & yScroll)
    for index, attrib in member.attributes:
      var progressFrame = frame & ".statinfo" & $index
      tclEval(script = "ttk::frame " & progressFrame)
      var memberLabel = progressFrame & ".label"
      tclEval(script = "ttk::label " & memberLabel & " -text {" &
          attributesList[index].name & ":}")
      tclEval(script = "grid " & memberLabel & " -sticky w")
      tclEval(script = "SetScrollbarBindings " & memberLabel & " " & yScroll)
      memberLabel = progressFrame & ".label2"
      tclEval(script = "ttk::label " & memberLabel & " -text {" &
          getAttributeLevelName(attributeLevel = attrib.level) & "} -style Golden.TLabel")
      tclEval(script = "grid " & memberLabel & " -sticky we -column 1 -row 0 -padx {5 0}")
      tclEval(script = "SetScrollbarBindings " & memberLabel & " " & yScroll)
      tclEval(script = "grid columnconfigure " & progressFrame & " " &
          memberLabel & " -weight 1")
      tclEval(script = "grid rowconfigure " & progressFrame & " " &
          memberLabel & " -weight 1")
      infoButton = progressFrame & ".button"
      tclEval(script = "ttk::button " & infoButton &
          " -image helpicon -style Header.Toolbutton -command {ShowCrewStatsInfo " &
          $(index + 1) & " .memberdialog}")
      tclEval(script = "tooltip::tooltip " & infoButton & " \"Show detailed information about the selected attribute.\"")
      tclEval(script = "grid " & infoButton & " -column 2 -row 0 -padx {5 0}")
      tclEval(script = "bind " & infoButton & " <Escape> {" & closeButton & " invoke;break}")
      if index < member.attributes.high:
        tclEval(script = "bind " & infoButton & " <Tab> {focus " & frame &
            ".statinfo" & $(index + 1) & ";break}")
      else:
        tclEval(script = "bind " & infoButton & " <Tab> {focus " &
            buttonsFrame & ".button1;break}")
      tclEval(script = "SetScrollbarBindings " & infoButton & " " & yScroll)
      tclEval(script = "grid " & progressFrame & " -sticky we -padx 5")
      tclEval(script = "SetScrollbarBindings " & progressFrame & " " & yScroll)
      tclEval(script = "update")
      var progressBar = frame & ".level" & $index
      tclEval(script = "ttk::progressbar " & progressBar & " -value " & $(
          if attrib.level > 2: attrib.level * 2 else: 6))
      tclEval(script = "tooltip::tooltip " & progressBar & " \"The current level of the attribute.\"")
      tclEval(script = "grid " & progressBar & " -sticky w -padx 5")
      tclEval(script = "SetScrollbarBindings " & progressBar & " " & yScroll)
      progressFrame = frame & ".experienceframe" & $index
      tclEval(script = "ttk::frame " & progressFrame & " -height 12")
      tclEval(script = "grid " & progressFrame & " -sticky w -padx 5")
      tclEval(script = "SetScrollbarBindings " & progressFrame & " " & yScroll)
      progressBar = progressFrame & ".experience" & $index
      tclEval(script = "ttk::progressbar " & progressBar & " -value " & $(
          attrib.experience.float / (attrib.level.float * 250.0)) & " -maximum 1.0 -style experience.Horizontal.TProgressbar")
      tclEval(script = "tooltip::tooltip " & progressBar & " \"Experience need to reach the next level\"")
      tclEval(script = "place " & progressBar & " -in " & progressFrame & " -relheight 1.0 -relwidth 1.0")
      tclEval(script = "SetScrollbarBindings " & progressBar & " " & yScroll)
    for index, attrib in member.attributes:
      var progressBar = frame & ".level" & $index
      tclEval(script = progressBar & " configure -length 360")
      let progressFrame = frame & ".experienceframe" & $index
      tclEval(script = progressFrame & " configure -width 360")
      progressBar = progressFrame & ".experience" & $index
      tclEval(script = progressBar & " configure -length 360")
    # Skills of the selected crew member
    frame = memberCanvas & ".skills"
    tclEval(script = "ttk::frame " & frame)
    tclEval(script = "SetScrollbarBindings " & frame & " " & yScroll)
    for index, skill in member.skills:
      var progressFrame = frame & ".skillinfo" & $index
      tclEval(script = "ttk::frame " & progressFrame)
      var memberLabel = progressFrame & ".label" & $index
      try:
        tclEval(script = "ttk::label " & memberLabel & " -text {" & skillsList[
            skill.index].name & ":}")
      except:
        return showError(message = "Can't show the crew member skill name.")
      tclEval(script = "grid " & memberLabel & " -sticky w")
      tclEval(script = "SetScrollbarBindings " & memberLabel & " " & yScroll)
      memberLabel = progressFrame & ".label2" & $index
      tclEval(script = "ttk::label " & memberLabel & " -text {" &
          getSkillLevelName(skillLevel = skill.level) & "} -style Golden.TLabel")
      tclEval(script = "grid " & memberLabel & " -sticky we -column 1 -row 0 -padx {5 0}")
      tclEval(script = "SetScrollbarBindings " & memberLabel & " " & yScroll)
      tclEval(script = "grid columnconfigure " & progressFrame & " " &
          memberLabel & " -weight 1")
      tclEval(script = "grid rowconfigure " & progressFrame & " " &
          memberLabel & " -weight 1")
      infoButton = progressFrame & ".button"
      tclEval(script = "ttk::button " & infoButton &
          " -image helpicon -style Header.Toolbutton -command {ShowCrewSkillInfo " &
          $skill.index & " " & $argv[1] & " .memberdialog}")
      tclEval(script = "tooltip::tooltip " & infoButton & " \"Show detailed information about the selected skill.\"")
      tclEval(script = "grid " & infoButton & " -column 2 -row 0 -padx {5 0}")
      tclEval(script = "bind " & infoButton & " <Escape> {" & closeButton & " invoke;break}")
      tclEval(script = "SetScrollbarBindings " & infoButton & " " & yScroll)
      if index < member.skills.high:
        tclEval(script = "bind " & infoButton & " <Tab> {focus " & frame &
            ".skillinfo" & $(index + 1) & ";break}")
      else:
        tclEval(script = "bind " & infoButton & " <Tab> {focus " &
            buttonsFrame & ".button1;break}")
      tclEval(script = "grid " & progressFrame & " -sticky we -padx 5")
      tclEval(script = "SetScrollbarBindings " & progressFrame & " " & yScroll)
      tclEval(script = "update")
      var progressBar = frame & ".level" & $index
      tclEval(script = "ttk::progressbar " & progressBar & " -value " & $skill.level)
      tclEval(script = "tooltip::tooltip " & progressBar & " \"The current level of the attribute.\"")
      tclEval(script = "grid " & progressBar & " -sticky w -padx 5")
      tclEval(script = "SetScrollbarBindings " & progressBar & " " & yScroll)
      progressFrame = frame & ".experienceframe" & $index
      tclEval(script = "ttk::frame " & progressFrame & " -height 12")
      tclEval(script = "grid " & progressFrame & " -sticky w -padx 5")
      tclEval(script = "SetScrollbarBindings " & progressFrame & " " & yScroll)
      progressBar = progressFrame & ".experience" & $index
      tclEval(script = "ttk::progressbar " & progressBar & " -value " & $(
          skill.experience.float / (skill.level.float * 25.0)) & " -maximum 1.0 -style experience.Horizontal.TProgressbar")
      tclEval(script = "tooltip::tooltip " & progressBar & " \"Experience need to reach the next level\"")
      tclEval(script = "place " & progressBar & " -in " & progressFrame & " -relheight 1.0 -relwidth 1.0")
      tclEval(script = "SetScrollbarBindings " & progressBar & " " & yScroll)
      tclEval(script = "update")
    for index, skill in member.skills:
      var progressBar = frame & ".level" & $index
      tclEval(script = progressBar & " configure -length 360")
      let progressFrame = frame & ".experienceframe" & $index
      tclEval(script = progressFrame & " configure -width 360")
      progressBar = progressFrame & ".experience" & $index
      tclEval(script = progressBar & " configure -length 360")
    # Order priorities of the selected crew member
    frame = memberCanvas & ".priorities"
    tclEval(script = "ttk::frame " & frame)
    tclEval(script = "SetScrollbarBindings " & frame & " " & yScroll)
    var memberLabel = frame & ".label1"
    tclEval(script = "ttk::label " & memberLabel & " -text {Priority}")
    tclEval(script = "grid " & memberLabel)
    tclEval(script = "SetScrollbarBindings " & memberLabel & " " & yScroll)
    memberLabel = frame & ".label2"
    tclEval(script = "ttk::label " & memberLabel & " -text {Level}")
    tclEval(script = "grid " & memberLabel & " -row 0 -column 1")
    tclEval(script = "SetScrollbarBindings " & memberLabel & " " & yScroll)
    const prioritesNames: array[1 .. 12, string] = ["Piloting:", "Engineering:",
        "Operating guns:", "Repair ship:", "Manufacturing:", "Upgrading ship:",
        "Talking in bases:", "Healing wounded:", "Cleaning ship:",
        "Defend ship:", "Board enemy ship", "Train skill:"]
    var comboBox = ""
    for index, order in member.orders:
      var memberLabel = frame & ".name" & $index
      tclEval(script = "ttk::label " & memberLabel & " -text {" &
          prioritesNames[index] & "} -takefocus 0}")
      tclEval(script = "grid " & memberLabel & " -sticky w -padx {5 0}")
      tclEval(script = "SetScrollbarBindings " & memberLabel & " " & yScroll)
      comboBox = frame & ".level" & $index
      tclEval(script = "ttk::combobox " & comboBox & " -values [list None Normal Highest] -state readonly -width 8")
      tclEval(script = comboBox & " current " & $(order.ord))
      tclEval(script = "bind " & comboBox &
          " <<ComboboxSelected>> {SetPriority " & $index & " [" & comboBox &
          " current] " & $(memberIndex + 1) & "}")
      tclEval(script = "grid " & comboBox & " -column 1 -row " & $index & " -padx {0 5}")
      tclEval(script = "bind " & comboBox & " <Escape> {" & closeButton & " invoke;break}")
    tclEval(script = "bind " & comboBox & " <Tab> {focus " & buttonsFrame & ".button1;break}")
  if argv[1] == "1" or playerShip.speed != docked:
    tclEval(script = "bind " & closeButton & " <Tab> {focus " & memberDialog & ".buttonbox.general;break}")
  else:
    tclEval(script = "bind " & closeButton & " <Tab> {focus " & memberDialog & ".buttons.button2;break}")
  tclEval(script = "grid " & buttonsFrame & " -padx 5 -pady 5")
  showDialog(dialog = memberDialog, relativeY = 0.2, relativeX = 0.2)
  return showMemberTabCommand(clientData = clientData, interp = interp,
      argc = 1, argv = @["ShowMemberTab"].allocCStringArray)

proc showCrewStatsInfoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Show the detailed information about the selected crew member statistic
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowCrewStatsInfo statindex
  ## Statindex is the index of statistic which info will be show
  let attribute = try:
      attributesList[($argv[1]).parseInt - 1]
    except:
      return showError(message = "Can't get the attribute data.")
  showInfo(text = attribute.description, parentName = $argv[2],
      title = attribute.name)
  return tclOk

proc showCrewSkillInfoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Show the detailed information about the selected crew member skill
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowCrewSkillInfo skillindex memberindex
  ## Skillindex is the index of skill which info will be show.
  ## Memberindex is the index of the crew member which skill will be show.
  let
    skillIndex = try:
        ($argv[1]).parseInt
      except:
        return showError(message = "Can't get the skill index.")
    skill = try:
        skillsList[skillIndex]
      except:
        return showError(message = "Can't get the skill.")
  var messageText = try:
        "Related attribute: " & attributesList[skill.attribute].name
      except:
        return showError(message = "Can't set messageText.")
  if skill.tool.len > 0:
    messageText.add(y = ".\nTraining tool: ")
    var
      quality = 0
      itemIndex = -1
    if argv[3] == ".memberdialog":
      for index, item in itemsList:
        try:
          if item.itemType == skill.tool and item.value[1] <=
              getTrainingToolQuality(memberIndex = ($argv[2]).parseInt - 1,
              skillIndex = skillIndex):
            if item.value[1] > quality:
              itemIndex = index
              quality = item.value[1]
        except:
          return showError(message = "Can't get member index.")
    else:
      for index, item in itemsList:
        try:
          if item.itemType == skill.tool and item.value[1] <= (
              $argv[2]).parseInt:
            if item.value[1] > quality:
              itemIndex = index
              quality = item.value[1]
        except:
          return showError(message = "Can't get the item value.")
    try:
      messageText.add(itemsList[itemIndex].name)
    except:
      return showError(message = "Can't add the tool name to text.")
  messageText.add(".\n" & skill.description)
  showInfo(text = messageText, parentName = $argv[3], title = skill.name)
  return tclOk

proc setPriorityCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [RootEffect], exportc.} =
  ## Set the selected priority of the selected crew member
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SetPriority orderindex priority memberindex
  ## Orderindex is the index of the order priority which will be changed,
  ## priority is the new level of the priority of the selected order,
  ## memberindex is the index of the crew member which priority order will
  ## be set
  let memberIndex = try:
      ($argv[3]).parseInt - 1
    except:
      return showError(message = "Can't get the crew member's index.")
  if argv[2] == "2":
    for order in playerShip.crew[memberIndex].orders.mitems:
      if order == 2:
        order = 1
        break
  try:
    playerShip.crew[memberIndex].orders[($argv[1]).parseInt] = ($argv[2]).parseInt
  except:
    return showError(message = "Can't set the crew member orders priority.")
  try:
    updateOrders(ship = playerShip)
  except CrewOrderError, CrewNoSpaceError:
    showMessage(text = getCurrentExceptionMsg(),
        title = "Can't give an order")
    return tclOk
  except:
    return showError(message = "Can't update orders.")
  updateHeader()
  updateMessages()
  updateCrewInfo()
  for index, order in playerShip.crew[memberIndex].orders:
    let comboBox = ".memberdialog.canvas.priorities.level" & $index
    tclEval(script = comboBox & " current " & $order)
  return tclOk

proc showCrewCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [RootEffect], exportc.} =
  ## Show the list of the player's ship crew to a player
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowCrew page skill
  ## Page parameter is a index of page from which starts showing
  ## crew. Skill is the index of skill to show
  try:
    updateCrewInfo(page = ($argv[1]).parseInt, skill = ($argv[2]).parseInt)
  except:
    showError(message = "Can't update the crew info.")
  return tclOk

type CrewSortOrders = enum
  selectedAsc, selectedDesc, nameAsc, nameDesc, orderAsc, orderDesc, skillAsc,
    skillDesc, healthAsc, healthDesc, fatigueAsc, fatigueDesc, thirstAsc,
    thirstDesc, hungerAsc, hungerDesc, moraleAsc, moraleDesc, none

const defaultCrewSortOrder: CrewSortOrders = none

var crewSortOrder = defaultCrewSortOrder

proc sortCrewCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [RootEffect], exportc.} =
  ## Sort the player's ship's crew list
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SortShipCrew x
  ## X is X axis coordinate where the player clicked the mouse button
  let column = try:
        (if argv[1] == "-1": Positive.high else: getColumnNumber(
            table = crewTable, xPosition = ($argv[1]).parseInt))
      except:
        return showError(message = "Can't get the column number.")
  case column
  of 1:
    if crewSortOrder == selectedAsc:
      crewSortOrder = selectedDesc
    else:
      crewSortOrder = selectedAsc
  of 2:
    if crewSortOrder == nameAsc:
      crewSortOrder = nameDesc
    else:
      crewSortOrder = nameAsc
  of 3:
    if crewSortOrder == orderAsc:
      crewSortOrder = orderDesc
    else:
      crewSortOrder = orderAsc
  of 4:
    if crewSortOrder == skillAsc:
      crewSortOrder = skillDesc
    else:
      crewSortOrder = skillAsc
  of 5:
    if crewSortOrder == healthAsc:
      crewSortOrder = healthDesc
    else:
      crewSortOrder = healthAsc
  of 6:
    if crewSortOrder == fatigueAsc:
      crewSortOrder = fatigueDesc
    else:
      crewSortOrder = fatigueAsc
  of 7:
    if crewSortOrder == thirstAsc:
      crewSortOrder = thirstDesc
    else:
      crewSortOrder = thirstAsc
  of 8:
    if crewSortOrder == hungerAsc:
      crewSortOrder = hungerDesc
    else:
      crewSortOrder = hungerAsc
  of 9:
    if crewSortOrder == moraleAsc:
      crewSortOrder = moraleDesc
    else:
      crewSortOrder = moraleAsc
  else:
    discard
  let
    skillBox = mainPaned & ".shipinfoframe.crew.canvas.frame.selectskill.combox"
    skillIndex = findSkillIndex(skillName = tclEval2(script = skillBox & " get"))
  if crewSortOrder == none:
    if column == Positive.high:
      updateCrewInfo(skill = skillIndex)
    return tclOk
  type LocalMemberData = object
    selected: bool
    name: string
    order: CrewOrders
    skill: string
    health: SkillRange
    fatigue: int
    thirst: SkillRange
    hunger: SkillRange
    morale: SkillRange
    id: Natural
  var localCrew: seq[LocalMemberData]
  for index, member in playerShip.crew:
    try:
      localCrew.add(y = LocalMemberData(selected: tclGetVar(
          varName = "crewindex" & $(index + 1)) == "1", name: member.name,
          order: member.order, skill: (if skillIndex == 0: getHighestSkill(
          memberIndex = index) else: getSkillLevelName(
          skillLevel = getSkillLevel(
          member = member, skillIndex = skillIndex))), health: member.health,
          fatigue: member.tired - member.attributes[conditionIndex].level,
          thirst: member.thirst, hunger: member.hunger, morale: member.morale[
              1], id: index))
    except:
      return showError(message = "Can't add local crew member.")
  proc sortCrew(x, y: LocalMemberData): int =
    case crewSortOrder
    of selectedAsc:
      if x.selected < y.selected:
        return 1
      else:
        return -1
    of selectedDesc:
      if x.selected > y.selected:
        return 1
      else:
        return -1
    of nameAsc:
      if x.name < y.name:
        return 1
      else:
        return -1
    of nameDesc:
      if x.name > y.name:
        return 1
      else:
        return -1
    of orderAsc:
      if x.order < y.order:
        return 1
      else:
        return -1
    of orderDesc:
      if x.order > y.order:
        return 1
      else:
        return -1
    of skillAsc:
      if x.skill < y.skill:
        return 1
      else:
        return -1
    of skillDesc:
      if x.skill > y.skill:
        return 1
      else:
        return -1
    of healthAsc:
      if x.health < y.health:
        return 1
      else:
        return -1
    of healthDesc:
      if x.health > y.health:
        return 1
      else:
        return -1
    of fatigueAsc:
      if x.fatigue < y.fatigue:
        return 1
      else:
        return -1
    of fatigueDesc:
      if x.fatigue > y.fatigue:
        return 1
      else:
        return -1
    of thirstAsc:
      if x.thirst < y.thirst:
        return 1
      else:
        return -1
    of thirstDesc:
      if x.thirst > y.thirst:
        return 1
      else:
        return -1
    of hungerAsc:
      if x.hunger < y.hunger:
        return 1
      else:
        return -1
    of hungerDesc:
      if x.hunger > y.hunger:
        return 1
      else:
        return -1
    of moraleAsc:
      if x.morale < y.morale:
        return 1
      else:
        return -1
    of moraleDesc:
      if x.morale > y.morale:
        return 1
      else:
        return -1
    of none:
      return -1
  localCrew.sort(cmp = sortCrew)
  crewIndexes = @[]
  for member in localCrew:
    crewIndexes.add(y = member.id)
  try:
    updateCrewInfo(skill = tclEval2(script = skillBox & " current").parseInt)
  except:
    showError(message = "Can't update the crew info.")
  return tclOk

proc selectCrewSkillCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [RootEffect], exportc.} =
  ## Show the list of the player's ship crew with selected skill from combobox
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SelectCrewSkill
  let skillBox = mainPaned & ".shipinfoframe.crew.canvas.frame.selectskill.combox"
  try:
    updateCrewInfo(skill = tclEval2(script = skillBox & " current").parseInt)
  except:
    showError(message = "Can't update the crew info.")
  return tclOk

proc setAvailableOrders(memberIndex: Natural; ordersBox,
    button: string) {.sideEffect, raises: [], tags: [].} =
  ## Set the list of available orders for the selected crew member
  ##
  ## * memberIndex - the index of the crew member for which the list will be set
  ## * orderBox    - the Ttk_ComboBox in which the orders will be set
  ## * button      - the Ttk_Button which will set the order
  var needRepair, needClean = false
  for module in playerShip.modules:
    if module.durability < module.maxDurability:
      needRepair = true
    if (module.durability > 0 and module.mType == ModuleType2.cabin) and
        module.cleanliness < module.quality:
      needClean = true
    if needRepair and needClean:
      break
  let member = playerShip.crew[memberIndex]
  var availableOrders, tclCommands = ""
  if ((member.tired == 100 or member.hunger == 100 or member.thirst == 100) and
      member.order != rest) or member.skills.len == 0 or
      member.contractLength == 0:
    availableOrders.add(y = " {Go on break}")
    tclCommands.add(y = " {Rest " & $(memberIndex + 1) & "}")
  else:
    if member.order != pilot:
      availableOrders.add(y = " {Go piloting the ship}")
      tclCommands.add(y = " {Pilot " & $(memberIndex + 1) & "}")
    if member.order != engineer:
      availableOrders.add(y = " {Go engineering the ship}")
      tclCommands.add(y = " {Engineer " & $(memberIndex + 1) & "}")

    proc isWorking(owners: seq[int]; mIndex: Natural): bool =
      for owner in owners:
        if owner == mIndex:
          return true
      return false

    for index, module in playerShip.modules:
      if module.durability > 0:
        case module.mType
        of gun, harpoonGun:
          if module.owner[0] != memberIndex:
            availableOrders.add(y = " {Operate " & module.name & "}")
            tclCommands.add(y = " {Gunner " & $(memberIndex + 1) & " " & $(
                index + 1) & "}")
        of workshop:
          if not isWorking(owners = module.owner, mIndex = memberIndex) and
              module.craftingIndex.len > 0:
            try:
              availableOrders.add(y = " {" & (if module.craftingIndex.len >
                  6 and module.craftingIndex[0 .. 4] == "Study": "Study " &
                  itemsList[module.craftingIndex[6 ..
                  ^1].strip.parseInt].name elif module.craftingIndex.len >
                  12 and module.craftingIndex[0 .. 10] ==
                  "Deconstruct": "Deconstruct " & itemsList[
                  module.craftingIndex[12 ..
                  ^1].strip.parseInt].name else: "Manufacture " &
                  $module.craftingAmount & "x " & itemsList[recipesList[
                  module.craftingIndex].resultIndex].name) & "}")
            except:
              showError(message = "Can't add an available order.")
              return
            tclCommands.add(y = " {Craft " & $(memberIndex + 1) & " " & $(
                index + 1) & "}")
        of cabin:
          if module.cleanliness < module.quality and member.order != clean and needClean:
            availableOrders.add(y = " {Clean ship}")
            tclCommands.add(y = " {Clean " & $(memberIndex + 1) & "}")
            needClean = false
        of trainingRoom:
          if not isWorking(owners = module.owner, mIndex = memberIndex):
            availableOrders.add(y = " {Go training in " & module.name & "}")
            tclCommands.add(y = " {Train " & $(memberIndex + 1) & " " & $(
                index + 1) & "}")
        else:
          discard
        if needRepair:
          availableOrders.add(y = " {Repair ship}")
          tclCommands.add(y = " {Repair " & $(memberIndex + 1) & "}")
    for index, member2 in playerShip.crew:
      if member2.health < 100 and index != memberIndex and member2.order != heal:
        availableOrders.add(y = " {Heal wounded crew members}")
        tclCommands.add(y = " {Heal " & $(memberIndex + 1) & "}")
        break
    if playerShip.upgradeModule > -1 and member.order != upgrading:
      availableOrders.add(y = " {Upgrade module}")
      tclCommands.add(y = " {Upgrading " & $(memberIndex + 1) & "}")
    if member.order != talk:
      availableOrders.add(y = " {Talk with others}")
      tclCommands.add(y = " {Talk " & $(memberIndex + 1) & "}")
    if member.order != rest:
      availableOrders.add(y = " {Go on break}")
      tclCommands.add(y = " {Rest " & $(memberIndex + 1) & "}")
  tclEval(script = ordersBox & " configure -values [list " & availableOrders & "]")
  tclEval(script = button & " configure -command {SelectCrewOrder {" &
      tclCommands & "} " & $(memberIndex + 1) & ";CloseDialog .memberdialog}")

proc showCrewOrderCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Show the dialog to change the order of the currently selected crew member
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowCrewOrder memberindex
  ## MemberIndex is the index of the crew member which order will be changed
  let
    memberIndex = try:
        ($argv[1]).parseInt - 1
      except:
        return showError(message = "Can't get the crew member index.")
    member = playerShip.crew[memberIndex]
    memberDialog = createDialog(name = ".memberdialog",
        title = "Change order for " & member.name, columns = 2)
    orderInfo = memberDialog & ".orderinfo"
  tclEval(script = "ttk::label " & orderInfo & " -text {Current order:}")
  tclEval(script = "grid " & orderInfo & " -padx 5")
  let orderLabel = memberDialog & ".current"
  try:
    tclEval(script = "ttk::label " & orderLabel & " -text {" & getCurrentOrder(
        memberIndex = memberIndex) & "} -wraplength 275 -style Golden.TLabel")
  except:
    return showError(message = "Can't get the current order.")
  tclEval(script = "grid " & orderLabel & " -padx 5 -column 1 -row 1 -sticky w")
  let ordersInfo = memberDialog & ".ordersinfo"
  tclEval(script = "ttk::label " & ordersInfo & " -text {New order:}")
  tclEval(script = "grid " & ordersInfo & " -padx 5 -sticky w")
  let ordersBox = memberDialog & ".list"
  tclEval(script = "ttk::combobox " & ordersBox & " -state readonly")
  let buttonsBox = memberDialog & ".buttons"
  tclEval(script = "ttk::frame " & buttonsBox)
  let closeDialogButton = buttonsBox & ".button"
  tclEval(script = "ttk::button " & closeDialogButton &
      " -text Cancel -command {CloseDialog " & memberDialog & "} -image cancelicon -style Dialogred.TButton")
  let acceptButton = buttonsBox & ".button2"
  tclEval(script = "ttk::button " & acceptButton & " -text Assign -image giveorder2icon -style Dialoggreen.TButton")
  tclEval(script = "bind " & ordersBox & " <Escape> {" & closeDialogButton & " invoke;break}")
  tclEval(script = "bind " & ordersBox & " <Tab> {focus " & acceptButton & ";break}")
  setAvailableOrders(memberIndex = memberIndex, ordersBox = ordersBox,
      button = acceptButton)
  tclEval(script = ordersBox & " current 0")
  tclEval(script = "grid " & ordersBox & " -padx 5 -column 1 -row 2 -sticky w")
  tclEval(script = "grid " & buttonsBox & " -columnspan 2 -pady {0 5}")
  tclEval(script = "grid " & acceptButton)
  tclEval(script = "bind " & acceptButton & " <Tab> {focus " &
      closeDialogButton & ";break}")
  tclEval(script = "bind " & acceptButton & " <Escape> {" & closeDialogButton & " invoke;break}")
  tclEval(script = "grid " & closeDialogButton & " -column 1 -row 0 -padx {5 0}")
  tclEval(script = "focus " & closeDialogButton)
  tclEval(script = "bind " & closeDialogButton & " <Tab> {focus " & ordersBox & ";break}")
  tclEval(script = "bind " & closeDialogButton & " <Escape> {" &
      closeDialogButton & " invoke;break}")
  showDialog(dialog = memberDialog)
  return tclOk

proc selectCrewOrderCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [RootEffect], exportc.} =
  ## Set the selected order for the selected crew member
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SelectCrewOrder orderslist memberindex
  ## Orderslist is the list of the available orders with their parameters,
  ## memberindex is the crew index of the selected crew member
  let
    ordersBox = ".memberdialog.list"
    orderIndex = try:
        tclEval2(script = ordersBox & " current").parseInt
      except:
        return showError("Can't get the index of the order.")
  var arguments = tclEval2(script = "lindex {" & $argv[1] & "} " &
        $orderIndex).split(sep = " ")
  arguments.insert("SetCrewOrder", 0)
  discard setCrewOrderCommand(clientData = clientData, interp = interp,
      argc = arguments.len.cint, argv = arguments.allocCStringArray)
  let
    orderLabel = ".memberdialog.current"
    memberIndex = try:
        ($argv[2]).parseInt - 1
      except:
        return showError(message = "Can't get the member's index.")
    button = ".memberdialog.buttons.button2"
  try:
    tclEval(script = orderLabel & " configure -text {" & getCurrentOrder(
        memberIndex = memberIndex) & "}")
  except:
    return showError("Can't get the current order.")
  setAvailableOrders(memberIndex = memberIndex, ordersBox = ordersBox,
      button = button)
  tclEval(script = "focus " & ordersBox)
  return tclOk

proc toggleAllCrewCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [RootEffect], exportc.} =
  ## Select or deselect all crew members
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ToggleAllCrew action
  ## Action is the action which will be performed. Possible values are
  ## select or deselect

  proc resetSelection() =
    for index, _ in playerShip.crew:
      if tclGetVar(varName = "crewindex" & $(index + 1)) == "1":
        tclUnsetVar(varName = "crewindex" & $(index + 1))

  if argv[1] == "unselect":
    resetSelection()
  else:
    for index, _ in playerShip.crew:
      tclSetVar(varName = "crewindex" & $(index + 1), newValue = "1")
  return sortCrewCommand(clientData = clientData, interp = interp, argc = 2,
      argv = @["SortShipCrew", "-1"].allocCStringArray)

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the crew UI
  try:
#    addCommand("OrderForAll", orderForAllCommand)
#    addCommand("ToggleCrewMember", toggleCrewMemberCommand)
#    addCommand("Dismiss", dismissCommand)
#    addCommand("SetCrewOrder", setCrewOrderCommand)
#    addCommand("ShowMemberTab", showMemberTabCommand)
#    addCommand("ShowMemberInfo", showMemberInfoCommand)
#    addCommand("ShowCrewStatsInfo", showCrewStatsInfoCommand)
#    addCommand("ShowCrewSkillInfo", showCrewSkillInfoCommand)
#    addCommand("SetPriority", setPriorityCommand)
#    addCommand("ShowCrew", showCrewCommand)
#    addCommand("SortShipCrew", sortCrewCommand)
#    addCommand("SelectCrewSkill", selectCrewSkillCommand)
#    addCommand("ShowCrewOrder", showCrewOrderCommand)
#    addCommand("SelectCrewOrder", selectCrewOrderCommand)
#    addCommand("ToggleAllCrew", toggleAllCrewCommand)
    shipsuicrewinventory.addCommands()
  except:
    showError(message = "Can't add a Tcl command.")

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
    columnsWidth: var array[10, cint]; row, rowHeight: var cint) {.raises: [],
        tags: [RootEffect], exportc.} =
  crewIndexes = @[]
  for index in cIndexes:
    if index == 0:
      break
    crewIndexes.add(index - 1)
  try:
    updateCrewInfo(page = page, skill = skill)
  except:
    echo getCurrentExceptionMsg()
    echo getStackTrace(e = getCurrentException())
  for index, width in crewTable.columnsWidth:
    columnsWidth[index] = width.cint
  row = crewTable.row.cint
  rowHeight = crewTable.rowHeight.cint

proc setAdaAvailableOrders(memberIndex: cint; ordersBox,
    button: cstring) {.raises: [], tags: [], exportc.} =
  try:
    setAvailableOrders(memberIndex = memberIndex - 1, ordersBox = $ordersBox,
        button = $button)
  except:
    echo getCurrentExceptionMsg()

proc addAdaCrewCommands() {.raises: [], tags: [RootEffect], exportc.} =
  try:
    addCommands()
  except:
    echo getCurrentExceptionMsg()
