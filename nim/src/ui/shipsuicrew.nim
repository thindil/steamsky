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
import ../[config, crew, game, messages, shipscrew, shipscrew2, tk, types]
import coreui, dialogs, table, updateheader, utilsui2

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
    tclEval(script = "bgerror {Can't get the highest skill. Index: " &
        $highestIndex & "}")
    return "Unknown"

proc updateCrewInfo*(page: Positive = 1; skill: Natural = 0) {.sideEffect,
    raises: [], tags: [].} =
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
        tclEval(script = "bgerror {Can't get the size of the grid. Result: " &
            $gridSize & "}")
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
        tclEval(script = "bgerror {Can't get the level of the skill.}")
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
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [RootEffect].} =
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
          tclEval(script = "bgerror {Can't give orders. Reason: " &
              getCurrentExceptionMsg() & "}")
          return tclOk
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
        tclEval(script = "bgerror {Can't give orders. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
  updateHeader()
  updateMessages()
  updateCrewInfo()
  return tclOk

proc toggleCrewMemberCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
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
      tclEval(script = "bgerror {Can't get the row. Reason: " &
          getCurrentExceptionMsg() & "}")
      return tclOk
  toggleCheckedButton(table = crewTable, row = row, column = 1)
  if isChecked(table = crewTable, row = row, column = 1):
    tclSetVar(varName = "crewindex" & $argv[2], newValue = "1")
  else:
    tclUnsetVar(varName = "crewindex" & $argv[2])
  updateTooltips()
  return tclOk

proc dismissCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
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
      tclEval(script = "bgerror {Can't get the crew member index. Reason: " &
          getCurrentExceptionMsg() & "}")
      return tclOk
  showQuestion(question = "Are you sure want to dismiss " & playerShip.crew[
      memberIndex - 1].name & "?", res = $argv[1])
  return tclOk

proc setCrewOrderCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [RootEffect].} =
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
        tclEval(script = "bgerror {Can't get the module index. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
  try:
    giveOrders(ship = playerShip, memberIndex = ($argv[2]).parseInt - 1,
        givenOrder = parseEnum[CrewOrders](s = $argv[1]),
        moduleIndex = moduleIndex)
  except CrewOrderError, CrewNoSpaceError:
    addMessage(message = getCurrentExceptionMsg(), mType = orderMessage, color = red)
    updateMessages()
    return tclOk
  except:
    tclEval(script = "bgerror {Can't give order. Reason: " &
        getCurrentExceptionMsg() & "}")
    return tclOk
  updateHeader()
  updateMessages()
  updateCrewInfo()
  return tclOk

proc showMemberTabCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
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
    argv: openArray[cstring]): TclResults =
  let
    memberIndex = ($argv[1]).parseInt
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
  if argv[1] == "1" and playerShip.speed == docked:
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
    tclEval(script = "ttk::frame " & labelBox & " -width 360")
    memberLabel = labelBox & ".label1"
    tclEval(script = "ttk::label " & memberLabel & " -text {" & text & "} -wraplength 360")
    tclEval(script = "grid " & memberLabel & " -sticky w")
    tclEval(script = "SetScrollbarBindings " & memberLabel & " " & yScroll)
    if text2.len > 0:
      memberLabel = labelBox & "label2"
      tclEval(script = "ttk::label " & memberLabel & " -text {" & text2 & "} -wraplength 360 -style Golden.TLabel")
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
    addLabel(name = frame & ".orderinfo", text = "Order: ",
        text2 = getCurrentOrder(memberIndex = memberIndex))
    infoButton = frame & ".orderinfo.button"
    tclEval(script = "ttk::button " & infoButton &
        " -image giveordericon -command {" & closeButton &
        " invoke;ShowCrewOrder " & $memberIndex & "} -style Small.TButton")
    tclEval(script = "tooltip::tooltip " & infoButton & " \"Set the new order for the crew member\"")
    tclEval(script = "grid " & infoButton & " -row 0 -column 2 -sticky n -padx {5 0}")
    tclEval(script = "bind " & infoButton & " <Escape> {" & closeButton & " invoke;break}")
    tclEval(script = "SetScrollbarBindings " & infoButton & " " & yScroll)
    tclEval(script = "bind " & infoButton & " <Tab> {focus " & buttonsFrame & ".button1;break}")
    infoButton = frame & ".nameinfo.button"
    tclEval(script = "bind " & infoButton & " <Tab> {focus " & frame & ".orderinfo.button;break}")
  else:
    tclEval(script = "bind " & infoButton & " <Tab> {focus " & closeButton & ";break}")
  let faction = factionsList[member.faction]
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
          getAttributeLevelName(attributeLevel = attrib.level) & " -style Golden.TLabel}")
      tclEval(script = "grid " & memberLabel & " -sticky we -column 1 -row 0 -padx {5 0}")
      tclEval(script = "SetScrollbarBindings " & memberLabel & " " & yScroll)
      tclEval(script = "grid columnconfigure " & progressFrame & " " &
          memberLabel & " -weight 1")
      tclEval(script = "grid rowconfigure " & progressFrame & " " &
          memberLabel & " -weight 1")
      infoButton = progressFrame & ".button"
      tclEval(script = "ttk::button " & infoButton &
          " -image helpicon -style Header.Toolbutton -command {ShowCrewStatsInfo " &
          $index & " .memberdialog}")
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
      var progressBar = frame & ".level"
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
      tclEval(script = progressFrame & " configure -length 360")
      progressBar = progressFrame & ".experience" & $index
      tclEval(script = progressBar & " configure -length 360")
    # Skills of the selected crew member
    frame = memberCanvas & ".skills"
    tclEval(script = "ttk::frame " & frame)
    tclEval(script = "SetScrollbarBindings " & frame & " " & yScroll)
    for index, skill in member.skills:
      let progressFrame = frame & ".skillinfo" & $index
      tclEval(script = "ttk::frame " & progressFrame)
      var memberLabel = progressFrame & ".label" & $index
      tclEval(script = "ttk::label " & memberLabel & " -text {" & skillsList[
          skill.index].name & ":}")
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the crew UI
  try:
    addCommand("OrderForAll", orderForAllCommand)
    addCommand("ToggleCrewMember", toggleCrewMemberCommand)
    addCommand("Dismiss", dismissCommand)
    addCommand("SetCrewOrder", setCrewOrderCommand)
    addCommand("ShowMemberTab", showMemberTabCommand)
  except:
    tclEval(script = "bgerror {Can't add a Tcl command. Reason: " &
        getCurrentExceptionMsg() & "}")

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
        tags: [], exportc.} =
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

proc addAdaCrewCommands() {.raises: [], tags: [RootEffect], exportc.} =
  try:
    addCommands()
  except:
    echo getCurrentExceptionMsg()
