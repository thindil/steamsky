# Copyright 2024-2026 Bartek thindil Jasicki
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

## Provides code related to the list of known bases, like showing it,
## sorting or showing them on the map.

import std/[algorithm, strutils, tables]
import contracts, nimalyzer
import ../[bases, basestypes, config, game, maps, messages, reputation, tk,
    types, utils]
import coreui, dialogs, errordialog, table

{.push ruleOff: "varDeclared".}
var
  basesTable: TableWidget
  basesIndexes: seq[Positive] = @[]
{.pop ruleOn: "varDeclared".}

proc updateBasesList*(baseName: string = "", page: Positive = 1) {.
    raises: [], tags: [RootEffect], contractual.} =
  ## Update and show list of known bases
  ##
  ## * baseName - the name of the base to find in the list
  ## * page     - the current page of the bases' list to show
  if basesTable.row > 1:
    clearTable(table = basesTable)
  let
    basesCanvas: string = mainPaned & ".knowledgeframe.bases.canvas"
    basesFrame: string = basesCanvas & ".frame"
  var rows: Natural = try:
      tclEval2(script = "grid size " & basesFrame).split(sep = " ")[1].parseInt
    except:
      showError(message = "Can't get the amount of rows.")
      return
  deleteWidgets(startIndex = 2, endIndex = rows - 1, frame = basesFrame)
  basesTable = createTable(parent = basesFrame, headers = @["Name", "Distance",
      "Coordinates", "Population", "Size", "Owner", "Type", "Reputation"],
      scrollbar = ".gameframe.paned.knowledgeframe.bases.scrolly",
      command = "SortKnownBases {" & baseName & "}",
      tooltipText = "Press mouse button to sort the bases.")
  if basesIndexes.len == 0:
    for index, _ in skyBases:
      basesIndexes.add(y = index)
  let searchEntry: string = basesFrame & ".options.search"
  if baseName.len == 0:
    tclEval(script = searchEntry & " configure -validatecommand {}")
    tclEval(script = searchEntry & " delete 0 end")
    tclEval(script = searchEntry & " configure -validatecommand {ShowBases %P}")
  var comboBox: string = basesFrame & ".options.types"
  var basesType: string = tclEval2(script = comboBox & " get")
  if basesType != "Any":
    for index, base in basesTypesList:
      if base.name == basesType:
        basesType = index
        break
  comboBox = basesFrame & ".options.status"
  let basesStatus: string = tclEval2(script = comboBox & " get")
  comboBox = basesFrame & ".options.owner"
  let basesOwner: string = tclEval2(script = comboBox & " get")
  rows = 0
  let startRow: Natural = ((page - 1) * gameSettings.listsLimit) + 1
  tclEval(script = "grid configure " & basesTable.canvas & " -row 2")
  var currentRow: Positive = 1
  for index in basesIndexes:
    if not skyBases[index].known:
      continue
    if baseName.len > 0 and not skyBases[index].name.toLowerAscii.contains(
        sub = baseName.toLowerAscii):
      continue
    if basesStatus == "Only not visited" and skyBases[index].visited.year != 0:
      continue
    if basesStatus == "Only visited" and skyBases[index].visited.year == 0:
      continue
    if skyBases[index].visited.year == 0 and (basesType != "Any" or
        basesOwner != "Any"):
      continue
    if basesType != "Any" and skyBases[index].baseType != basesType:
      continue
    if currentRow < startRow:
      currentRow.inc
      continue
    var color: string = (if skyBases[index].visited.year > 0: "green3" else: "")
    if skyBases[index].skyX == playerShip.destinationX and skyBases[
        index].skyY == playerShip.destinationY:
      color = "yellow"
    addButton(table = basesTable, text = skyBases[index].name,
        tooltip = "Show the base's details", command = "ShowBaseInfo " & $index,
        column = 1, color = color)
    addButton(table = basesTable, text = $countDistance(destinationX = skyBases[
        index].skyX, destinationY = skyBases[index].skyY),
        tooltip = "The distance to the base", command = "ShowBaseInfo " &
        $index, column = 2, color = color)
    addButton(table = basesTable, text = "X: " & $skyBases[index].skyX &
        " Y: " & $skyBases[index].skyY, tooltip = "The coordinates of the base",
        command = "ShowBaseInfo " & $index, column = 3, color = color)
    if skyBases[index].visited.year > 0:
      addButton(table = basesTable, text = $getBasePopulation(
          baseIndex = index), tooltip = "The population size of the base",
          command = "ShowBaseInfo " & $index, column = 4, color = color)
      addButton(table = basesTable, text = ($skyBases[index].size).toLowerAscii,
          tooltip = "The size of the base", command = "ShowBaseInfo " & $index,
          column = 5, color = color)
      try:
        addButton(table = basesTable, text = factionsList[skyBases[
            index].owner].name, tooltip = "The faction which own the base",
            command = "ShowBaseInfo " & $index, column = 6, color = color)
      except:
        showError(message = "Can't show the faction name.")
        return
      try:
        addButton(table = basesTable, text = basesTypesList[skyBases[
            index].baseType].name, tooltip = "The type of the base",
            command = "ShowBaseInfo " & $index, column = 7, color = color)
      except:
        showError(message = "Can't show the type of the base.")
        return
      addButton(table = basesTable, text = getReputationText(
          reputationLevel = skyBases[index].reputation.level),
          tooltip = "Your reputation in the base", command = "ShowBaseInfo " &
          $index, column = 8, newRow = true, color = color)
    else:
      addButton(table = basesTable, text = "not",
          tooltip = "Show the base's details", command = "ShowBaseInfo " &
          $index, column = 4, color = color)
      addButton(table = basesTable, text = "",
          tooltip = "Show the base's details", command = "ShowBaseInfo " &
          $index, column = 5, color = color)
      addButton(table = basesTable, text = "visited",
          tooltip = "Show the base's details", command = "ShowBaseInfo " &
          $index, column = 6, color = color)
      addButton(table = basesTable, text = "",
          tooltip = "Show the base's details", command = "ShowBaseInfo " &
          $index, column = 7, color = color)
      addButton(table = basesTable, text = "yet",
          tooltip = "Show the base's details", command = "ShowBaseInfo " &
          $index, column = 8, newRow = true, color = color)
    rows.inc
    if rows == gameSettings.listsLimit + 1 and index < skyBases.high:
      break
  if page > 1:
    addPagination(table = basesTable, previousCommand = "ShowBases {" &
        baseName & "} " & $(page - 1), nextCommand = (if basesTable.row <
        gameSettings.listsLimit + 1: "" else: "ShowBases {" & baseName & "} " &
        $(page + 1)))
  elif basesTable.row == gameSettings.listsLimit + 2:
    addPagination(table = basesTable, previousCommand = "",
        nextCommand = "ShowBases {" & baseName & "} " & $(page + 1))
  updateTable(table = basesTable, grabFocus = tclEval2(script = "focus") != searchEntry)
  tclEval(script = basesCanvas & " xview moveto 0.0")
  tclEval(script = basesCanvas & " yview moveto 0.0")
  tclEval(script = "update")
  tclEval(script = basesCanvas & " configure -scrollregion [list " & tclEval2(
      script = basesCanvas & " bbox all") & "]")

proc showBasesCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    RootEffect], cdecl, contractual, ruleOff: "params".} =
  ## Show the list of known bases to a player
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowBases ?basename? ?page?
  ## Basename parameter is a string which will be looking for in the bases
  ## names, page parameter is a index of page from which starts showing
  ## bases.
  case argc
  of 3:
    try:
      updateBasesList(baseName = $argv[1], page = ($argv[2]).parseInt)
    except:
      return showError(message = "Can't update the list of bases.")
  of 2:
    updateBasesList(baseName = $argv[1])
  else:
    updateBasesList()
  tclSetResult(value = "1")
  return tclOk

proc showBaseInfoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
    TimeEffect, RootEffect], cdecl, contractual.} =
  ## Show information about the selected base
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowBaseInfo baseindex
  ## BaseIndex is the index of the base to show
  let
    baseIndex: BasesRange = try:
        ($argv[1]).parseInt
      except:
        return showError(message = "Can't get the base's index.")
    baseDialog: string = createDialog(name = ".basedialog", title = skyBases[
        baseIndex].name, columns = 3)
    baseLabel: string = baseDialog & ".info"
  tclEval(script = "text " & baseLabel & " -wrap char -height 5 -width 30")
  tclEval(script = baseLabel & " tag configure gold -foreground " & tclGetVar(
      varName = "ttk::theme::" & gameSettings.interfaceTheme &
      "::colors(-goldenyellow)"))
  tclEval(script = baseLabel & " tag configure red -foreground " & tclGetVar(
      varName = "ttk::theme::" & gameSettings.interfaceTheme &
      "::colors(-red)"))
  tclEval(script = baseLabel & " tag configure green -foreground " & tclGetVar(
      varName = "ttk::theme::" & gameSettings.interfaceTheme &
      "::colors(-green)"))
  tclEval(script = baseLabel & " tag configure cyan -foreground " & tclGetVar(
      varName = "ttk::theme::" & gameSettings.interfaceTheme &
      "::colors(-cyan)"))
  tclEval(script = baseLabel & " insert end {Coordinates X: }")
  tclEval(script = baseLabel & " insert end {" & $skyBases[baseIndex].skyX & "} [list gold]")
  tclEval(script = baseLabel & " insert end { Y: }")
  tclEval(script = baseLabel & " insert end {" & $skyBases[baseIndex].skyY & "} [list gold]")
  var linesDiff: int = (if skyBases[baseIndex].visited.year > 0: 1 else: 0)
  let population: BasePopulation = getBasePopulation(baseIndex = baseIndex)
  if skyBases[baseIndex].visited.year > 0:
    tclEval(script = baseLabel & " insert end {\nLast visited: }")
    tclEval(script = baseLabel & " insert end {" & formattedTime(
        time = skyBases[baseIndex].visited) & "} [list gold]")
    var timeDiff: int = 0
    if population > empty and skyBases[baseIndex].reputation.level > -25:
      timeDiff = 30 - daysDifference(dateToCompare = skyBases[
          baseIndex].recruitDate)
      if timeDiff > 0:
        tclEval(script = baseLabel & " insert end {\nNew recruits available in }")
        tclEval(script = baseLabel & " insert end {" & $timeDiff & "} [list gold]")
        tclEval(script = baseLabel & " insert end { days.}")
      else:
        tclEval(script = baseLabel & " insert end {\nNew recruits available now.} [list green]")
    else:
      tclEval(script = baseLabel & " insert end {\nYou can't recruit crew members at this base.} [list red]")
    if population > empty and skyBases[baseIndex].reputation.level > -25:
      timeDiff = daysDifference(dateToCompare = skyBases[
          baseIndex].askedForEvents)
      if timeDiff < 7:
        tclEval(script = baseLabel & " insert end {\nYou asked for events }")
        tclEval(script = baseLabel & " insert end {" & $timeDiff & "} [list gold]")
        tclEval(script = baseLabel & " insert end { days ago.}")
      else:
        tclEval(script = baseLabel & " insert end {\nYou can ask for events again.} [list green]")
    else:
      tclEval(script = baseLabel & " insert end {\nYou can't ask for events at this base.} [list red]")
    if population > empty and skyBases[baseIndex].reputation.level > -1:
      timeDiff = 7 - daysDifference(dateToCompare = skyBases[
          baseIndex].missionsDate)
      if timeDiff > 0:
        tclEval(script = baseLabel & " insert end {\nNew missions available in }")
        tclEval(script = baseLabel & " insert end {" & $timeDiff & "} [list gold]")
        tclEval(script = baseLabel & " insert end { days.}")
      else:
        tclEval(script = baseLabel & " insert end {\nNew missions available now.} [list green]")
        linesDiff.dec
    else:
      tclEval(script = baseLabel & " insert end {\nYou can't take missions at this base.} [list red]")

    proc setReputationText(reputationText: string) =
      let reputationLabel: string = baseDialog & ".reputationlabel"
      tclEval(script = "ttk::label " & reputationLabel)
      if skyBases[baseIndex].reputation.level == 0:
        tclEval(script = reputationLabel & " configure -text {Reputation: Unknown}")
        tclEval(script = "grid " & reputationLabel & " -row 2 -sticky w -padx {5 0} -columnspan 2")
      else:
        tclEval(script = reputationLabel & " configure -text {Reputation:}")
        let reputationBar: string = baseDialog & ".reputation"
        tclEval(script = "ttk::frame " & reputationBar & " -width 204 -height 24 -style ProgressBar.TFrame")
        tclEval(script = "grid " & reputationBar & " -row 2 -column 1 -padx 5 -columnspan 2")
        tclEval(script = "grid propagate " & reputationBar & " off")
        let reputationProgress: string = reputationBar & ".reputation"
        tclEval(script = "ttk::frame " & reputationProgress &
            " -height 18 -width " & $(skyBases[baseIndex].reputation.level.abs))
        if skyBases[baseIndex].reputation.level > 0:
          tclEval(script = reputationProgress & " configure -style GreenProgressBar.TFrame")
          tclEval(script = "grid " & reputationProgress & " -padx {100 0} -pady 3")
        else:
          tclEval(script = reputationProgress & " configure -style RedProgressBar.TFrame")
          tclEval(script = "grid " & reputationProgress & " -padx {" & $(100 +
              skyBases[baseIndex].reputation.level) & " 0} -pady 3")
        tclEval(script = "tooltip::tooltip " & reputationBar & " \"" &
            reputationText & "\"")
        tclEval(script = "grid " & reputationLabel & " -row 2 -sticky w -padx {5 0}")

    setReputationText(reputationText = getReputationText(
        reputationLevel = skyBases[baseIndex].reputation.level))
    if baseIndex == playerShip.homeBase:
      tclEval(script = baseLabel & " insert end {\nIt is your home base.} [list cyan]")
  else:
    tclEval(script = baseLabel & " insert end {\nNot visited yet.} [list red]")
  try:
    tclEval(script = baseLabel & " configure -state disabled -height " & $((
        tclEval2(script = baseLabel & " count -displaylines 0.0 end").parseInt /
        tclEval2(script = "font metrics InterfaceFont -linespace").parseInt) -
        linesDiff.float))
  except:
    return showError(message = "Can't configure text field.")
  tclEval(script = "grid " & baseLabel & " -row 1 -columnspan 3 -padx 5 -pady {5 0} -sticky w")
  var baseButton: string = baseDialog & ".destination"
  tclEval(script = "ttk::button " & baseButton &
      " -text Target -image destinationicon -command {CloseDialog " &
      baseDialog & ";SetDestination2 " & $skyBases[baseIndex].skyX & " " &
      $skyBases[baseIndex].skyY & "} -style Dialoggreen.TButton")
  tclEval(script = "tooltip::tooltip " & baseButton & " \"Set the base as the ship destination\"")
  tclEval(script = "grid " & baseButton & " -row 3 -padx 5 -sticky e")
  tclEval(script = "bind " & baseButton & " <Tab> {focus " & baseDialog & ".button;break}")
  tclEval(script = "bind " & baseButton & " <Escape> {" & baseDialog & ".button invoke;break}")
  addCloseButton(name = baseDialog & ".button", text = "Close",
      command = "CloseDialog " & baseDialog, row = 3, column = 1)
  baseButton = baseDialog & ".button"
  tclEval(script = "bind " & baseButton & " <Tab> {focus " & baseDialog & ".show;break}")
  baseButton = baseDialog & ".show"
  tclEval(script = "ttk::button " & baseButton &
      " -text Show -image show2icon -command {CloseDialog " & baseDialog &
      ";ShowOnMap " & $skyBases[baseIndex].skyX & " " & $skyBases[
      baseIndex].skyY & "} -style Dialoggreen.TButton")
  tclEval(script = "tooltip::tooltip " & baseButton & " \"Show the base on the map\"")
  tclEval(script = "grid " & baseButton & " -row 3 -column 2 -padx 5")
  tclEval(script = "bind " & baseButton & " <Tab> {focus " & baseDialog & ".destination;break}")
  tclEval(script = "bind " & baseButton & " <Escape> {" & baseDialog & ".button invoke;break}")
  showDialog(dialog = baseDialog)
  return tclOk

type BasesSortOrders = enum
  none, nameAsc, nameDesc, distanceAsc, distanceDesc, populationAsc,
    populationDesc, sizeAsc, sizeDesc, ownerAsc, ownerDesc, typeAsc, typeDesc,
    reputationAsc, reputationDesc, coordAsc, coordDesc

const defaultBasesSortOrder: BasesSortOrders = none

var basesSortOrder: BasesSortOrders = defaultBasesSortOrder

proc sortBasesCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [RootEffect], cdecl,
    contractual.} =
  ## Sort the list of known bases
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SortKnownBases x
  ## X is X axis coordinate where the player clicked the mouse button
  let column: int = try:
        getColumnNumber(table = basesTable, xPosition = ($argv[2]).parseInt)
      except:
        return showError(message = "Can't get the column number.")
  case column
  of 1:
    if basesSortOrder == nameAsc:
      basesSortOrder = nameDesc
    else:
      basesSortOrder = nameAsc
  of 2:
    if basesSortOrder == distanceAsc:
      basesSortOrder = distanceDesc
    else:
      basesSortOrder = distanceAsc
  of 3:
    if basesSortOrder == coordAsc:
      basesSortOrder = coordDesc
    else:
      basesSortOrder = coordAsc
  of 4:
    if basesSortOrder == populationAsc:
      basesSortOrder = populationDesc
    else:
      basesSortOrder = populationAsc
  of 5:
    if basesSortOrder == sizeAsc:
      basesSortOrder = sizeDesc
    else:
      basesSortOrder = sizeAsc
  of 6:
    if basesSortOrder == ownerAsc:
      basesSortOrder = ownerDesc
    else:
      basesSortOrder = ownerAsc
  of 7:
    if basesSortOrder == typeAsc:
      basesSortOrder = typeDesc
    else:
      basesSortOrder = typeAsc
  of 8:
    if basesSortOrder == reputationAsc:
      basesSortOrder = reputationDesc
    else:
      basesSortOrder = reputationAsc
  else:
    discard
  if basesSortOrder == none:
    return tclOk
  type LocalBaseData = object
    name: string
    distance: Natural
    coords: string
    population: int
    size: BasesSize
    owner: string
    baseType: string
    reputation: int
    id: BasesRange = 1
  var localBases: seq[LocalBaseData] = @[]
  for index, base in skyBases:
    localBases.add(y = LocalBaseData(name: base.name, distance: countDistance(
        destinationX = base.skyX, destinationY = base.skyY), coords: "X: " &
        $base.skyX & " Y: " & $base.skyY, population: (if base.visited.year ==
        0: -1 else: base.population), size: (if base.visited.year ==
        0: unknown else: base.size), owner: (if base.visited.year ==
        0: "" else: base.owner), baseType: (if base.visited.year ==
        0: "" else: base.baseType), reputation: (if base.visited.year ==
        0: 200 else: base.reputation.level), id: index))
  proc sortBases(x, y: LocalBaseData): int {.raises: [], tags: [],
      contractual.} =
    ## Compare two bases and return which should go first, based on the sort
    ## order of the bases
    ##
    ## * x - the first base to compare
    ## * y - the second base to compare
    ##
    ## Returns 1 if the first base should go first, -1 if the second base
    ## should go first.
    case basesSortOrder
    of nameAsc:
      if x.name < y.name:
        return 1
      return -1
    of nameDesc:
      if x.name > y.name:
        return 1
      return -1
    of distanceAsc:
      if x.distance < y.distance:
        return 1
      return -1
    of distanceDesc:
      if x.distance > y.distance:
        return 1
      return -1
    of coordAsc:
      if x.coords < y.coords:
        return 1
      return -1
    of coordDesc:
      if x.coords > y.coords:
        return 1
      return -1
    of populationAsc:
      if x.population < y.population:
        return 1
      return -1
    of populationDesc:
      if x.population > y.population:
        return 1
      return -1
    of sizeAsc:
      if x.size < y.size:
        return 1
      return -1
    of sizeDesc:
      if x.size > y.size:
        return 1
      return -1
    of ownerAsc:
      if x.owner < y.owner:
        return 1
      return -1
    of ownerDesc:
      if x.owner > y.owner:
        return 1
      return -1
    of typeAsc:
      if x.baseType < y.baseType:
        return 1
      return -1
    of typeDesc:
      if x.baseType > y.baseType:
        return 1
      return -1
    of reputationAsc:
      if x.reputation < y.reputation:
        return 1
      return -1
    of reputationDesc:
      if x.reputation > y.reputation:
        return 1
      return -1
    of none:
      return -1
  localBases.sort(cmp = sortBases)
  basesIndexes = @[]
  for base in localBases:
    basesIndexes.add(y = base.id)
  updateBasesList(baseName = $argv[1])
  return tclOk

proc addCommands*() {.raises: [], tags: [WriteIOEffect, TimeEffect,
    RootEffect], contractual.} =
  ## Adds Tcl commands related to the trades UI
  try:
    addCommand(name = "ShowBases", nimProc = showBasesCommand)
    addCommand(name = "ShowBaseInfo", nimProc = showBaseInfoCommand)
    addCommand(name = "SortKnownBases", nimProc = sortBasesCommand)
  except:
    showError(message = "Can't add a Tcl command.")
