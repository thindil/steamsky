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

import std/[algorithm, os, strformat, strutils, tables]
import ../[game, goals, statistics, tk, types]
import coreui, errordialog, utilsui2

var craftingIndexes, missionsIndexes, goalsIndexes, destroyedIndexes,
  killedIndexes: seq[Natural]

proc showStatistics*(refresh: bool = false) {.sideEffect, raises: [], tags: [].} =
  ## Show the game statistics to the player
  ##
  ## * refresh - if true, refresh the view, otherwise back to the sky map
  var statsFrame = mainPaned & ".statsframe"
  let statsCanvas = statsFrame & ".canvas"
  var label = statsCanvas & ".stats.left.points.points"
  if tclEval2(script = "winfo exists " & label) == "0":
    tclEvalFile(fileName = dataDirectory & "ui" & DirSep & "stats.tcl")
    tclEval(script = "bind " & statsFrame & " <Configure> {ResizeCanvas %W.canvas %w %h}")
  elif tclEval2(script = "winfo ismapped " & label) == "1" and not refresh:
    tclEval(script = "InvokeButton " & closeButton)
    return
  tclEval(script = label & " configure -text {" & $getGamePoints() & "}")
  tclEval(script = "tooltip::tooltip " & label & " \"The amount of points gained in this game\"")
  label = statsCanvas & ".stats.left.points.lblpoints"
  tclEval(script = "tooltip::tooltip " & label & " \"The amount of points gained in this game\"")
  var statsText = ""
  let minutesDiff = (gameDate.minutes + (gameDate.hour * 60) + (gameDate.day *
      1_440) + (gameDate.month * 43_200) + (gameDate.year * 518_400)) - 829_571_520
  minutesToDate(minutes = minutesDiff, infoText = statsText)
  label = statsCanvas & ".stats.left.time.time"
  tclEval(script = label & " configure -text {" & statsText & "}")
  tclEval(script = "tooltip::tooltip " & label & " \"In game time which was passed since it started\"")
  label = statsCanvas & ".stats.left.time.lbltime"
  tclEval(script = "tooltip::tooltip " & label & " \"In game time which was passed since it started\"")
  var visitedPercent: float = (gameStats.basesVisited.float / 1_024.0) * 100.0
  statsText = try:
      $gameStats.basesVisited & " (" & fmt"{visitedPercent:5.3f}" & "%)"
    except:
      showError(message = "Can't show info about visited bases.")
      return
  label = statsCanvas & ".stats.left.bases.bases"
  tclEval(script = label & " configure -text {" & statsText & "}")
  tclEval(script = "tooltip::tooltip " & label & " \"The amount of sky bases visited and total percentage of all bases\"")
  label = statsCanvas & ".stats.left.bases.lblbases"
  tclEval(script = "tooltip::tooltip " & label & " \"The amount of sky bases visited and total percentage of all bases\"")
  visitedPercent = (gameStats.mapVisited.float / (1_024.0 * 1_024.0)) * 100.0
  if visitedPercent < 0.001:
    visitedPercent = 0.001
  statsText = try:
      fmt"{visitedPercent:5.3f}" & "%"
    except:
      showError(message = "Can't show info about discovered map.")
      return
  label = statsCanvas & ".stats.left.map.map"
  tclEval(script = label & " configure -text {" & statsText & "}")
  tclEval(script = "tooltip::tooltip " & label & " \"The amount of unique map's fields visited\"")
  label = statsCanvas & ".stats.left.map.lblmap"
  tclEval(script = "tooltip::tooltip " & label & " \"The amount of unique map's fields visited\"")
  statsText = $gameStats.distanceTraveled
  label = statsCanvas & ".stats.left.distance.distance"
  tclEval(script = label & " configure -text {" & statsText & "}")
  tclEval(script = "tooltip::tooltip " & label & " \"The total amount of map's fields visited\"")
  label = statsCanvas & ".stats.left.distance.lbldistance"
  tclEval(script = "tooltip::tooltip " & label & " \"The total amount of map's fields visited\"")
  statsFrame = statsCanvas & ".stats"
  var
    totalFinished = 0
    statsList = gameStats.craftingOrders
  for craftingOrder in statsList:
    totalFinished = totalFinished + craftingOrder.amount
  label = statsFrame & ".left.crafts.crafts"
  tclEval(script = label & " configure -text {" & $totalFinished & "}")
  tclEval(script = "tooltip::tooltip " & label & " \"The total amount of crafting orders finished in this game\"")
  label = statsFrame & ".left.crafts.lblcrafts"
  tclEval(script = "tooltip::tooltip " & label & " \"The total amount of crafting orders finished in this game\"")
  statsFrame = statsCanvas & ".stats.left.craftsframe"
  var treeView = statsFrame & ".craftsview"
  if tclEval2(script = treeView & " children {}") != "{}":
    tclEval(script = treeView & " delete [list " & tclEval2(script = treeView &
        " children {}") & "]")
  if totalFinished > 0:
    if craftingIndexes.len != statsList.len:
      craftingIndexes = @[]
      for index, order in statsList:
        craftingIndexes.add(index)
    for item in craftingIndexes:
      try:
        discard tclEval(script = treeView & " insert {} end -values [list {" &
            itemsList[recipesList[statsList[item].index].resultIndex].name &
            "} {" & $statsList[item].amount & "}]")
      except:
        showError(message = "Can't show finished crafting orders.")
        return
    tclEval(script = treeView & " configure -height " & (if statsList.len <
        10: $statsList.len else: "10"))
    tclEval(script = "grid " & statsFrame)
  else:
    tclEval(script = "grid remove " & statsFrame)
  totalFinished = 0
  statsList = gameStats.finishedMissions
  for finishedMission in statsList:
    totalFinished = totalFinished + finishedMission.amount
  label = statsCanvas & ".stats.left.missions.missions"
  var missionsPercent = 0
  if gameStats.acceptedMissions > 0:
    missionsPercent = ((totalFinished.float /
        gameStats.acceptedMissions.float) * 100.0).int
  tclEval(script = label & " configure -text {" &
      $totalFinished & " (" & $missionsPercent & "%)}")
  tclEval(script = "tooltip::tooltip " & label & " \"The total amount of missions finished in this game\"")
  label = statsCanvas & ".stats.left.missions.lblmissions"
  tclEval(script = "tooltip::tooltip " & label & " \"The total amount of missions finished in this game\"")
  statsFrame = statsCanvas & ".stats.left.missionsframe"
  treeView = statsFrame & ".missionsview"
  if tclEval2(script = treeView & " children {}") != "{}":
    tclEval(script = treeView & " delete [list " & tclEval2(script = treeView &
        " children {}") & "]")
  if totalFinished > 0:
    if missionsIndexes.len != statsList.len:
      missionsIndexes = @[]
      for index, mission in statsList:
        missionsIndexes.add(index)
    for item in missionsIndexes:
      try:
        case parseEnum[MissionsTypes](statsList[item].index)
        of deliver:
          tclEval(script = treeView & " insert {} end -values [list {Delivered items} {" &
              $statsList[item].amount & "}]")
        of patrol:
          tclEval(script = treeView & " insert {} end -values [list {Patroled areas} {" &
              $statsList[item].amount & "}]")
        of destroy:
          tclEval(script = treeView & " insert {} end -values [list {Destroyed ships} {" &
              $statsList[item].amount & "}]")
        of explore:
          tclEval(script = treeView & " insert {} end -values [list {Explored areas} {" &
              $statsList[item].amount & "}]")
        of passenger:
          discard tclEval(script = treeView &
              " insert {} end -values [list {Passengers transported} {" &
              $statsList[item].amount & "}]")
      except:
        showError(message = "Can't show finished missions.")
        return
    tclEval(script = treeView & " configure -height " & (if statsList.len <
        10: $statsList.len else: "10"))
    tclEval(script = "grid " & statsFrame)
  else:
    tclEval(script = "grid remove " & statsFrame)
  label = statsCanvas & ".stats.left.goal"
  try:
    discard tclEval(script = label & " configure -text {" & (if goalText(
        0).len < 22: goalText(0) else: goalText(0)[0 .. 21] & "...") & "}")
    discard tclEval(script = "tooltip::tooltip " & label &
        " \"The current goal: " & goalText(0) & "\"")
  except:
    showError(message = "Can't show the current goal.")
    return
  totalFinished = 0
  statsList = gameStats.finishedGoals
  for finishedGoal in statsList:
    totalFinished = totalFinished + finishedGoal.amount
  label = statsCanvas & ".stats.left.goals.goals"
  tclEval(script = label & " configure -text {" & $totalFinished & "}")
  tclEval(script = "tooltip::tooltip " & label & " \"The total amount of goals finished in this game\"")
  label = statsCanvas & ".stats.left.lblgoals"
  tclEval(script = "tooltip::tooltip " & label & " \"The total amount of goals finished in this game\"")
  statsFrame = statsCanvas & ".stats.left.goalsframe"
  treeView = statsFrame & ".goalsview"
  if tclEval2(script = treeView & " children {}") != "{}":
    tclEval(script = treeView & " delete [list " & tclEval2(script = treeView &
        " children {}") & "]")
  if totalFinished > 0:
    if goalsIndexes.len != statsList.len:
      goalsIndexes = @[]
      for index, goal in statsList:
        goalsIndexes.add(index)
    for item in goalsIndexes:
      var protoIndex = 0
      try:
        for j in 1 .. 256:
          if goalsList[j].index == statsList[item].index:
            protoIndex = j
            break
        discard tclEval(script = treeView & " insert {} end -values [list {" &
            goalText(index = protoIndex) & "} {" & $statsList[item].amount & "}]")
      except:
        showError(message = "Can't show finished goals.")
        return
    tclEval(script = treeView & " configure -height " & (if statsList.len <
        10: $statsList.len else: "10"))
    tclEval(script = "grid " & statsFrame)
  else:
    tclEval(script = "grid remove " & statsFrame)
  statsFrame = statsCanvas & ".stats.right.destroyedframe"
  treeView = statsFrame & ".destroyedview"
  statsList = gameStats.destroyedShips
  var totalDestroyed = 0
  if statsList.len > 0:
    if tclEval2(script = treeView & " children {}") != "{}":
      tclEval(script = treeView & " delete [list " & tclEval2(
          script = treeView & " children {}") & "]")
    if destroyedIndexes.len != statsList.len:
      destroyedIndexes = @[]
      for index, ship in statsList:
        destroyedIndexes.add(index)
    for item in destroyedIndexes:
      for index, ship in protoShipsList:
        if $index == statsList[item].index:
          tclEval(script = treeView & " insert {} end -values [list {" &
              ship.name & "} {" & $statsList[item].amount & "}]")
      totalDestroyed = totalDestroyed + statsList[item].amount
    tclEval(script = treeView & " configure -height " & (if statsList.len <
        10: $statsList.len else: "10"))
    tclEval(script = "grid " & statsFrame)
  else:
    tclEval(script = "grid remove " & statsFrame)
  label = statsCanvas & ".stats.right.destroyed.destroyed"
  tclEval(script = label & " configure -text {" &
      $totalDestroyed & "}")
  tclEval(script = "tooltip::tooltip " & label & " \"The total amount of destroyed ships in this game\"")
  label = statsCanvas & ".stats.right.destroyed.lbldestroyed"
  tclEval(script = "tooltip::tooltip " & label & " \"The total amount of destroyed ships in this game\"")
  statsFrame = statsCanvas & ".stats.right.killedframe"
  treeView = statsFrame & ".killedview"
  totalDestroyed = 0
  statsList = gameStats.killedMobs
  if statsList.len > 0:
    if tclEval2(script = treeView & " children {}") != "{}":
      tclEval(script = treeView & " delete [list " & tclEval2(
          script = treeView & " children {}") & "]")
    if killedIndexes.len != statsList.len:
      killedIndexes = @[]
      for index, mob in statsList:
        killedIndexes.add(index)
    for mob in statsList:
      tclEval(script = treeView & " insert {} end -values [list {" &
          mob.index & "} {" & $mob.amount & "}]")
      totalDestroyed = totalDestroyed + mob.amount
    tclEval(script = treeView & " configure -height " & (if statsList.len <
        10: $statsList.len else: "10"))
    tclEval(script = "grid " & statsFrame)
  else:
    tclEval(script = "grid remove " & statsFrame)
  label = statsCanvas & ".stats.right.killed.killed"
  tclEval(script = label & " configure -text {" &
      $totalDestroyed & "}")
  tclEval(script = "tooltip::tooltip " & label & " \"The total amount of enemies killed in melee combat in this game\"")
  label = statsCanvas & ".stats.right.killed.lblkilled"
  tclEval(script = "tooltip::tooltip " & label & " \"The total amount of enemies killed in melee combat in this game\"")
  tclEval(script = statsCanvas & " configure -height [expr & " & tclEval2(
      script = mainPaned & " sashpos 0") & " - 20] -width " & tclEval2(
      script = mainPaned & " cget -width"))
  tclEval(script = "update")
  statsFrame = statsCanvas & ".stats"
  tclEval(script = statsCanvas & " create window 0 0 -anchor nw -window " & statsFrame)
  tclEval(script = "update")
  tclEval(script = statsCanvas & " configure -scrollregion [list " & tclEval2(
      script = statsCanvas & " bbox all") & "]")
  showScreen(newScreenName = "statsframe")

type ListSortOrders = enum
  none, nameAsc, nameDesc, amountAsc, amountDesc

const defaultListSortOrder: ListSortOrders = none

proc setSortingOrder(sortingOrder: var ListSortOrders;
    column: Positive) {.sideEffect, raises: [], tags: [].} =
  ## Set sorting order for the selected list
  ##
  ## * sortingOrder - the sorting order to set
  ## * column       - the column in ttk_tree_view whith was clicked
  ##
  ## Returns the modified parameter sortingOrder
  sortingOrder = case column
    of 1:
      if sortingOrder == nameAsc:
        nameDesc
      else:
        nameAsc
    of 2:
      if sortingOrder == amountAsc:
        amountDesc
      else:
        amountAsc
    else:
      none

var craftingSortOrder: ListSortOrders = defaultListSortOrder

type
  SortingData = object
    name: string
    amount: Positive = 1
    id: Natural

  SortingList = seq[SortingData]

proc sortFinishedCraftingCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Sort the list of finished crafting orders
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SortFinishedCrafting x
  ## X is the number of column where the player clicked the mouse button
  let column = try:
        ($argv[1]).parseInt
      except:
        return showError(message = "Can't get the column number.")
  setSortingOrder(sortingOrder = craftingSortOrder, column = column)
  if craftingSortOrder == none:
    return tclOk
  var localCrafting: SortingList = @[]
  for index, order in gameStats.craftingOrders:
    try:
      localCrafting.add(y = SortingData(name: itemsList[recipesList[
          order.index].resultIndex].name, amount: order.amount, id: index))
    except:
      return showError(message = "Can't add local order.")
  proc sortCrafting(x, y: SortingData): int =
    case craftingSortOrder
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
    of amountAsc:
      if x.amount < y.amount:
        return 1
      else:
        return -1
    of amountDesc:
      if x.amount > y.amount:
        return 1
      else:
        return -1
    of none:
      return -1
  localCrafting.sort(cmp = sortCrafting)
  craftingIndexes = @[]
  for order in localCrafting:
    craftingIndexes.add(y = order.id)
  showStatistics(refresh = true)
  return tclOk

var missionsSortOrder: ListSortOrders = defaultListSortOrder

proc sortFinishedMissionsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Sort the list of finished missions
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SortFinishedMissions x
  ## X is the number of column where the player clicked the mouse button
  let column = try:
        ($argv[1]).parseInt
      except:
        return showError(message = "Can't get the column number.")
  setSortingOrder(sortingOrder = missionsSortOrder, column = column)
  if missionsSortOrder == none:
    return tclOk
  var localMissions: SortingList = @[]
  for index, mission in gameStats.finishedMissions:
    try:
      localMissions.add(y = SortingData(name: (
        case mission.index.parseInt.MissionsTypes
        of deliver:
          "Delivered items"
        of patrol:
          "Patroled areas"
        of destroy:
          "Destroyed ships"
        of explore:
          "Explored areas"
        of passenger:
          "Passengers transported"), amount: mission.amount, id: index))
    except:
      return showError(message = "Can't add local mission.")
  proc sortMissions(x, y: SortingData): int =
    case missionsSortOrder
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
    of amountAsc:
      if x.amount < y.amount:
        return 1
      else:
        return -1
    of amountDesc:
      if x.amount > y.amount:
        return 1
      else:
        return -1
    of none:
      return -1
  localMissions.sort(cmp = sortMissions)
  missionsIndexes = @[]
  for mission in localMissions:
    missionsIndexes.add(y = mission.id)
  showStatistics(refresh = true)
  return tclOk

var goalsSortOrder: ListSortOrders = defaultListSortOrder

proc sortFinishedGoalsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Sort the list of finished goals
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SortFinishedGoals x
  ## X is the number of column where the player clicked the mouse button
  let column = try:
        ($argv[1]).parseInt
      except:
        return showError(message = "Can't get the column number.")
  setSortingOrder(sortingOrder = goalsSortOrder, column = column)
  if goalsSortOrder == none:
    return tclOk
  var localGoals: SortingList = @[]
  for index, finishedGoal in gameStats.finishedGoals:
    var protoIndex = -1
    for i, goal in goalsList:
      if goal.index == finishedGoal.index:
        protoIndex = i
        break
    try:
      localGoals.add(y = SortingData(name: goalText(index = protoIndex),
          amount: finishedGoal.amount, id: index))
    except:
      return showError(message = "Can't add local goal.")
  proc sortGoals(x, y: SortingData): int =
    case goalsSortOrder
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
    of amountAsc:
      if x.amount < y.amount:
        return 1
      else:
        return -1
    of amountDesc:
      if x.amount > y.amount:
        return 1
      else:
        return -1
    of none:
      return -1
  localGoals.sort(cmp = sortGoals)
  goalsIndexes = @[]
  for goal in localGoals:
    goalsIndexes.add(y = goal.id)
  showStatistics(refresh = true)
  return tclOk

var destroyedSortOrder: ListSortOrders = defaultListSortOrder

proc sortDestroyedCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Sort the list of destroyed enemy ships
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SortDestroyedShips x
  ## X is the number of column where the player clicked the mouse button
  let column = try:
        ($argv[1]).parseInt
      except:
        return showError(message = "Can't get the column number.")
  setSortingOrder(sortingOrder = destroyedSortOrder, column = column)
  if destroyedSortOrder == none:
    return tclOk
  var localDestroyed: SortingList = @[]
  for index, destroyed in gameStats.destroyedShips:
    for i, ship in protoShipsList:
      if $i == destroyed.index:
        localDestroyed.add(y = SortingData(name: ship.name,
            amount: destroyed.amount, id: index))
        break
  proc sortDestroyed(x, y: SortingData): int =
    case destroyedSortOrder
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
    of amountAsc:
      if x.amount < y.amount:
        return 1
      else:
        return -1
    of amountDesc:
      if x.amount > y.amount:
        return 1
      else:
        return -1
    of none:
      return -1
  localDestroyed.sort(cmp = sortDestroyed)
  destroyedIndexes = @[]
  for ship in localDestroyed:
    destroyedIndexes.add(y = ship.id)
  showStatistics(refresh = true)
  return tclOk

var killedSortOrder: ListSortOrders = defaultListSortOrder

proc sortKilledCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Sort the list of killed enemies
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SortKilledEnemies x
  ## X is the number of column where the player clicked the mouse button
  let column = try:
        ($argv[1]).parseInt
      except:
        return showError(message = "Can't get the column number.")
  setSortingOrder(sortingOrder = killedSortOrder, column = column)
  if killedSortOrder == none:
    return tclOk
  var localKilled: SortingList = @[]
  for index, killed in gameStats.killedMobs:
    localKilled.add(y = SortingData(name: killed.index, amount: killed.amount, id: index))
  proc sortKilled(x, y: SortingData): int =
    case killedSortOrder
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
    of amountAsc:
      if x.amount < y.amount:
        return 1
      else:
        return -1
    of amountDesc:
      if x.amount > y.amount:
        return 1
      else:
        return -1
    of none:
      return -1
  localKilled.sort(cmp = sortKilled)
  killedIndexes = @[]
  for mob in localKilled:
    killedIndexes.add(y = mob.id)
  showStatistics(refresh = true)
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the list of available missions
  try:
    discard
#    addCommand("SortFinishedCrafting", sortFinishedCraftingCommand)
#    addCommand("SortFinishedMissions", sortFinishedMissionsCommand)
#    addCommand("SortFinishedGoals", sortFinishedGoalsCommand)
#    addCommand("SortDestroyedShips", sortDestroyedCommand)
#    addCommand("SortKilledMobs", sortKilledCommand)
  except:
    showError(message = "Can't add a Tcl command.")

# Temporary code for interfacing with Ada

proc showAdaStatistics(refresh: cint) {.raises: [], tags: [], exportc.} =
  try:
    showStatistics(refresh = refresh == 1)
  except:
    echo getCurrentExceptionMsg()

proc setAdaSortingOrder(sortingOrder: var cint; column: cint) {.raises: [],
    tags: [], exportc.} =
  var sortOrder = sortingOrder.ListSortOrders
  setSortingOrder(sortingOrder = sortOrder, column = column.Positive)
  sortingOrder = sortOrder.ord.cint
