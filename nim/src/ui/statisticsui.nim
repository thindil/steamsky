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

import std/[os, strformat, strutils, tables]
import ../[game, goals, statistics, tk, types]
import coreui, utilsui2

var craftingIndexes, missionsIndexes, goalsIndexes, destroyedIndexes,
  killedIndexes: seq[Natural]

proc showStatistics*(refresh: bool = false) {.sideEffect, raises: [], tags: [].} =
  ## Show the game statistics to the player
  ##
  ## * refresh - if true, refresh the view, otherwise back to the sky map
  var statsFrame = mainPaned & ".statsframe"
  let statsCanvas = statsFrame & ".canvas"
  var label = statsCanvas & ".stats.left.points"
  if tclEval2(script = "winfo exists " & label) == "0":
    tclEvalFile(fileName = dataDirectory & "ui" & DirSep & "stats.tcl")
    tclEval(script = "bind " & statsFrame & " <Configure> {ResizeCanvas %W.canvas %w %h}")
  elif tclEval2(script = "winfo ismapped " & label) == "1" and not refresh:
    tclEval(script = "InvokeButton " & closeButton)
    return
  tclEval(script = label & " configure -text {Points: " & $getGamePoints() & "}")
  tclEval(script = "tooltip::tooltip " & label & " \"The amount of points gained in this game\"")
  var statsText = "Time passed:"
  let minutesDiff = (gameDate.minutes + (gameDate.hour * 60) + (gameDate.day *
      1_440) + (gameDate.month * 43_200) + (gameDate.year * 518_400)) - 829_571_520
  minutesToDate(minutes = minutesDiff, infoText = statsText)
  label = statsCanvas & ".stats.left.time"
  tclEval(script = label & " configure -text {" & statsText & "}")
  tclEval(script = "tooltip::tooltip " & label & " \"In game time which was passed since it started\"")
  var visitedPercent: float = (gameStats.basesVisited.float / 1_024.0) * 100.0
  statsText = try:
      "Bases visited: " & $gameStats.basesVisited & " (" &
         fmt"{visitedPercent:5.3f}" & "%)"
    except:
      tclEval(script = "bgerror {Can't show info about visited bases. Reason: " &
          getCurrentExceptionMsg() & "}")
      return
  label = statsCanvas & ".stats.left.bases"
  tclEval(script = label & " configure -text {" & statsText & "}")
  tclEval(script = "tooltip::tooltip " & label & " \"The amount of sky bases visited and total percentage of all bases\"")
  visitedPercent = (gameStats.mapVisited.float / (1_024.0 * 1_024.0)) * 100.0
  if visitedPercent < 0.001:
    visitedPercent = 0.001
  statsText = try:
      "Map discovered: " & fmt"{visitedPercent:5.3f}" & "%"
    except:
      tclEval(script = "bgerror {Can't show info about discovered map. Reason: " &
          getCurrentExceptionMsg() & "}")
      return
  label = statsCanvas & ".stats.left.map"
  tclEval(script = label & " configure -text {" & statsText & "}")
  tclEval(script = "tooltip::tooltip " & label & " \"The amount of unique map's fields visited\"")
  statsText = "Distance traveled: " & $gameStats.distanceTraveled
  label = statsCanvas & ".stats.left.distance"
  tclEval(script = label & " configure -text {" & statsText & "}")
  tclEval(script = "tooltip::tooltip " & label & " \"The total amount of map's fields visited\"")
  statsFrame = statsCanvas & ".stats"
  var
    totalFinished = 0
    statsList = gameStats.craftingOrders
  for craftingOrder in statsList:
    totalFinished = totalFinished + craftingOrder.amount
  label = statsFrame & ".left.crafts"
  tclEval(script = label & " configure -text {Crafting orders finished: " &
      $totalFinished & "}")
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
        tclEval(script = "bgerror {Can't show finished crafting orders. Reason: " &
            getCurrentExceptionMsg() & "}")
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
  label = statsCanvas & ".stats.left.missions"
  var missionsPercent = 0
  if gameStats.acceptedMissions > 0:
    missionsPercent = ((totalFinished.float /
        gameStats.acceptedMissions.float) * 100.0).int
  tclEval(script = label & " configure -text {Missions completed: " &
      $totalFinished & " (" & $missionsPercent & "%)}")
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
        tclEval(script = "bgerror {Can't show finished missions. Reason: " &
            getCurrentExceptionMsg() & "}")
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
    tclEval(script = "bgerror {Can't show the current goal. Reason: " &
        getCurrentExceptionMsg() & "}")
    return
  totalFinished = 0
  statsList = gameStats.finishedGoals
  for finishedGoal in statsList:
    totalFinished = totalFinished + finishedGoal.amount
  label = statsCanvas & ".stats.left.goals"
  tclEval(script = label & " configure -text {Finished goals: " &
      $totalFinished & "}")
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
        tclEval(script = "bgerror {Can't show finished goals. Reason: " &
            getCurrentExceptionMsg() & "}")
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
  label = statsCanvas & ".stats.right.destroyed"
  tclEval(script = label & " configure -text {Destroyed ships (Total: " &
      $totalDestroyed & ")}")
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
  label = statsCanvas & ".stats.right.killed"
  tclEval(script = label & " configure -text {Killed enemies (Total: " &
      $totalDestroyed & ")}")
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

# Temporary code for interfacing with Ada

proc showAdaStatistics(refresh: cint) {.raises: [], tags: [], exportc.} =
  try:
    showStatistics(refresh = refresh == 1)
  except:
    echo getCurrentExceptionMsg()
