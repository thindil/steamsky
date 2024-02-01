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

import std/[os, strformat, tables]
import ../[game, statistics, tk]
import coreui, utilsui2

var craftingIndexes: seq[Positive]

proc showStatistics*(refresh: bool = false) =
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
  statsText = "Bases visited: " & $gameStats.basesVisited & "(" &
      fmt"{visitedPercent:5.3f}" & "%)"
  label = statsCanvas & ".stats.left.bases"
  tclEval(script = label & " configure -text {" & statsText & "}")
  tclEval(script = "tooltip::tooltip " & label & " \"The amount of sky bases visited and total percentage of all bases\"")
  visitedPercent = (gameStats.mapVisited.float / (1_024.0 * 1_024.0)) * 100.0
  if visitedPercent < 0.001:
    visitedPercent = 0.001
  statsText = "Map discovered: " & fmt"{visitedPercent:5.3f}" & "%"
  label = statsCanvas & ".stats.left.map"
  tclEval(script = label & " configure -text {" & statsText & "}")
  tclEval(script = "tooltip::tooltip " & label & " \"The amount of unique map's fields visited\"")
  statsText = "Distance traveled: " & $gameStats.distanceTraveled
  label = statsCanvas & ".stats.left.distance"
  tclEval(script = label & " configure -text {" & statsText & "}")
  tclEval(script = "tooltip::tooltip " & label & " \"The total amount of map's fields visited\"")
  var
    totalFinished = 0
    statsList = gameStats.craftingOrders
  for craftingOrder in statsList:
    totalFinished = totalFinished + craftingOrder.amount
  label = statsFrame & ".left.crafts"
  tclEval(script = label & " configure -text {Crafting orders finished: " &
      $totalFinished & "}")
  tclEval(script = "tooltip::tooltip " & label & " \"The total amount of crafting orders finished in this game\"")
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
      tclEval(script = treeView & " insert {} end -values [list {" & itemsList[
          recipesList[statsList[item].index].resultIndex].name &
          "} {" & $statsList[item].amount & "}]")
    tclEval(script = treeView & " configure -height " & (if statsList.len <
        10: $statsList.len else: "10"))
    tclEval(script = "grid " & statsFrame)
  else:
    tclEval(script = "grid " & statsFrame)
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
