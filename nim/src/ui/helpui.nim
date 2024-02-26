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
import ../[game, help, items, tk]

proc showTopicCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the help system
  try:
    addCommand("ShowTopic", showTopicCommand)
  except:
    tclEval(script = "bgerror {Can't add a Tcl command. Reason: " &
        getCurrentExceptionMsg() & "}")

import mapsui

proc showTopicCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
  let helpView = ".help.paned.content.view"
  tclEval(script = helpView & " configure -state normal")
  tclEval(script = helpView & " delete 1.0 end")
  let topicsView = ".help.paned.topics.view"
  var newText = ""
  for entry in helpList.values:
    if entry.index == tclEval2(script = topicsView & " selection"):
      newText = entry.text
      break
  var oldIndex = 0
  type VariablesData = object
    name, value: string
  let
    variables: array[1 .. 11, VariablesData] = [VariablesData(
      name: "MoneyName", value: moneyName), VariablesData(name: "FuelName",
      value: itemsList[findProtoItem(itemType = fuelType)].name), VariablesData(
      name: "StrengthName", value: attributesList[strengthIndex].name),
      VariablesData(name: "PilotingSkill", value: skillsList[
      pilotingSkill].name), VariablesData(name: "EngineeringSkill",
      value: skillsList[engineeringSkill].name), VariablesData(
      name: "GunnerySkill", value: skillsList[gunnerySkill].name),
      VariablesData(name: "TalkingSkill", value: skillsList[talkingSkill].name),
      VariablesData(name: "PerceptionSkill", value: skillsList[
      perceptionSkill].name), VariablesData(name: "ConditionName",
      value: attributesList[conditionIndex].name), VariablesData(
      name: "DodgeSkill", value: skillsList[dodgeSkill].name), VariablesData(
      name: "UnarmedSkill", value: skillsList[unarmedSkill].name)]
    accelNames: array[1 .. 25, string] = [mapAccelerators[5], mapAccelerators[
        6], mapAccelerators[7], mapAccelerators[8], mapAccelerators[9],
        mapAccelerators[10], mapAccelerators[11], mapAccelerators[12],
        mapAccelerators[13], mapAccelerators[14], menuAccelerators[1],
        menuAccelerators[2], menuAccelerators[3], menuAccelerators[4],
        menuAccelerators[5], menuAccelerators[6], mapAccelerators[2],
        menuAccelerators[7], menuAccelerators[9], menuAccelerators[10],
        menuAccelerators[11], mapAccelerators[1], menuAccelerators[8],
        mapAccelerators[3], mapAccelerators[4]]
  while true:
    let startIndex = newText.find(sub = '{', start = oldIndex)
    if startIndex == -1:
      tclEval(script = helpView & " insert end {" & newText[oldIndex .. ^1] & "}")
      break
    tclEval(script = helpView & " insert end {" & newText[oldIndex ..
        startIndex - 1] & "}")
    let
      endIndex = newText.find(sub = '}', start = startIndex) - 1
      tagText = newText[startIndex + 1 .. endIndex]
    for variable in variables:
      if tagText == variable.name:
        tclEval(script = helpView & " insert end {" & variable.value & "} [list special]")
        break
    for index, accel in accelNames:
      if tagText == "GameKey" & $index:
        tclEval(script = helpView & " insert end {" & accel & "} [list special]")
        break
  return tclOk

# Temporary code for interfacing with Ada

proc addAdaHelpCommands() {.raises: [], tags: [RootEffect], exportc.} =
  try:
    addCommands()
  except:
    echo getCurrentExceptionMsg()
