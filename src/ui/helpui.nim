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

import std/[os, strutils, tables]
import ../[basestypes, config, game, help, items, tk]
import dialogs, errordialog, themes

proc showTopicCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [].}
  ## Show the content of the selected topic help
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowTopic

proc closeHelpCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect, TimeEffect].} =
  ## Destroy the help window and save the sash position to the game
  ## configuration
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## CloseHelp
  let
    helpWindow = ".help"
    paned = helpWindow & ".paned"
    topicPosition = tclEval2(script = paned & " sashpos 0")
  gameSettings.topicsPosition = try:
      topicPosition.parseInt
    except:
      return showError(message = "Can't set topic position.")
  tclEval(script = "destroy " & helpWindow)
  return tclOk

proc showHelpCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect, TimeEffect].} =
  ## Show the help window to the player
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowHelp topicindex
  ## Topicindex is the index of the help topic which content will be show
  let helpWindow = ".help"
  if tclEval2(script = "winfo exists " & helpWindow) == "1":
    return closeHelpCommand(clientData = clientData, interp = interp,
        argc = argc, argv = argv)
  tclEvalFile(fileName = dataDirectory & "ui" & DirSep & "help.tcl")
  let
    paned = helpWindow & ".paned"
    helpView = paned & ".content.view"
  let theme = try:
        themesList[gameSettings.interfaceTheme]
      except:
        return showError(message = "Can't find theme '" &
            gameSettings.interfaceTheme & "'")
  tclEval(script = helpView & " tag configure special -foreground {" &
      theme.specialHelpColor & "} -font BoldHelpFont")
  tclEval(script = helpView & " tag configure underline -foreground {" &
      theme.underlineHelpColor & "} -font UnderlineHelpFont")
  tclEval(script = helpView & " tag configure bold -foreground {" &
      theme.boldHelpColor & "} -font BoldHelpFont")
  tclEval(script = helpView & " tag configure italic -foreground {" &
      theme.italicHelpColor & "} -font ItalicHelpFont")
  var x = try:
        ((tclEval2(script = "winfo vrootwidth " & helpWindow).parseInt -
            gameSettings.windowWidth) / 2).int
      except:
        return showError(message = "Can't count X position of help window.")
  if x < 0:
    x = 0
  var y = try:
        ((tclEval2(script = "winfo vrootheight " & helpWindow).parseInt -
            gameSettings.windowHeight) / 2).int
      except:
        return showError(message = "Can't count Y position of help window.")
  if y < 0:
    y = 0
  tclEval(script = "wm geometry " & helpWindow & " " &
      $gameSettings.windowWidth & "x" & $gameSettings.windowHeight & "+" & $x &
      "+" & $y)
  tclEval(script = "update")
  tclEval(script = paned & " sashpos 0 " & $gameSettings.topicsPosition)
  let topicsView = paned & ".topics.view"
  for title, help in helpList:
    tclEval(script = topicsView & " insert {} end -id {" & help.index &
        "} -text {" & title & "}")
  tclEval(script = "bind " & topicsView & " <<TreeviewSelect>> {ShowTopic}")
  let topicIndex: string = (if argc == 1: tclGetVar(
      varName = "gamestate") else: $argv[1])
  if tclEval2(script = topicsView & " exists " & topicIndex) == "0":
    showMessage(text = "The selected help topic doesn't exist. Showing the first avaiable instead.",
        parentFrame = ".help", title = "Can't find help topic")
    for help in helpList.values:
      tclEval(script = topicsView & " selection set " & help.index)
      break
    return tclOk
  tclEval(script = topicsView & " selection set " & topicIndex)
  tclEval(script = "update")
  tclEval(script = topicsView & " see " & topicIndex)
  return tclOk

proc addCommands*() {.raises: [], tags: [WriteIOEffect, TimeEffect].} =
  ## Adds Tcl commands related to the help system
  try:
    addCommand("ShowTopic", showTopicCommand)
    addCommand("CloseHelp", closeHelpCommand)
    addCommand("ShowHelp", showHelpCommand)
  except:
    showError(message = "Can't add a Tcl command.")

import mapsui

proc showTopicCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults =
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
  type
    VariablesData = object
      name, value: string
    FontTag = object
      tag, textTag: string
  let
    variables: array[1 .. 11, VariablesData] = try:
        [VariablesData(name: "MoneyName", value: moneyName), VariablesData(
            name: "FuelName", value: itemsList[findProtoItem(
            itemType = fuelType)].name), VariablesData(name: "StrengthName",
            value: attributesList[strengthIndex].name), VariablesData(
            name: "PilotingSkill", value: skillsList[pilotingSkill].name),
            VariablesData(name: "EngineeringSkill", value: skillsList[
            engineeringSkill].name), VariablesData(name: "GunnerySkill",
            value: skillsList[gunnerySkill].name), VariablesData(
            name: "TalkingSkill", value: skillsList[talkingSkill].name),
            VariablesData(name: "PerceptionSkill", value: skillsList[
            perceptionSkill].name), VariablesData(name: "ConditionName",
            value: attributesList[conditionIndex].name), VariablesData(
            name: "DodgeSkill", value: skillsList[dodgeSkill].name),
            VariablesData(name: "UnarmedSkill", value: skillsList[
            unarmedSkill].name)]
      except:
        tclEval(script = "bgerror {Can't set help variables. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    accelNames: array[1 .. 25, string] = [mapAccelerators[5], mapAccelerators[
        6], mapAccelerators[7], mapAccelerators[8], mapAccelerators[9],
        mapAccelerators[10], mapAccelerators[11], mapAccelerators[12],
        mapAccelerators[13], mapAccelerators[14], menuAccelerators[1],
        menuAccelerators[2], menuAccelerators[3], menuAccelerators[4],
        menuAccelerators[5], menuAccelerators[6], mapAccelerators[2],
        menuAccelerators[7], menuAccelerators[9], menuAccelerators[10],
        menuAccelerators[11], mapAccelerators[1], menuAccelerators[8],
        mapAccelerators[3], mapAccelerators[4]]
    fontTags: array[1 .. 3, FontTag] = [FontTag(tag: "b", textTag: "bold"),
        FontTag(tag: "u", textTag: "underline"), FontTag(tag: "i",
        textTag: "italic")]
    flagsTags: array[1 .. 8, string] = ["diseaseimmune", "nofatigue",
        "nomorale", "naturalarmor", "toxicattack", "sentientships",
        "fanaticism", "loner"]
    basesFlags: array[1 .. 4, string] = ["shipyard", "temple", "blackmarket", "barracks"]
  while true:
    var startIndex = newText.find(sub = '{', start = oldIndex)
    if startIndex == -1:
      tclEval(script = helpView & " insert end {" & newText[oldIndex .. ^1] & "}")
      break
    tclEval(script = helpView & " insert end {" & newText[oldIndex ..
        startIndex - 1] & "}")
    var endIndex = newText.find(sub = '}', start = startIndex) - 1
    let tagText = newText[startIndex + 1 .. endIndex]
    for variable in variables:
      if tagText == variable.name:
        tclEval(script = helpView & " insert end {" & variable.value & "} [list special]")
        break
    for index, accel in accelNames:
      if tagText == "GameKey" & $index:
        tclEval(script = helpView & " insert end {" & accel & "} [list special]")
        break
    for tag in fontTags:
      if tagText == tag.tag:
        startIndex = newText.find(sub = '{', start = endIndex) - 1
        tclEval(script = helpView & " insert end {" & newText[endIndex + 2 ..
            startIndex] & "} [list " & tag.textTag & "]")
        endIndex = newText.find(sub = '}', start = startIndex) - 1
        break
    for tag in flagsTags:
      if tagText == tag:
        var factionsWithFlag = ""
        for faction in factionsList.values:
          if tagText in faction.flags:
            if factionsWithFlag.len > 0:
              factionsWithFlag.add(", ")
            factionsWithFlag.add(faction.name)
        factionsWithFlag.removeSuffix(", ")
        tclEval(script = helpView & " insert end {" & factionsWithFlag & "}")
        break
    for tag in basesFlags:
      if tagText != tag:
        continue
      var basesWithFlag = ""
      for baseType in basesTypesList.values:
        if tagText in baseType.flags:
          if basesWithFlag.len > 0:
            basesWithFlag.add(", ")
          basesWithFlag.add(baseType.name)
      basesWithFlag.removeSuffix(", ")
      tclEval(script = helpView & " insert end {" & basesWithFlag & "}")
      break
    oldIndex = endIndex + 2
  tclEval(script = helpView & " configure -state disabled")
  return tclOk
