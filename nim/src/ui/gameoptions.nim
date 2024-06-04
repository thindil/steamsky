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

import std/[os, tables]
import ../[config, game, tk]
import coreui, mapsui, themes

proc showOptionsTabCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Show the selected options tab
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowOptionsTab
  let
    optionsCanvas = mainPaned & ".optionsframe.canvas"
    optionsFrame = optionsCanvas & ".options"
    oldFrame = tclEval2(script = "grid slaves " & optionsFrame & " -row 1")
  tclEval(script = "grid remove " & oldFrame)
  let frame = optionsFrame & "." & tclGetVar(varName = "newtab")
  tclEval(script = "grid " & frame & " -sticky nwes -padx 10")
  tclEval(script = "update")
  tclEval(script = optionsCanvas & " configure -scrollregion [list " & tclEval2(
      script = optionsCanvas & " bbox all") & "]")
  return tclOk

proc showOptionsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.exportc.} =
  tclSetVar(varName = "newtab", newValue = "general")
  var optionsFrame = mainPaned & ".optionsframe"
  let optionsCanvas = optionsFrame & ".canvas"
  type WidgetData = object
    name, value: string
  if tclEval2(script = "winfo exists " & optionsCanvas) == "0":
    tclEvalFile(fileName = dataDirectory & DirSep & "options.tcl")
    tclEval(script = "bind " & optionsFrame & " <Configure> {ResizeCanvas %W.canvas %w %h}")
    let labelsArray: array[4, WidgetData] = [WidgetData(name: "data",
        value: dataDirectory), WidgetData(name: "save", value: saveDirectory),
        WidgetData(name: "docs", value: docDirectory), WidgetData(name: "mods",
        value: modsDirectory)]
    for label in labelsArray:
      let labelName = optionsCanvas & ".options.info." & label.name
      tclEval(script = labelName & " configure -text {" & label.value & " }")
    var themesNames = ""
    for theme in themesList.values:
      themesNames &= " {" & theme.name & "}"
    let comboBox = optionsFrame & ".canvas.options.interface.theme"
    tclEval(script = comboBox & " configure -values [list" & themesNames & "]")
  elif tclEval2(script = "winfo ismapped " & optionsCanvas) == "1":
    tclEval(script = "grid remove " & closeButton)
    showSkyMap(clear = true)
    return tclOk
  optionsFrame = optionsCanvas & ".options.general"
  tclEval(script = "grid " & optionsFrame & " -sticky nwes -padx 10")
  let checkboxArray: array[11, WidgetData] = [WidgetData(name: optionsCanvas & ".options.general.autorest", value: (if gameSettings.autoRest: "1" else: "0")), WidgetData(name: optionsCanvas & ".options.general.autocenter", value: (if gameSettings.autoCenter: "1" else: "0")), WidgetData(name: optionsCanvas & ".options.general.autoreturn", value: (if gameSettings.autoReturn: "1" else: "0")), WidgetData(name: optionsCanvas & ".options.general.autofinish", value: (if gameSettings.autoFinish: "1" else: "0")), WidgetData(name: optionsCanvas & ".options.general.autoaskforbases", value: (if gameSettings.autoAskForBases: "1" else: "0")), WidgetData(name: optionsCanvas & ".options.general.autoaskforevents", value: (if gameSettings.autoAskForEvents: "1" else: "0")), WidgetData(name: optionsCanvas & ".options.interface.rightbutton", value: (if gameSettings.rightButton: "1" else: "0")), WidgetData(name: optionsCanvas & ".options.interface.showtooltips", value: (if gameSettings.showTooltips: "1" else: "0")), WidgetData(name: optionsCanvas & ".options.interface.showmessages", value: (if gameSettings.showLastMessages: "1" else: "0")), WidgetData(name: optionsCanvas & ".options.interface.fullscreen", value: (if gameSettings.fullScreen: "1" else: "0"))]
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the crew UI
  try:
    discard
#    addCommand("ShowOptionsTab", showOptionsTabCommand)
#    addCommand("ShowOptions", showOptionsCommand)
  except:
    showError(message = "Can't add a Tcl command.")
