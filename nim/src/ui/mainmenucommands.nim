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

import std/[os, osproc, strutils]
import ../[config, events, game, halloffame, tk]
import dialogs, mapsui

proc openLinkCommand*(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
        ReadIOEffect, ExecIOEffect, RootEffect], exportc.} =
  ## Open the selected link in a proper program
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## OpenLink url
  ## Url is link which will be opened
  let command = try:
        findExe(exe = (if hostOs == "windows": "start" elif hostOs ==
          "macosx": "open" else: "xdg-open"))
      except:
        return showError(message = "Can't find the program to open the link")
  if command.len == 0:
    showMessage(text = "Can't open the link. Reason: no program to open it.",
        parentFrame = ".", title = "Can't open the link.")
    return tclOk
  try:
    discard execCmd(command = command & " " & $argv[1])
  except:
    showError(message = "Can't open the link")
  return tclOk

proc showFileCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
    ReadDirEffect, ReadIOEffect], exportc.} =
  ## Show the selected file content
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowFile filename
  ## Filename is the name of the file in the documentation directory which
  ## will be show
  let textView = ".showfilemenu.text"
  tclEval(script = textView & " configure -state normal")
  tclEval(script = textView & " delete 1.0 end")
  let fileName = $argv[1]
  if fileExists(filename = docDirectory & fileName):
    try:
      for line in lines(docDirectory & fileName):
        tclEval(script = textView & " insert end {" & line & "\n}")
    except:
      showError(message = "Can't read file '" & fileName & "'.")
  else:
    tclEval(script = textView & " insert end {Can't find file to load. Did '" &
        fileName & "' file is in '" & docDirectory & "' directory?}")
  tclEval(script = textView & " configure -state disabled")
  tclEval(script = "bind . <Alt-b> {InvokeButton .showfilemenu.back}")
  tclEval(script = "bind . <Escape> {InvokeButton .showfilemenu.back}")
  return tclOk

var allNews: bool = false

proc showNewsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
    ReadIOEffect, ReadDirEffect], exportc.} =
  ## Show the list of changes in the game, all or just recent, since the last
  ## release
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowNews boolean
  ## If boolean is true, show all news, otherwise only recent
  let allNewsButton = ".newsmenu.showall"
  if argv[1] == "false":
    allNews = false
    tclEval(script = allNewsButton & " configure -text {Show all changes} -command {ShowNews true}")
    tclEval(script = "tooltip::tooltip " & allNewsButton & " \"Show all changes to the game since previous big stable version\"")
  else:
    allNews = true
    tclEval(script = allNewsButton & " configure -text {Show only newest changes} -command {ShowNews false}")
    tclEval(script = "tooltip::tooltip " & allNewsButton & " \"Show only changes to the game since previous relese\"")
  let textView = ".newsmenu.text"
  tclEval(script = textView & " configure -state normal")
  tclEval(script = textView & " delete 1.0 end")
  if fileExists(filename = docDirectory & "CHANGELOG.md"):
    try:
      var index = 0
      for line in lines(docDirectory & "CHANGELOG.md"):
        index.inc
        if index < 6:
          continue
        if (not allNews) and line.len > 1 and line[0 .. 2] == "## ":
          break
        tclEval(script = textView & " insert end {" & line & "\n}")
    except:
      showError(message = "Can't read file 'CHANGELOG.md'")
  else:
    tclEval(script = textView & " insert end {Can't find file to load. Did 'CHANGELOG.md' file is in '" &
        docDirectory & "' directory?}")
  tclEval(script = textView & " configure -state disabled")
  return tclOk

proc showHallOfFameCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Show the hall of fame screen
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowHallOfFame
  let hofView = ".hofmenu.view"
  tclEval(script = hofView & " delete [list [" & hofView & " children {}]]")
  for index, entry in hallOfFameArray:
    if entry.points == 0:
      break
    tclEval(script = hofView & " insert {} end -values [list " & $index & " " &
        entry.name & " " & $entry.points & " " & entry.deathReason & "]")
  return tclOk

proc deleteGameCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Delete a saved game file
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## DeleteGame file
  ## File is the name of the saved game to delete
  tclSetVar(varName = "deletesave", newValue = $argv[1])
  showQuestion(question = "Are you sure you want delete this savegame?",
      res = "deletesave", inGame = false)
  return tclOk

proc startGame() {.sideEffect, raises: [], tags: [WriteIOEffect, ReadIOEffect,
    RootEffect].} =
  let mainWindow = "."
  var x: int = try:
      ((tclEval2(script = "winfo vrootwidth " & mainWindow).parseInt -
        gameSettings.windowWidth) / 2).int
    except:
      showError(message = "Can't get window X position")
      return
  if x < 0:
    x = 0
  var y: int = try:
      ((tclEval2(script = "winfo vrootheight " & mainWindow).parseInt -
        gameSettings.windowHeight) / 2).int
    except:
      showError(message = "Can't get window Y position")
      return
  if y < 0:
    y = 0
  tclEval(script = "wm geometry " & $gameSettings.windowWidth & "x" &
      $gameSettings.windowHeight & "+" & $x & "+" & $y)
  try:
    generateTraders()
  except:
    showError(message = "Can't generate traders")
  createGameUi()

proc addCommands*() =
  discard
#  addCommand("OpenLink", openLinkCommand)
#  addCommand("ShowFile", showFileCommand)
#  addCommand("ShowNews", showNewsCommand)
#  addCommand("ShowHallOfFame", showHallOfFameCommand)
#  addCommand("DeleteGame", deleteGameCommand)
