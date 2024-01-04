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

import std/[os, osproc, strutils, times]
import ../[game, halloffame, tk]
import dialogs, table

proc openLinkCommand*(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [
        ReadIOEffect, ExecIOEffect, RootEffect].} =
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
        tclEval(script = "bgerror {Can't find the program to open the link. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
  if command.len == 0:
    showMessage(text = "Can't open the link. Reason: no program to open it.",
        parentFrame = ".", title = "Can't open the link.")
    return tclOk
  try:
    discard execCmd(command = command & " " & $argv[1])
  except:
    tclEval(script = "bgerror {Can't open the link. Reason: " &
        getCurrentExceptionMsg() & "}")
  return tclOk

proc showFileCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [
    ReadDirEffect, ReadIOEffect].} =
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
      tclEval(script = "bgerror {Can't read file '" & fileName & "'. Reason: " &
          getCurrentExceptionMsg() & "}")
  else:
    tclEval(script = textView & " insert end {Can't find file to load. Did '" &
        fileName & "' file is in '" & docDirectory & "' directory?}")
  tclEval(script = textView & " configure -state disabled")
  tclEval(script = "bind . <Alt-b> {InvokeButton .showfilemenu.back}")
  tclEval(script = "bind . <Escape> {InvokeButton .showfilemenu.back}")
  return tclOk

var allNews: bool = false

proc showNewsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [
    ReadIOEffect, ReadDirEffect].} =
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
      tclEval(script = "bgerror {Can't read file 'CHANGELOG.md'. Reason: " &
          getCurrentExceptionMsg() & "}")
  else:
    tclEval(script = textView & " insert end {Can't find file to load. Did 'CHANGELOG.md' file is in '" &
        docDirectory & "' directory?}")
  tclEval(script = textView & " configure -state disabled")
  return tclOk

proc showHallOfFameCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
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

type SaveSortOrders = enum
  playerAsc, playerDesc, shipAsc, shipDesc, timeAsc, timeDesc

var
  loadTable: TableWidget
  saveSortOrder = timeDesc

proc showLoadGameCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
  if loadTable.rowHeight == 1:
    loadTable = createTable(parent = ".loadmenu.list", headers = @[
        "Player name", "Ship name", "Last saved"], command = "SortSaves",
        tooltipText = "Press mouse button to sort the saved games.")
  else:
    clearTable(table = loadTable)
  type SaveRecord = object
    playerName, shipName, saveTime, fileName: string
  var saves: seq[SaveRecord]
  for file in walkFiles(saveDirectory & "*.sav"):
    let
      (_, name, _) = splitFile(path = file)
      parts = name.split('_')
    if parts.len == 3:
      saves.add(SaveRecord(playerName: parts[0], shipName: parts[1],
          saveTime: file.getLastModificationTime.format("yyyy-MM-dd hh:mm:ss"),
          fileName: file))
    else:
      saves.add(SaveRecord(playerName: "Unknown", shipName: "Unknown",
          saveTime: file.getLastModificationTime.format("yyyy-MM-dd hh:mm:ss"),
          fileName: file))

  proc sortSaves(x, y: SaveRecord): int =
    case saveSortOrder
    of playerAsc:
      if x.playerName < y.playerName:
        return 1
      else:
        return -1
    of playerDesc:
      if x.playerName > y.playerName:
        return 1
      else:
        return -1
    of shipAsc:
      if x.shipName < y.shipName:
        return 1
      else:
        return -1
    of shipDesc:
      if x.shipName > y.shipName:
        return 1
      else:
        return -1
    of timeAsc:
      if x.saveTime < y.saveTime:
        return 1
      else:
        return -1
    of timeDesc:
      if x.saveTime > y.saveTime:
        return 1
      else:
        return -1

  echo saves
  return tclOk

proc addCommands*() =
  discard showLoadGameCommand(0, getInterp(), 0, @[])
  addCommand("OpenLink", openLinkCommand)
  addCommand("ShowFile", showFileCommand)
  addCommand("ShowNews", showNewsCommand)
  addCommand("ShowHallOfFame", showHallOfFameCommand)

# Temporary code for interfacing with Ada

proc addAdaMainMenuCommands() {.raises: [], tags: [RootEffect], exportc.} =
  try:
    addCommands()
  except:
    echo getCurrentExceptionMsg()

