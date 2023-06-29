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

import std/[strutils, tables, xmlparser, xmltree]
import game, log

type HelpData = object
  index: string
  text: string

var
  helpList* = initTable[string, HelpData]()

proc loadHelp*(fileName: string) {.sideEffect, raises: [DataLoadingError],
    tags: [WriteIOEffect, ReadIOEffect, RootEffect].} =
  ## Load the help data from the file
  ##
  ## * fileName - the name of the file to load
  let helpXml = try:
      loadXml(path = fileName)
    except XmlError, ValueError, IOError, OSError, Exception:
      raise newException(exceptn = DataLoadingError,
          message = "Can't load help data file. Reason: " &
          getCurrentExceptionMsg())
  for helpNode in helpXml:
    if helpNode.kind != xnElement:
      continue
    let
      helpIndex: string =
        helpNode.attr(name = "index")
      helpTitle: string = helpNode.attr(name = "title")
      helpAction: DataAction = try:
          parseEnum[DataAction](helpNode.attr(name = "action").toLowerAscii)
        except ValueError:
          DataAction.add
    if helpAction in [update, remove]:
      if helpTitle notin helpList:
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $helpAction & " help '" & $helpTitle & "', there is no help with that title.")
    elif helpTitle in helpList:
      raise newException(exceptn = DataLoadingError,
          message = "Can't add help '" & $helpTitle & "', there is an help with that title.")
    if helpAction == DataAction.remove:
      helpList.del(key = helpTitle)
      logMessage(message = "Help removed: '" & $helpTitle & "'",
          debugType = everything)
      continue
    var helpEntry: HelpData = if helpAction == DataAction.update:
        try:
          helpList[helpTitle]
        except ValueError:
          HelpData(index: helpIndex)
      else:
        HelpData(index: helpIndex)
    var text = helpNode.innerText()
    if text.len() > 0:
      helpEntry.text = text
    if helpAction == DataAction.add:
      logMessage(message = "Help added: '" & helpTitle & "'",
          debugType = everything)
    else:
      logMessage(message = "Help updated: '" & helpTitle & "'",
          debugType = everything)
    helpList[helpTitle] = helpEntry

# Temporary code for interfacing with Ada

proc loadAdaHelp(fileName: cstring): cstring {.sideEffect, raises: [], tags: [
    WriteIOEffect, ReadIOEffect, RootEffect], exportc.} =
  try:
    loadHelp(fileName = $fileName)
    return "".cstring
  except DataLoadingError:
    return getCurrentExceptionMsg().cstring
