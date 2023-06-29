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
      helpAction: DataAction = try:
          parseEnum[DataAction](helpNode.attr(name = "action").toLowerAscii)
        except ValueError:
          DataAction.add
    if helpAction in [update, remove]:
      if helpIndex notin helpList:
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $helpAction & " help '" & $helpIndex & "', there is no help with that index.")
    elif helpIndex in helpList:
      raise newException(exceptn = DataLoadingError,
          message = "Can't add help '" & $helpIndex & "', there is an help with that index.")
    if helpAction == DataAction.remove:
      helpList.del(key = helpIndex)
      logMessage(message = "help removed: '" & $helpIndex & "'",
          debugType = everything)
      continue
    var help: HelpData = if helpAction == DataAction.update:
        try:
          helpList[helpIndex]
        except ValueError:
          HelpData()
      else:
        HelpData()
    var attribute = helpNode.attr(name = "index")
    if attribute.len() > 0:
      help.index = attribute
