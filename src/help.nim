# Copyright 2023-2025 Bartek thindil Jasicki
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

## Provides code related to in-game help system, like load it from files.

import std/[logging, paths, strutils, tables, xmlparser, xmltree]
import contracts
import basestypes, careers, game, log, types

type HelpData = object
  index*: string
  text*: string

var helpList*: OrderedTable[string, HelpData] = initOrderedTable[string,
    HelpData]() ## The whole in-game help

proc loadHelp*(fileName: Path) {.raises: [DataLoadingError,
    KeyError], tags: [WriteIOEffect, ReadIOEffect, RootEffect], contractual.} =
  ## Load the help data from the file
  ##
  ## * fileName - the name of the file to load
  require:
    ($fileName).len > 0
  body:
    var
      helpTitle: string = ""
      helpEntry: HelpData = HelpData()
    let helpXml: XmlNode = try:
        loadXml(path = $fileName)
      except XmlError, ValueError, IOError, OSError, Exception:
        raise newException(exceptn = DataLoadingError,
            message = "Can't load help data file. Reason: " &
            getCurrentExceptionMsg())
    for helpNode in helpXml:
      if helpNode.kind != xnElement:
        continue
      let helpIndex: string = helpNode.attr(name = "index")
      helpTitle = helpNode.attr(name = "title")
      let helpAction: DataAction = try:
            parseEnum[DataAction](s = helpNode.attr(
                name = "action").toLowerAscii)
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
            messageLevel = lvlInfo)
        continue
      helpEntry = if helpAction == DataAction.update:
          try:
            helpList[helpTitle]
          except ValueError:
            HelpData(index: helpIndex)
        else:
          HelpData(index: helpIndex)
      var text: string = helpNode.innerText()
      if text.len() > 0:
        helpEntry.text = text
      if helpAction == DataAction.add:
        logMessage(message = "Help added: '" & helpTitle & "'",
            messageLevel = lvlInfo)
      else:
        logMessage(message = "Help updated: '" & helpTitle & "'",
            messageLevel = lvlInfo)
      helpList[helpTitle] = helpEntry
    # Add help page about available statistics and attributes
    helpEntry.index = "stats"
    helpTitle = $(helpList.len + 1) & ". Attributes and skills"
    helpEntry.text = "Here you will find information about all available attributes and skills in the game\n\n{u}Attributes{/u}\n\n"
    for attribute in attributesList:
      helpEntry.text.add(y = "{b}" & attribute.name & "{/b}\n    " &
          attribute.description & "\n\n")
    helpEntry.text.add(y = "\n{u}Skills{/u}\n\n")
    for skill in skillsList.values:
      helpEntry.text.add(y = "{b}" & skill.name &
          "{/b}\n    {i}Related attribute:{/i} " & attributesList[
          skill.attribute].name & "\n")
      for item in itemsList.values:
        if item.itemType == skill.tool:
          helpEntry.text.add(y = "   {i}Training tool:{/i} " & (
              if item.showType.len == 0: item.itemType else: item.showType) & "\n")
          break
      helpEntry.text.add(y = "    " & skill.description & "\n\n")
    helpList[helpTitle] = helpEntry
    logMessage(message = "Help added: '" & helpTitle & "'",
        messageLevel = lvlInfo)
    # Add help page about available careers and factions
    helpEntry.index = "factions"
    helpTitle = $(helpList.len + 1) & ". Factions and careers"
    helpEntry.text = "Here you will find information about all available factions and careers in the game\n\n{u}Factions{/u}\n\n"
    for faction in factionsList.values:
      if faction.careers.len > 0:
        helpEntry.text.add(y = "{b}" & faction.name & "{/b}\n    " &
            faction.description[0..faction.description.rfind(sub = '\n') - 1] & "\n    {i}Relations{/i}\n")
        for index, relation in faction.relations:
          helpEntry.text.add(y = "        " & factionsList[index].name & ": " &
              (if relation.friendly: "Friendly" else: "Enemies") & "\n")
        helpEntry.text.add(y = "\n")
    helpEntry.text.add(y = "\n{u}Careers{/u}\n\n")
    for index, career in careersList:
      helpEntry.text.add(y = "{b}" & career.name & "{/b}\n" & factionsList[
          "POLEIS"].careers[index].description & "\n")
      if career.skills.len > 0:
        helpEntry.text.add(y = "    {i}Bonus to skills{/i}\n")
        for skill in career.skills:
          helpEntry.text.add(y = "        " & skill & "\n")
      helpEntry.text.add(y = "\n")
    helpList[helpTitle] = helpEntry
    logMessage(message = "Help added: '" & helpTitle & "'",
        messageLevel = lvlInfo)
    # Add help page about available bases types
    helpEntry.index = "basestypes"
    helpTitle = $(helpList.len + 1) & ". Bases Types"
    helpEntry.text = "Here you will find information about all available bases types in the game\n\n"
    for baseType in basesTypesList.values:
      helpEntry.text.add(y = "{b}" & baseType.name & "{/b}\n    " &
          baseType.description & "\n\n")
    helpList[helpTitle] = helpEntry
    logMessage(message = "Help added: '" & helpTitle & "'",
        messageLevel = lvlInfo)
