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
# along with Steam Sky.  if, see <http://www.gnu.org/licenses/>.

## Provides code related to the game's themes' system, like default
## theme setting, etc

import std/[os, parsecfg, streams, tables]
import contracts
import ../[config, game]

type
  ThemeData* = object
    ## Stores data about the game's theme
    name: string
    fileName: string
    menuIcons*: array[4, string]
    mapIcons*: array[4, string]

let
  defaultThemeIconPath: string = dataDirectory & "ui" & DirSep & "images" &
      DirSep & "ui" & DirSep ## The path to the default theme's icons
  defaultTheme: ThemeData = ThemeData(name: "Default theme",
    fileName: dataDirectory & "ui" & DirSep & "theme.cfg", menuIcons: ["", "",
    "", ""], mapIcons: ["", "", "", ""])

var themesList*: Table[string, ThemeData] = initTable[string, ThemeData]() ## The list of all available themes

proc loadTheme*() {.raises: [], tags: [WriteIOEffect, TimeEffect, RootEffect,
    ReadDirEffect, ReadIOEffect, RootEffect], contractual.} =
  ## Set the theme for the game
  var theme: ThemeData = defaultTheme
  themesList["steamsky"] = theme
  try:
    for themeDir in walkDirs(pattern = themesDirectory):
      for configName in walkPattern(pattern = themeDir & DirSep & "*.cfg"):
        var configFile: FileStream = newFileStream(filename = configName, mode = fmRead)
        if configFile == nil:
          continue
        var parser: CfgParser = CfgParser()
        try:
          parser.open(input = configFile, filename = configName)
        except OSError, IOError, Exception:
          echo "Can't initialize configuration file parser. Reason: " &
              getCurrentExceptionMsg()
          return
        while true:
          try:
            let entry: CfgEvent = parser.next()
            case entry.kind
            of cfgEof:
              break
            of cfgKeyValuePair, cfgOption:
              case entry.key
              of "Name":
                theme.name = entry.value
              of "FileName":
                theme.fileName = themeDir & DirSep & entry.value
              else:
                discard
            of cfgError:
              echo entry.msg
            of cfgSectionStart:
              discard
          except ValueError, OSError, IOError:
            echo "Invalid data in the theme configuration file. Details: " &
                getCurrentExceptionMsg()
            continue
        try:
          parser.close()
        except OSError, IOError, Exception:
          echo "Can't close configuration file parser. Reason: " &
              getCurrentExceptionMsg()
      themesList[themeDir.lastPathPart] = theme
      theme = defaultTheme
  except:
    discard
  if gameSettings.interfaceTheme notin themesList:
    gameSettings.interfaceTheme = "steamsky"
