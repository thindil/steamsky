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

import std/[colors, os, parsecfg, streams, tables]
import contracts, nuklear/nuklear_sdl_renderer, nimalyzer
import ../[config, game]

type
  ThemeData* = object
    ## Stores data about the game's theme
    name: string
    fileName: string
    icons*: array[8, string]
    colors*: array[24, Color]

let
  defaultThemeIconPath: string = dataDirectory & "ui" & DirSep & "images" &
      DirSep & "ui" & DirSep ## The path to the default theme's icons
  defaultTheme: ThemeData = ThemeData(name: "Default theme",
      fileName: dataDirectory & "ui" & DirSep & "theme.cfg", icons: [
      dataDirectory & "ui" & DirSep & "images" & DirSep & "logo.svg",
      defaultThemeIconPath & "random.svg", defaultThemeIconPath & "male.svg",
      defaultThemeIconPath & "female.svg", defaultThemeIconPath & "menu.svg",
      defaultThemeIconPath & "fuel.svg", defaultThemeIconPath & "nofuel.svg",
      defaultThemeIconPath & "lowfuel.svg"], colors: [parseColor(
      name = "#1a130c"), parseColor(name = "#eee8aa"), parseColor(
      name = "#4e9a06"), parseColor(name = "#372412"), parseColor(
      name = "#291913"), parseColor(name = "#500000"), parseColor(
      name = "#120d0d"), parseColor(name = "#ffdf00"), parseColor(
      name = "#120d0d"), parseColor(name = "#372412"), parseColor(
      name = "#1a130c"), parseColor(name = "#120d0d"), parseColor(
      name = "#120d0d"), parseColor(name = "#ffdf00"), parseColor(
      name = "#372412"), parseColor(name = "#ffdf00"), parseColor(
      name = "#ffdf00"), parseColor(name = "#7f8c8d"), parseColor(
      name = "#000000"), parseColor(name = "#006400"), parseColor(
      name = "#458588"), parseColor(name = "#4e9a06"), parseColor(
      name = "#ffdf00"), parseColor(name = "#ffdf00")])

var themesList*: Table[string, ThemeData] = initTable[string, ThemeData]() ## The list of all available themes

proc loadThemes*() {.raises: [], tags: [WriteIOEffect, TimeEffect, RootEffect,
    ReadDirEffect, ReadIOEffect, RootEffect], contractual.} =
  ## Load all the game's themes and set the configured theme for the game
  var theme: ThemeData = defaultTheme
  themesList["steamsky"] = theme
  try:
    const iconsNames: array[8, string] = ["LogoImage", "RandomIcon", "MaleIcon",
        "FemaleIcon", "MenuIcon", "FuelIcon", "NofuelIcon", "LowfuelIcon"]
    const colorsNames: array[24, string] = ["BackgroundColor",
        "ForegroundColor", "GreenColor", "BorderColor", "ButtonColor",
        "ButtonHoverColor", "EditColor", "EditCursorColor", "ButtonActiveColor",
        "HeaderColor", "ComboColor", "PropertyColor", "ScrollbarColor",
        "ButtonTextColor", "ScrollbarCursorColor", "EditTextColor",
        "ComboTextColor", "TooltipBorderColor", "TooltipColor",
        "GroupBorderColor", "HeaderTextColor", "GroupTextColor",
        "SelecteActiveTextColor", "PropertyTextColor"]
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
              of iconsNames:
                let index: Natural = iconsNames.find(item = entry.value)
                theme.icons[index] = themeDir & DirSep &
                    entry.value.unixToNativePath
              of colorsNames:
                let index: Natural = colorsNames.find(item = entry.value)
                theme.colors[index] = parseColor(name = entry.value)
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
  {.ruleOff: "varDeclared".}
  var table: array[countColors, NimColor]
  {.ruleOn: "varDeclared".}

  proc setColor(colorName: StyleColors; index: Natural) {.raises: [], tags: [],
      contractual.} =
    ## Convert the selected color to Nuklear color
    ##
    ## * colorName - the Nuklear's theme's color name to which will be converted
    ## * index     - the index of the color from the game's current theme
    let (r, g, b) = try:
        extractRGB(a = themesList[gameSettings.interfaceTheme].colors[index])
      except:
        echo "Can't set the theme's color."
        return
    table[colorName] = NimColor(r: r, g: g, b: b, a: 255)

  setColor(colorName = windowColor, index = 0)
  setColor(colorName = textColor, index = 1)
  setColor(colorName = borderColor, index = 3)
  setColor(colorName = buttonColor, index = 4)
  setColor(colorName = buttonHoverColor, index = 5)
  setColor(colorName = editColor, index = 6)
  setColor(colorName = editCursorColor, index = 15)
  setColor(colorName = buttonActiveColor, index = 8)
  setColor(colorName = headerColor, index = 9)
  setColor(colorName = comboColor, index = 10)
  setColor(colorName = propertyColor, index = 11)
  setColor(colorName = scrollbarColor, index = 12)
  setColor(colorName = buttonTextColor, index = 13)
  setColor(colorName = buttonHoverTextColor, index = 1)
  setColor(colorName = buttonActiveTextColor, index = 1)
  setColor(colorName = scrollbarCursorColor, index = 14)
  setColor(colorName = scrollbarCursorHoverColor, index = 14)
  setColor(colorName = scrollbarCursorActiveColor, index = 14)
  setColor(colorName = editTextColor, index = 15)
  setColor(colorName = comboTextColor, index = 16)
  setColor(colorName = tooltipBorderColor, index = 17)
  setColor(colorName = tooltipColor, index = 18)
  setColor(colorName = groupBorderColor, index = 19)
  setColor(colorName = headerTextColor, index = 20)
  setColor(colorName = groupTextColor, index = 21)
  setColor(colorName = selectColor, index = 0)
  setColor(colorName = selectActiveColor, index = 5)
  setColor(colorName = selectActiveTextColor, index = 22)
  setColor(colorName = propertyTextColor, index = 23)
  table[toggleColor] = NimColor(r: 50, g: 58, b: 61, a: 255)
  table[toggleHoverColor] = NimColor(r: 45, g: 53, b: 56, a: 255)
  table[toggleCursorColor] = NimColor(r: 48, g: 83, b: 111, a: 255)
  table[sliderColor] = NimColor(r: 50, g: 58, b: 61, a: 255)
  table[sliderCursorColor] = NimColor(r: 48, g: 83, b: 111, a: 245)
  table[sliderCursorHoverColor] = NimColor(r: 53, g: 88, b: 116, a: 255)
  table[sliderCursorActiveColor] = NimColor(r: 58, g: 93, b: 121, a: 255)
  table[chartColor] = NimColor(r: 50, g: 58, b: 61, a: 255)
  table[colorChartColor] = NimColor(r: 48, g: 83, b: 111, a: 255)
  table[colorChartHighlightColor] = NimColor(r: 255, g: 0, b: 0, a: 255)
  table[tabHeaderColor] = NimColor(r: 48, g: 83, b: 111, a: 255)
  styleFromTable(table = table)
