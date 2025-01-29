# Copyright 2024-2025 Bartek thindil Jasicki
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

const
  iconsAmount: Positive = 37
  colorsAmount: Positive = 32
  fontsAmount: Positive = 2
  mapIconsAmount: Positive = 18
  mapColorsAmount: Positive = 13

type
  ThemeData* = object
    ## Stores data about the game's theme
    name: string
    fileName: string
    icons*: array[iconsAmount, string]
    colors*: array[colorsAmount, Color]
    fonts*: array[fontsAmount, string]
    mapIcons*: array[mapIconsAmount, string]
    mapColors*: array[mapColorsAmount, Color]

let
  defaultThemePath: string = dataDirectory & "ui" & DirSep
  defaultThemeIconPath: string = defaultThemePath & "images" & DirSep & "ui" & DirSep
  defaultThemeFontPath: string = defaultThemePath & "fonts" & DirSep
  defaultTheme: ThemeData = ThemeData(name: "Default theme",
      fileName: dataDirectory & "ui" & DirSep & "theme.cfg", icons: [
      dataDirectory & "ui" & DirSep & "images" & DirSep & "logo.svg",
      defaultThemeIconPath & "random.svg", defaultThemeIconPath &
      "male.svg", defaultThemeIconPath & "female.svg",
      defaultThemeIconPath & "menu.svg", defaultThemeIconPath & "fuel.svg",
      defaultThemeIconPath & "nofuel.svg", defaultThemeIconPath &
      "lowfuel.svg", defaultThemeIconPath & "food.svg",
      defaultThemeIconPath & "nofood.svg", defaultThemeIconPath &
      "lowfood.svg", defaultThemeIconPath & "drinks.svg",
      defaultThemeIconPath & "nodrinks.svg", defaultThemeIconPath &
      "lowdrinks.svg", defaultThemeIconPath & "pilot.svg",
      defaultThemeIconPath & "nopilot.svg", defaultThemeIconPath &
      "engineer.svg", defaultThemeIconPath & "noengineer.svg",
      defaultThemeIconPath & "overloaded.svg", defaultThemeIconPath &
      "gunner.svg", defaultThemeIconPath & "repair.svg",
      defaultThemeIconPath & "repair-empty.svg", defaultThemeIconPath &
      "craft.svg", defaultThemeIconPath & "craft-empty.svg",
      defaultThemeIconPath & "upgrade.svg", defaultThemeIconPath &
      "upgrade-empty.svg", defaultThemeIconPath & "crewtrader.svg",
      defaultThemeIconPath & "clean.svg", defaultThemeIconPath &
      "clean-empty.svg", defaultThemeIconPath & "arrow-up-left.svg",
      defaultThemeIconPath & "arrow-up.svg", defaultThemeIconPath &
      "arrow-up-right.svg", defaultThemeIconPath & "arrow-left.svg",
      defaultThemeIconPath & "arrow-right.svg", defaultThemeIconPath &
      "arrow-down-left.svg", defaultThemeIconPath & "arrow-down.svg",
      defaultThemeIconPath & "arrow-down-right.svg"],
      colors: ["#1a130c".parseColor, "#eee8aa".parseColor,
      "#4e9a06".parseColor, "#372412".parseColor, "#291913".parseColor,
      "#500000".parseColor, "#120d0d".parseColor, "#ffdf00".parseColor,
      "#120d0d".parseColor, "#372412".parseColor, "#1a130c".parseColor,
      "#120d0d".parseColor, "#120d0d".parseColor, "#ffdf00".parseColor,
      "#372412".parseColor, "#ffdf00".parseColor, "#ffdf00".parseColor,
      "#7f8c8d".parseColor, "#000000".parseColor, "#006400".parseColor,
      "#458588".parseColor, "#4e9a06".parseColor, "#ffdf00".parseColor,
      "#ffdf00".parseColor, "#372412".parseColor, "#500000".parseColor,
      "#ffdf00".parseColor, "#ffdf00".parseColor, "#fb4934".parseColor,
      "#7f8c8d".parseColor, "#000000".parseColor, "#b16286".parseColor],
      fonts: [defaultThemeFontPath & "Amarante-Regular.ttf",
      defaultThemeFontPath & "Hack Bold Nerd Font Complete Mono Windows Compatible.ttf"],
      mapIcons: ["\uf135", " ", "\uf05b", "\uf059", "\uf53b", "\ufc6a",
          "\uf540", "\uf707", "\uf183", "\uf51c", "\uf543", "\uf51b", "\uf5a6",
          "\uf057", "\uf0d6", "\uf197", "\uf197", "\u229b"],
      mapColors: [colBlack, "#1f2223".parseColor, colWhite,
          "#4e9a06".parseColor, "#d79921".parseColor, "#fb4934".parseColor,
          "#00ff00".parseColor, "#00ffff".parseColor, "#a40000".parseColor,
          "#732727".parseColor, "#73d216".parseColor, "#ffdf00".parseColor,
          "#b16286".parseColor])

var themesList*: Table[string, ThemeData] = initTable[string, ThemeData]() ## The list of all available themes

proc loadThemes*() {.raises: [], tags: [WriteIOEffect, TimeEffect, RootEffect,
    ReadDirEffect, ReadIOEffect, RootEffect], contractual.} =
  ## Load all the game's themes and set the configured theme for the game
  var theme: ThemeData = defaultTheme
  themesList["steamsky"] = theme
  try:
    const iconsNames: array[iconsAmount, string] = ["LogoImage", "RandomIcon",
        "MaleIcon", "FemaleIcon", "MenuIcon", "FuelIcon", "NoFuelIcon",
        "LowFuelIcon", "FoodIcon", "NoFoodIcon", "LowFoodIcon", "DrinksIcon",
        "NoDrinksIcon", "LowDrinksIcon", "PilotIcon", "NoPilotIcon",
        "EngineerIcon", "NoEnginerIcon", "OverloadedIcon", "GunnerIcon",
        "RepairIcon", "NoRepairIcon", "ManufactureIcon", "NoManufactureIcon",
        "UpgradeIcon", "NoUpgradeIcon", "TraderIcon", "CleanIcon", "NoCleanIcon",
        "ArrowUpLeft", "ArrowUp", "ArrowUpRight", "ArrowLeft", "ArrowRight",
        "ArrowDownLeft", "ArrowDown", "ArrowDownRight"]
    const colorsNames: array[colorsAmount, string] = ["BackgroundColor",
        "ForegroundColor", "GreenColor", "BorderColor", "ButtonColor",
        "ButtonHoverColor", "EditColor", "EditCursorColor", "ButtonActiveColor",
        "HeaderColor", "ComboColor", "PropertyColor", "ScrollbarColor",
        "ButtonTextColor", "ScrollbarCursorColor", "EditTextColor",
        "ComboTextColor", "TooltipBorderColor", "TooltipColor",
        "GroupBorderColor", "HeaderTextColor", "GroupTextColor",
        "SelecteActiveTextColor", "PropertyTextColor", "ToggleColor",
        "ToggleHoverColor", "ToggleCursorColor", "GoldenColor", "RedColor",
        "MapInfoBorderColor", "MapInfoColor", "PinkColor"]
    const fontsNames: array[fontsAmount, string] = ["UIFont", "MapFont"]
    const mapIconsNames: array[mapIconsAmount, string] = ["PlayerShipIcon",
        "EmptyMapIcon", "TargetIcon", "StoryIcon", "DeliverIcon", "DestroyIcon",
        "PatrolIcon", "ExploreIcon", "PassengerIcon", "EnemyShipIcon",
        "AttackOnBaseIcon", "EnemyPatrolIcon", "DiseaseIcon", "FullDocksIcon",
        "DoublePriceIcon", "MapTraderIcon", "FriendlyShipIcon", "NotVisitedBaseIcon"]
    const mapColorsNames: array[mapColorsAmount, string] = ["MapVisitedColor",
        "MapUnvisitedColor", "MapDefaultColor", "MapGreenColor",
        "MapYellowColor", "MapRedColor", "MapLimeColor", "MapCyanColor",
        "MapRed2Color", "MapRed3Color", "MapGreen2Color", "MapGoldenYellow",
        "MapPinkColor"]
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
              of fontsNames:
                let index: Natural = fontsNames.find(item = entry.value)
                theme.fonts[index] = themeDir & DirSep &
                    entry.value.unixToNativePath
              of mapIconsNames:
                let index: Natural = mapIconsNames.find(item = entry.value)
                theme.mapIcons[index] = entry.value
              of mapColorsNames:
                let index: Natural = mapColorsNames.find(item = entry.value)
                theme.mapColors[index] = entry.value.parseColor
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
  setColor(colorName = toggleColor, index = 24)
  setColor(colorName = toggleHoverColor, index = 25)
  setColor(colorName = toggleCursorColor, index = 26)
  setColor(colorName = popupBorderColor, index = 29)
  setColor(colorName = popupColor, index = 30)
  table[sliderColor] = NimColor(r: 50, g: 58, b: 61, a: 255)
  table[sliderCursorColor] = NimColor(r: 48, g: 83, b: 111, a: 245)
  table[sliderCursorHoverColor] = NimColor(r: 53, g: 88, b: 116, a: 255)
  table[sliderCursorActiveColor] = NimColor(r: 58, g: 93, b: 121, a: 255)
  table[chartColor] = NimColor(r: 50, g: 58, b: 61, a: 255)
  table[colorChartColor] = NimColor(r: 48, g: 83, b: 111, a: 255)
  table[colorChartHighlightColor] = NimColor(r: 255, g: 0, b: 0, a: 255)
  table[tabHeaderColor] = NimColor(r: 48, g: 83, b: 111, a: 255)
  styleFromTable(table = table)
