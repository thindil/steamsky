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

import std/[colors, os, parsecfg, streams, strutils, tables]
import contracts, nuklear/nuklear_sdl_renderer, nimalyzer
import ../[config, game]

type
  ColorsNames* = enum
    ## Names of the colors used in the game's themes
    backgroundColor, foregroundColor, greenColor, borderColor, buttonColor,
      buttonHoverColor, editColor, editCursorColor, buttonActiveColor,
      headerColor, comboColor, propertyColor, scrollbarColor, buttonTextColor,
      scrollbarCursorColor, editTextColor, comboTextColor, tooltipBorderColor,
      tooltipColor, groupBorderColor, headerTextColor, groupTextColor,
      selectActiveTextColor, propertyTextColor, toggleColor, toggleHoverColor,
      toggleCursorColor, goldenColor, redColor, mapInfoBorderColor,
      mapInfoColor, pinkColor, yellowColor, blueColor, cyanColor, grayColor,
      tableRowColor, tableTextColor, sliderEmptyColor, sliderFillColor,
      sliderCursorColor, progressbarColor
  MapColorsNames* = enum
    ## Names of colors used in the game's map
    mapVisitedColor, mapUnvisitedColor, mapDefaultColor, mapGreenColor,
      mapYellowColor, mapRedColor, mapLimeColor, mapCyanColor, mapRed2Color,
      mapRed3Color, mapGreen2Color, mapGoldenYellow, mapPinkColor
  FontsNames* = enum
    ## Names of fonts used in the game's themes
    UIFont, mapFont
  IconsNames* = enum
    ## Names of icons used in the game
    logoImage, randomIcon, maleIcon, femaleIcon, menuIcon, fuelIcon, noFuelIcon,
      lowFuelIcon, foodIcon, noFoodIcon, lowFoodIcon, drinksIcon, noDrinksIcon,
      lowDrinksIcon, pilotIcon, noPilotIcon, engineerIcon, noEngineerIcon,
      overloadedIcon, gunnerIcon, repairIcon, noRepairIcon, manufactureIcon,
      noManufactureIcon, upgradeIcon, noUpgradeIcon, traderIcon, cleanIcon,
      noCleanIcon, arrowUpLeft, arrowUp, arrowUpRight, arrowLeft, arrowRight,
      arrowDownLeft, arrowDown, arrowDownRight, waitIcon, moveToIcon,
      moveStepIcon, expandIcon, contractIcon, selectAllIcon, unselectAllIcon,
      cancelIcon, assignCrewIcon, exitIcon, moreOptionsIcon, contract2Icon,
      expand2Icon, buyDefaultIcon, sellDefaultIcon, buyIcon, sellIcon,
      negotiateIcon, helpIcon, negotiateColoredIcon, showColoredIcon, giveIcon,
      dropIcon, editIcon, editColoredIcon, showIcon, goRestIcon, cleanOrderIcon,
      repairOrderIcon, giveOrderColoredIcon, inventoryIcon, dismissIcon,
      giveOrderIcon, cargoIcon, moveIcon, repairPriorityIcon, upgradeButtonIcon,
      powerIcon, assignAmmoIcon, giveColoredIcon, dropColoredIcon, craftIcon,
      studyIcon, deconstructIcon, craftColoredIcon, studyColoredIcon,
      deconstructColoredIcon, destinationIcon
  MapIconsNames* = enum
    ## Names of icons used in the game's map
    playerShipIcon, emptyMapIcon, targetIcon, storyIcon, deliverIcon,
      destroyIcon, patrolIcon, exploreIcon, passengerIcon, enemyShipIcon,
      attackOnBaseIcon, enemyPatrolIcon, diseaseIcon, fullDocksIcon,
      doublePriceIcon, mapTraderIcon, friendlyShipIcon, notVisitedBaseIcon
  ThemeData* = object
    ## Stores data about the game's theme
    name: string
    fileName: string
    icons*: array[IconsNames, string]
    colors*: array[ColorsNames, Color]
    fonts*: array[FontsNames, string]
    mapIcons*: array[MapIconsNames, string]
    mapColors*: array[MapColorsNames, Color]

let
  defaultThemePath: string = dataDirectory & "ui" & DirSep
  defaultThemeIconPath: string = defaultThemePath & "images" & DirSep & "ui" & DirSep
  defaultThemeFontPath: string = defaultThemePath & "fonts" & DirSep
  defaultTheme*: ThemeData = ThemeData(name: "Default theme",
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
      defaultThemeIconPath & "arrow-down-right.svg", defaultThemeIconPath &
      "wait.svg", defaultThemeIconPath & "moveto.svg", defaultThemeIconPath &
      "movestep.svg", defaultThemeIconPath & "expand.svg",
      defaultThemeIconPath & "contract2.svg", defaultThemeIconPath &
      "selectall.svg", defaultThemeIconPath & "unselectall.svg",
      defaultThemeIconPath & "cancel.svg", defaultThemeIconPath &
      "giveorder.svg", defaultThemeIconPath & "exit.svg",
      defaultThemeIconPath & "moreoptions.svg", defaultThemeIconPath &
      "vertical-flip.svg", defaultThemeIconPath & "contract.svg",
      defaultThemeIconPath & "buy2.svg", defaultThemeIconPath & "sell2.svg",
      defaultThemeIconPath & "buy.svg", defaultThemeIconPath & "sell.svg",
      defaultThemeIconPath & "negotiate.svg", defaultThemeIconPath &
      "help.svg", defaultThemeIconPath & "negotiate2.svg",
      defaultThemeIconPath & "show2.svg", defaultThemeIconPath & "give.svg",
      defaultThemeIconPath & "drop.svg", defaultThemeIconPath & "edit.svg",
      defaultThemeIconPath & "edit2.svg", defaultThemeIconPath & "show.svg",
      defaultThemeIconPath & "gorest.svg", defaultThemeIconPath &
      "clean-order.svg", defaultThemeIconPath & "repair-order.svg",
      defaultThemeIconPath & "giveorder2.svg", defaultThemeIconPath &
      "inventory.svg", defaultThemeIconPath & "dismiss.svg",
      defaultThemeIconPath & "giveorder.svg", defaultThemeIconPath &
      "cargo.svg", defaultThemeIconPath & "cargo2.svg",
      defaultThemeIconPath & "repair-priority.svg", defaultThemeIconPath &
      "upgrade-button.svg", defaultThemeIconPath & "power.svg",
      defaultThemeIconPath & "assignammo.svg", defaultThemeIconPath &
      "give2.svg", defaultThemeIconPath & "drop2.svg", defaultThemeIconPath &
      "craft-order.svg", defaultThemeIconPath & "study.svg",
      defaultThemeIconPath & "deconstruct.svg", defaultThemeIconPath &
      "craft-order2.svg", defaultThemeIconPath & "study2.svg",
      defaultThemeIconPath & "deconstruct2.svg", defaultThemeIconPath &
      "destination.svg"],
      colors: ["#1a130c".parseColor, "#eee8aa".parseColor,
      "#4e9a06".parseColor, "#372412".parseColor, "#291913".parseColor,
      "#500000".parseColor, "#120d0d".parseColor, "#ffdf00".parseColor,
      "#120d0d".parseColor, "#372412".parseColor, "#1a130c".parseColor,
      "#120d0d".parseColor, "#120d0d".parseColor, "#ffdf00".parseColor,
      "#372412".parseColor, "#ffdf00".parseColor, "#ffdf00".parseColor,
      "#7f8c8d".parseColor, "#000000".parseColor, "#006400".parseColor,
      "#458588".parseColor, "#4e9a06".parseColor, "#ffdf00".parseColor,
      "#ffdf00".parseColor, "#372412".parseColor, "#500000".parseColor,
      "#4e9a06".parseColor, "#ffdf00".parseColor, "#fb4934".parseColor,
      "#7f8c8d".parseColor, "#000000".parseColor, "#b16286".parseColor,
      "#d79921".parseColor, "#458588".parseColor, "#00ffff".parseColor,
      "#7f8c8d".parseColor, "#120d0d".parseColor, "#eee8aa".parseColor,
      "#120d0d".parseColor, "#ffdf00".parseColor, "#500000".parseColor,
      "#4e9a06".parseColor],
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
    ## The default game's theme

var themesList*: Table[string, ThemeData] = initTable[string, ThemeData]() ## The list of all available themes

proc loadThemes*() {.raises: [], tags: [WriteIOEffect, TimeEffect, RootEffect,
    ReadDirEffect, ReadIOEffect, RootEffect], contractual.} =
  ## Load all the game's themes and set the configured theme for the game
  var localTheme: ThemeData = defaultTheme
  themesList["steamsky"] = localTheme
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
                localTheme.name = entry.value
              of "FileName":
                localTheme.fileName = themeDir & DirSep & entry.value
              else:
                var validName: bool = true
                # Check if the option is a color
                try:
                  let index: ColorsNames = parseEnum[ColorsNames](
                      s = entry.value)
                  localTheme.colors[index] = entry.value.parseColor
                except:
                  validName = false
                # Check if the option is a map's color
                if not validName:
                  try:
                    let index: MapColorsNames = parseEnum[MapColorsNames](
                        s = entry.value)
                    localTheme.mapColors[index] = entry.value.parseColor
                    validName = true
                  except:
                    discard
                # Check if the option is a font's name
                if not validName:
                  try:
                    let index: FontsNames = parseEnum[FontsNames](
                        s = entry.value)
                    localTheme.fonts[index] = themeDir & DirSep &
                      entry.value.unixToNativePath
                    validName = true
                  except:
                    discard
                # Check if the option is an icon's name
                if not validName:
                  try:
                    let index: IconsNames = parseEnum[IconsNames](
                        s = entry.value)
                    localTheme.icons[index] = themeDir & DirSep &
                        entry.value.unixToNativePath
                    validName = true
                  except:
                    discard
                # Check if the option is a map's icon's name
                if not validName:
                  try:
                    let index: MapIconsNames = parseEnum[MapIconsNames](
                        s = entry.value)
                    localTheme.mapIcons[index] = entry.value
                    validName = true
                  except:
                    discard
                if not validName:
                  echo "Invalid name of configuration option '" & entry.key &
                      "' in file: " & configName
                  return
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
      themesList[themeDir.lastPathPart] = localTheme
      localTheme = defaultTheme
  except:
    discard
  if gameSettings.interfaceTheme notin themesList:
    gameSettings.interfaceTheme = "steamsky"
  {.ruleOff: "varDeclared".}
  var table: array[countColors, NkColor]
  {.ruleOn: "varDeclared".}

  proc setColor(colorName: StyleColors; index: ColorsNames) {.raises: [],
      tags: [], contractual.} =
    ## Convert the selected color to Nuklear color
    ##
    ## * colorName - the Nuklear's theme's color name to which will be converted
    ## * index     - the index of the color from the game's current theme
    let (r, g, b) = try:
        extractRGB(a = themesList[gameSettings.interfaceTheme].colors[index])
      except:
        echo "Can't set the theme's color."
        return
    table[colorName] = NkColor(r: r, g: g, b: b, a: 255)

  setColor(colorName = windowColor, index = backgroundColor)
  setColor(colorName = textColor, index = foregroundColor)
  setColor(colorName = borderColor, index = borderColor)
  setColor(colorName = buttonColor, index = buttonColor)
  setColor(colorName = buttonHoverColor, index = buttonHoverColor)
  setColor(colorName = editColor, index = editColor)
  setColor(colorName = editCursorColor, index = editCursorColor)
  setColor(colorName = buttonActiveColor, index = buttonActiveColor)
  setColor(colorName = headerColor, index = headerColor)
  setColor(colorName = comboColor, index = comboColor)
  setColor(colorName = propertyColor, index = propertyColor)
  setColor(colorName = scrollbarColor, index = scrollbarColor)
  setColor(colorName = buttonTextColor, index = buttonTextColor)
  setColor(colorName = buttonHoverTextColor, index = foregroundColor)
  setColor(colorName = buttonActiveTextColor, index = foregroundColor)
  setColor(colorName = scrollbarCursorColor, index = scrollbarCursorColor)
  setColor(colorName = scrollbarCursorHoverColor, index = scrollbarCursorColor)
  setColor(colorName = scrollbarCursorActiveColor, index = scrollbarCursorColor)
  setColor(colorName = editTextColor, index = editTextColor)
  setColor(colorName = comboTextColor, index = comboTextColor)
  setColor(colorName = tooltipBorderColor, index = tooltipBorderColor)
  setColor(colorName = tooltipColor, index = tooltipColor)
  setColor(colorName = groupBorderColor, index = groupBorderColor)
  setColor(colorName = headerTextColor, index = headerTextColor)
  setColor(colorName = groupTextColor, index = groupTextColor)
  setColor(colorName = selectColor, index = backgroundColor)
  setColor(colorName = selectActiveColor, index = buttonHoverColor)
  setColor(colorName = selectActiveTextColor, index = selectActiveTextColor)
  setColor(colorName = propertyTextColor, index = propertyTextColor)
  setColor(colorName = toggleColor, index = toggleColor)
  setColor(colorName = toggleHoverColor, index = toggleHoverColor)
  setColor(colorName = toggleCursorColor, index = toggleCursorColor)
  setColor(colorName = popupBorderColor, index = mapInfoBorderColor)
  setColor(colorName = popupColor, index = mapInfoColor)
  setColor(colorName = sliderColor, index = sliderEmptyColor)
  setColor(colorName = sliderCursorColor, index = sliderFillColor)
  setColor(colorName = sliderCursorHoverColor, index = sliderCursorColor)
  setColor(colorName = sliderCursorActiveColor, index = sliderCursorColor)
  setColor(colorName = progressbarColor, index = progressbarColor)
  table[chartColor] = NkColor(r: 50, g: 58, b: 61, a: 255)
  table[colorChartColor] = NkColor(r: 48, g: 83, b: 111, a: 255)
  table[colorChartHighlightColor] = NkColor(r: 255, g: 0, b: 0, a: 255)
  table[tabHeaderColor] = NkColor(r: 48, g: 83, b: 111, a: 255)
  table[progressbarBorderColor] = NkColor(r: 55, g: 36, b: 18, a: 255)
  styleFromTable(table = table)
