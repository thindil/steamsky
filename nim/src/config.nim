# Copyright 2022-2024 Bartek thindil Jasicki
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

## Provides code related to the game's configuration, like load and save it to
## the file.

import std/[parsecfg, streams, strutils]
import contracts
import game, types

type
  AutoMoveBreak* = enum
    ## When to stop auto movement of the player's ship: never, on encounter any
    ## ship, friendly ship, enemy ship
    never, any, friendly, enemy

  MessagesOrder* = enum
    ## In what order show the last messages: older messages first, newer messages
    ## first
    olderFirst = "older_First", newerFirst = "newer_First"

  AutoSaveTime* = enum
    ## When save the game automatically: never, after dock to a base, after
    ## undock from a base, every game day, every game month, every game year
    none, dock, undock, daily, monthly, yearly

  GameSettingsRecord* = object
    ## Used to store the game's configuration
    ##
    ## * autoRest              - If true, auto rest when pilot or engineer need a rest
    ## * undockSpeed           - The default speed of the player's ship after undock from a base
    ## * autoCenter            - If true, back to the player's ship after setting destination for it
    ## * autoReturn            - If true, set the destination for the player's ship to the base after
    ##                           finishing a mission
    ## * autoFinish            - If true, automatically finish the mission if the player's ships is in
    ##                           the proper base
    ## * lowFuel               - The amount of fuel at which the game will show the warning
    ##                           about it
    ## * lowDrinks             - The amount of drinks at which the game will show the warning
    ##                           about it
    ## * lowFood               - The amount of food at which the game will show the warning
    ##                           about it
    ## * autoMoveStop          - When stop the player's ship's auto movement
    ## * windowWidth           - The game window default width
    ## * windowHeight          - The game window default height
    ## * messagesLimit         - The max amount of messages to show in the game
    ## * savedMessages         - The max amount of messages to save to a file
    ## * helpFontSize          - The size of a font used in help
    ## * mapFontSize           - The size of a font used on the map
    ## * interfaceFontSize     - The size of a font used in the game interface
    ## * interfaceTheme        - The name of the current theme of the game interface
    ## * messagesOrder         - In what order the messages should be shown
    ## * autoAskForBases       - If true, auto ask for new bases when the player's ship is
    ##                           docked to a base
    ## * autoAskForEvents      - If true, auto ask for new events when the player's ship is
    ##                           docked to a base
    ## * showTooltips          - Show the in-game tooltips with help information
    ## * showLastMessages      - Show the last messages window below the map
    ## * messagesPosition      - The height of the last messages window
    ## * fullScreen            - Run the game in full screen mode
    ## * autoCloseMessagesTime - The amount of seconds after which messages' dialogs
    ##                           will be closed
    ## * autoSave              - How often the game should save itself automatically
    ## * topicsPosition        - The height of the topics' window position in help window
    ## * showNumbers           - If true, show numbers for speed, skills, attributes, etc.
    ## * rightButton           - If true, use the right mouse button for show menus in various lists
    ## * listsLimit            - The amount of items displayed in various lists
    ## * waitMinutes           - The amount of in-game minutes which pass when the player press Wait button
    autoRest*: bool
    undockSpeed*: ShipSpeed
    autoCenter*: bool
    autoReturn*: bool
    autoFinish*: bool
    lowFuel*: Natural
    lowDrinks*: Natural
    lowFood*: Natural
    autoMoveStop*: AutoMoveBreak
    windowWidth*: Positive
    windowHeight*: Positive
    messagesLimit*: Natural
    savedMessages*: Natural
    helpFontSize*: Positive
    mapFontSize*: Positive
    interfaceFontSize*: Positive
    interfaceTheme*: string
    messagesOrder*: MessagesOrder
    autoAskForBases*: bool
    autoAskForEvents*: bool
    showTooltips*: bool
    showLastMessages*: bool
    messagesPosition*: Natural
    fullScreen*: bool
    autoCloseMessagesTime*: Natural
    autoSave*: AutoSaveTime
    topicsPosition*: Natural
    showNumbers*: bool
    rightButton*: bool
    listsLimit*: Positive
    waitMinutes*: Positive

  BonusType* = range[0.0..5.0]
    ## Points' multiplier from various game's settings

  DifficultyType* = enum
    ## The level of the game's difficulty. All setttings except custom are preset
    ## levels
    veryEasy, easy, normal, hard, veryHard, custom

  NewGameRecord* = object
    ## Used to store the default settings for the new game
    ##
    ## * playerName             - The player's character name
    ## * playerGender           - The player's character gender
    ## * shipName               - The player's ship name
    ## * playerFaction          - The player's character faction
    ## * playerCareer           - The player's character career
    ## * startingBase           - The type of the starting base
    ## * enemyDamageBonus       - The bonus to damage for enemies in ship to ship combat
    ## * playerDamageBonus      - The bonus to damage for the player's character and crew in
    ##                            ship to ship combat
    ## * enemyMeleeDamageBonus  - The bonus to damage for enemies in melee combat
    ## * playerMeleeDamageBonus - The bonus to damage for the player's character and crew
    ##                            in melee combat
    ## * experienceBonus        - The bonus to the gained by player's character and crew experience
    ## * reputationBonus        - The bonus to the gained the player's character reputation in bases
    ## * upgradeCostBonus       - The bonus to costs of upgrades the player's ship
    ## * pricesBonus            - The bonus to prices in bases
    ## * difficultyLevel        - The preset level of difficulty for the game
    playerName*: string
    playerGender*: char
    shipName*: string
    playerFaction*: string
    playerCareer*: string
    startingBase*: string
    enemyDamageBonus*: BonusType
    playerDamageBonus*: BonusType
    enemyMeleeDamageBonus*: BonusType
    playerMeleeDamageBonus*: BonusType
    experienceBonus*: BonusType
    reputationBonus*: BonusType
    upgradeCostBonus*: BonusType
    pricesBonus*: BonusType
    difficultyLevel*: DifficultyType

  FontTypes* = enum
    ## The types of in-game fonts, used to set their size
    helpFont, interfaceFont, mapFont

const
  defaultGameSettings*: GameSettingsRecord = GameSettingsRecord(autoRest: true,
    undockSpeed: fullSpeed, autoCenter: true, autoReturn: true,
    autoFinish: true, lowFuel: 100, lowDrinks: 50, lowFood: 25,
    autoMoveStop: never, windowWidth: 800, windowHeight: 600,
    messagesLimit: 500, savedMessages: 10, helpFontSize: 14, mapFontSize: 16,
    interfaceFontSize: 14, interfaceTheme: "steamsky",
    messagesOrder: olderFirst, autoAskForBases: false,
    autoAskForEvents: false,
    showTooltips: true, showLastMessages: true, messagesPosition: 256,
    fullScreen: false, autoCloseMessagesTime: 6, autoSave: none,
    topicsPosition: 200, showNumbers: false, rightButton: false, listsLimit: 25,
    waitMinutes: 1)
    ## The default setting for the game

  defaultNewGameSettings*: NewGameRecord = NewGameRecord(playerName: "Laeran",
    playerGender: 'M', shipName: "Anaria", playerFaction: "POLEIS",
    playerCareer: "general", startingBase: "Any", enemyDamageBonus: 1.0,
    playerDamageBonus: 1.0, enemyMeleeDamageBonus: 1.0,
    playerMeleeDamageBonus: 1.0, experienceBonus: 1.0, reputationBonus: 1.0,
    upgradeCostBonus: 1.0, pricesBonus: 1.0, difficultyLevel: normal)
    ## The default setting for the new game

var
  newGameSettings*: NewGameRecord = defaultNewGameSettings ## The settings for new game
  gameSettings*: GameSettingsRecord = defaultGameSettings ## The general settings for the game

proc loadConfig*() {.sideEffect, raises: [], tags: [RootEffect], contractual.} =
  ## Load the game and new game settings from the file
  let fileName: string = saveDirectory & "game.cfg"
  var configFile: FileStream = newFileStream(filename = fileName, mode = fmRead)
  if configFile == nil:
    return
  var parser: CfgParser = CfgParser()
  try:
    parser.open(input = configFile, filename = fileName)
  except OSError, IOError, Exception:
    echo "Can't initialize configuration file parser. Reason: " &
        getCurrentExceptionMsg()
    return

  proc parseAdaFloat(value: string): cfloat {.sideEffect, raises: [ValueError],
      tags: [], contractual.} =
    ## Temporary function, for backward compatibility with Ada code
    require:
      value.len > 0
    body:
      var newValue: string = value
      newValue.removeSuffix(c = 'E')
      return newValue.parseFloat().cfloat

  proc parseAdaBool(value: string): bool {.sideEffect, raises: [], tags: [],
      contractual.} =
    ## Temporary function, for backward compatibility with Ada code
    require:
      value.len > 0
    body:
      return value == "Yes"

  while true:
    try:
      let entry: CfgEvent = parser.next()
      case entry.kind
      of cfgEof:
        break
      of cfgKeyValuePair, cfgOption:
        case entry.key
        of "PlayerName":
          newGameSettings.playerName = entry.value
        of "PlayerGender":
          newGameSettings.playerGender = entry.value[0]
        of "ShipName":
          newGameSettings.shipName = entry.value
        of "PlayerFaction":
          newGameSettings.playerFaction = entry.value
        of "PlayerCareer":
          newGameSettings.playerCareer = entry.value
        of "StartingBase":
          newGameSettings.startingBase = entry.value
        of "EnemyDamageBonus":
          newGameSettings.enemyDamageBonus = entry.value.parseAdaFloat()
        of "PlayerDamageBonus":
          newGameSettings.playerDamageBonus = entry.value.parseAdaFloat()
        of "EnemyMeleeDamageBonus":
          newGameSettings.enemyMeleeDamageBonus = entry.value.parseAdaFloat()
        of "PlayerMeleeDamageBonus":
          newGameSettings.playerMeleeDamageBonus = entry.value.parseAdaFloat()
        of "ExperienceBonus":
          newGameSettings.experienceBonus = entry.value.parseAdaFloat()
        of "ReputationBonus":
          newGameSettings.reputationBonus = entry.value.parseAdaFloat()
        of "UpgradeCostBonus":
          newGameSettings.upgradeCostBonus = entry.value.parseAdaFloat()
        of "PricesBonus":
          newGameSettings.pricesBonus = entry.value.parseAdaFloat()
        of "DifficultyLevel":
          newGameSettings.difficultyLevel = parseEnum[DifficultyType](s =
            entry.value.toLowerAscii)
        of "AutoRest":
          gameSettings.autoRest = entry.value.parseAdaBool()
        of "UndockSpeed":
          gameSettings.undockSpeed = parseEnum[ShipSpeed](s =
            entry.value.toLowerAscii)
        of "AutoCenter":
          gameSettings.autoCenter = entry.value.parseAdaBool()
        of "AutoReturn":
          gameSettings.autoReturn = entry.value.parseAdaBool()
        of "AutoFinish":
          gameSettings.autoFinish = entry.value.parseAdaBool()
        of "LowFuel":
          gameSettings.lowFuel = entry.value.parseInt().cint
        of "LowDrinks":
          gameSettings.lowDrinks = entry.value.parseInt().cint
        of "LowFood":
          gameSettings.lowFood = entry.value.parseInt().cint
        of "AutoMoveStop":
          gameSettings.autoMoveStop = parseEnum[AutoMoveBreak](s =
            entry.value.toLowerAscii)
        of "WindowWidth":
          gameSettings.windowWidth = entry.value.parseInt().cint
        of "WindowHeight":
          gameSettings.windowHeight = entry.value.parseInt().cint
        of "MessagesLimit":
          gameSettings.messagesLimit = entry.value.parseInt().cint
        of "SavedMessages":
          gameSettings.savedMessages = entry.value.parseInt().cint
        of "HelpFontSize":
          gameSettings.helpFontSize = entry.value.parseInt().cint
        of "MapFontSize":
          gameSettings.mapFontSize = entry.value.parseInt().cint
        of "InterfaceFontSize":
          gameSettings.interfaceFontSize = entry.value.parseInt().cint
        of "InterfaceTheme":
          gameSettings.interfaceTheme = entry.value
        of "MessagesOrder":
          gameSettings.messagesOrder = parseEnum[MessagesOrder](s =
            entry.value.toLowerAscii)
        of "AutoAskForBases":
          gameSettings.autoAskForBases = entry.value.parseAdaBool()
        of "AutoAskForEvents":
          gameSettings.autoAskForEvents = entry.value.parseAdaBool()
        of "ShowTooltips":
          gameSettings.showTooltips = entry.value.parseAdaBool()
        of "ShowLastMessages":
          gameSettings.showLastMessages = entry.value.parseAdaBool()
        of "MessagesPosition":
          gameSettings.messagesPosition = entry.value.parseInt().cint
        of "FullScreen":
          gameSettings.fullScreen = entry.value.parseAdaBool()
        of "AutoCloseMessagesTime":
          gameSettings.autoCloseMessagesTime = entry.value.parseInt().cint
        of "AutoSave":
          gameSettings.autoSave = parseEnum[AutoSaveTime](s =
            entry.value.toLowerAscii)
        of "TopicsPosition":
          gameSettings.topicsPosition = entry.value.parseInt().cint
        of "ShowNumbers":
          gameSettings.showNumbers = entry.value.parseAdaBool()
        of "RightButton":
          gameSettings.rightButton = entry.value.parseAdaBool()
        of "ListsLimit":
          gameSettings.listsLimit = entry.value.parseInt().cint
        of "WaitMinutes":
          gameSettings.waitMinutes = entry.value.parseInt().cint
        else:
          discard
      of cfgError:
        echo entry.msg
      of cfgSectionStart:
        discard
    except ValueError, OSError, IOError:
      echo "Invalid data in the game configuration file. Details: " &
          getCurrentExceptionMsg()
      continue
  try:
    parser.close()
  except OSError, IOError, Exception:
    echo "Can't close configuration file parser. Reason: " &
        getCurrentExceptionMsg()

proc saveConfig*() {.sideEffect, raises: [KeyError, IOError, OSError], tags: [
    WriteIOEffect], contractual.} =
  ## Save the new game and the game itself configuration to the file
  var config: Config = newConfig()

  proc saveAdaBoolean(value: bool, name: string) {.sideEffect, raises: [
      KeyError], tags: [], contractual.} =
    ## Temporary function, for backward compatibility with Ada code
    require:
      name.len > 0
    body:
      if value:
        config.setSectionKey(section = "", key = name, value = "Yes")
      else:
        config.setSectionKey(section = "", key = name, value = "No")

  config.setSectionKey(section = "", key = "PlayerName",
      value = $newGameSettings.playerName)
  config.setSectionKey(section = "", key = "PlayerGender",
      value = $newGameSettings.playerGender)
  config.setSectionKey(section = "", key = "ShipName",
      value = $newGameSettings.shipName)
  config.setSectionKey(section = "", key = "PlayerFaction",
      value = $newGameSettings.playerFaction)
  config.setSectionKey(section = "", key = "PlayerCareer",
      value = $newGameSettings.playerCareer)
  config.setSectionKey(section = "", key = "StartingBase",
      value = $newGameSettings.startingBase)
  config.setSectionKey(section = "", key = "EnemyDamageBonus",
      value = $newGameSettings.enemyDamageBonus)
  config.setSectionKey(section = "", key = "PlayerDamageBonus",
      value = $newGameSettings.playerDamageBonus)
  config.setSectionKey(section = "", key = "EnemyMeleeDamageBonus",
      value = $newGameSettings.enemyMeleeDamageBonus)
  config.setSectionKey(section = "", key = "PlayerMeleeDamageBonus",
      value = $newGameSettings.playerMeleeDamageBonus)
  config.setSectionKey(section = "", key = "ExperienceBonus",
      value = $newGameSettings.experienceBonus)
  config.setSectionKey(section = "", key = "ReputationBonus",
      value = $newGameSettings.reputationBonus)
  config.setSectionKey(section = "", key = "UpgradeCostBonus",
      value = $newGameSettings.upgradeCostBonus)
  config.setSectionKey(section = "", key = "PricesBonus",
      value = $newGameSettings.pricesBonus)
  config.setSectionKey(section = "", key = "DifficultyLevel", value = (
      $newGameSettings.difficultyLevel).toUpperAscii)
  saveAdaBoolean(value = gameSettings.autoRest, name = "AutoRest")
  config.setSectionKey(section = "", key = "UndockSpeed", value = (
      $gameSettings.undockSpeed).toUpperAscii)
  saveAdaBoolean(value = gameSettings.autoCenter, name = "AutoCenter")
  saveAdaBoolean(value = gameSettings.autoReturn, name = "AutoReturn")
  saveAdaBoolean(value = gameSettings.autoFinish, name = "AutoFinish")
  config.setSectionKey(section = "", key = "LowFuel",
      value = $gameSettings.lowFuel)
  config.setSectionKey(section = "", key = "LowDrinks",
      value = $gameSettings.lowDrinks)
  config.setSectionKey(section = "", key = "LowFood",
      value = $gameSettings.lowFood)
  config.setSectionKey(section = "", key = "AutoMoveStop", value = (
      $gameSettings.autoMoveStop).toUpperAscii)
  config.setSectionKey(section = "", key = "WindowWidth",
      value = $gameSettings.windowWidth)
  config.setSectionKey(section = "", key = "WindowHeight",
      value = $gameSettings.windowHeight)
  config.setSectionKey(section = "", key = "MessagesLimit",
      value = $gameSettings.messagesLimit)
  config.setSectionKey(section = "", key = "SavedMessages",
      value = $gameSettings.savedMessages)
  config.setSectionKey(section = "", key = "HelpFontSize",
      value = $gameSettings.helpFontSize)
  config.setSectionKey(section = "", key = "MapFontSize",
      value = $gameSettings.mapFontSize)
  config.setSectionKey(section = "", key = "InterfaceFontSize",
      value = $gameSettings.interfaceFontSize)
  config.setSectionKey(section = "", key = "InterfaceTheme",
      value = $gameSettings.interfaceTheme)
  config.setSectionKey(section = "", key = "MessagesOrder", value = (
      $gameSettings.messagesOrder).toUpperAscii)
  saveAdaBoolean(value = gameSettings.autoAskForBases, name = "AutoAskForBases")
  saveAdaBoolean(value = gameSettings.autoAskForEvents,
      name = "AutoAskForEvents")
  saveAdaBoolean(value = gameSettings.showTooltips, name = "ShowTooltips")
  saveAdaBoolean(value = gameSettings.showLastMessages,
      name = "ShowLastMessages")
  config.setSectionKey(section = "", key = "MessagesPosition",
      value = $gameSettings.messagesPosition)
  saveAdaBoolean(value = gameSettings.fullScreen, name = "FullScreen")
  config.setSectionKey(section = "", key = "AutoCloseMessagesTime", value =
    $gameSettings.autoCloseMessagesTime)
  config.setSectionKey(section = "", key = "AutoSave", value = (
      $gameSettings.autoSave).toUpperAscii)
  config.setSectionKey(section = "", key = "TopicsPosition",
      value = $gameSettings.topicsPosition)
  saveAdaBoolean(value = gameSettings.showNumbers, name = "ShowNumbers")
  saveAdaBoolean(value = gameSettings.rightButton, name = "RightButton")
  config.setSectionKey(section = "", key = "ListsLimit",
      value = $gameSettings.listsLimit)
  config.setSectionKey(section = "", key = "WaitMinutes",
      value = $gameSettings.waitMinutes)
  config.writeConfig(filename = saveDirectory & "game.cfg")

# Temporary code for interfacing with Ada

proc loadAdaConfig() {.sideEffect, raises: [], tags: [RootEffect], exportc,
    contractual.} =
  ## Temporary C binding
  loadConfig()

proc setAdaMessagesPosition(newValue: cint) {.sideEffect, raises: [], tags: [],
    exportc, contractual.} =
  ## Temporary C binding
  gameSettings.messagesPosition = newValue

proc saveAdaConfig() {.sideEffect, raises: [], tags: [RootEffect], exportc,
    contractual.} =
  ## Temporary C binding
  try:
    saveConfig()
  except KeyError, IOError, OSError, ValueError:
    discard

proc getAdaBooleanSetting(name: cstring): cint {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  case $name
  of "autoRest":
    result = gameSettings.autoRest.cint
  of "autoCenter":
    result = gameSettings.autoCenter.cint
  of "autoReturn":
    result = gameSettings.autoReturn.cint
  of "autoFinish":
    result = gameSettings.autoFinish.cint
  of "autoAskForBases":
    result = gameSettings.autoAskForBases.cint
  of "autoAskForEvents":
    result = gameSettings.autoAskForEvents.cint
  of "showTooltips":
    result = gameSettings.showTooltips.cint
  of "showLastMessages":
    result = gameSettings.showLastMessages.cint
  of "fullScreen":
    result = gameSettings.fullScreen.cint
  of "showNumbers":
    result = gameSettings.showNumbers.cint
  of "rightButton":
    result = gameSettings.rightButton.cint
  else:
    result = 0

proc setAdaBooleanSetting(name: cstring; value: cint) {.raises: [], tags: [],
    exportc, contractual.} =
  ## Temporary C binding
  case $name
  of "autoRest":
    gameSettings.autoRest = value == 1
  of "autoCenter":
    gameSettings.autoCenter = value == 1
  of "autoReturn":
    gameSettings.autoReturn = value == 1
  of "autoFinish":
    gameSettings.autoFinish = value == 1
  of "autoAskForBases":
    gameSettings.autoAskForBases = value == 1
  of "autoAskForEvents":
    gameSettings.autoAskForEvents = value == 1
  of "showTooltips":
    gameSettings.showTooltips = value == 1
  of "showLastMessages":
    gameSettings.showLastMessages = value == 1
  of "fullScreen":
    gameSettings.fullScreen = value == 1
  of "showNumbers":
    gameSettings.showNumbers = value == 1
  of "rightButton":
    gameSettings.rightButton = value == 1
  else:
    discard

proc getAdaIntegerSetting(name: cstring): cint {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  case $name
  of "lowFuel":
    result = gameSettings.lowFuel.cint
  of "lowDrinks":
    result = gameSettings.lowDrinks.cint
  of "lowFood":
    result = gameSettings.lowFood.cint
  of "windowWidth":
    result = gameSettings.windowWidth.cint
  of "windowHeight":
    result = gameSettings.windowHeight.cint
  of "messagesLimit":
    result = gameSettings.messagesLimit.cint
  of "savedMessages":
    result = gameSettings.savedMessages.cint
  of "helpFontSize":
    result = gameSettings.helpFontSize.cint
  of "mapFontSize":
    result = gameSettings.mapFontSize.cint
  of "interfaceFontSize":
    result = gameSettings.interfaceFontSize.cint
  of "messagesPosition":
    result = gameSettings.messagesPosition.cint
  of "autoCloseMessagesTime":
    result = gameSettings.autoCloseMessagesTime.cint
  of "topicsPosition":
    result = gameSettings.topicsPosition.cint
  of "listsLimit":
    result = gameSettings.listsLimit.cint
  of "undockSpeed":
    result = gameSettings.undockSpeed.ord.cint
  of "autoMoveStop":
    result = gameSettings.autoMoveStop.ord.cint
  of "messagesOrder":
    result = gameSettings.messagesOrder.ord.cint
  of "autoSave":
    result = gameSettings.autoSave.ord.cint
  of "difficulty":
    result = newGameSettings.difficultyLevel.ord.cint
  of "waitMinutes":
    result = gameSettings.waitMinutes.cint
  else:
    result = 0

proc setAdaIntegerSetting(name: cstring; value: cint) {.raises: [], tags: [],
    exportc, contractual.} =
  ## Temporary C binding
  case $name
  of "lowFuel":
    gameSettings.lowFuel = value
  of "lowDrinks":
    gameSettings.lowDrinks = value
  of "lowFood":
    gameSettings.lowFood = value
  of "windowWidth":
    gameSettings.windowWidth = value
  of "windowHeight":
    gameSettings.windowHeight = value
  of "messagesLimit":
    gameSettings.messagesLimit = value
  of "savedMessages":
    gameSettings.savedMessages = value
  of "helpFontSize":
    gameSettings.helpFontSize = value
  of "mapFontSize":
    gameSettings.mapFontSize = value
  of "interfaceFontSize":
    gameSettings.interfaceFontSize = value
  of "messagesPosition":
    gameSettings.messagesPosition = value
  of "autoCloseMessagesTime":
    gameSettings.autoCloseMessagesTime = value
  of "topicsPosition":
    gameSettings.topicsPosition = value
  of "listsLimit":
    gameSettings.listsLimit = value
  of "undockSpeed":
    gameSettings.undockSpeed = value.ShipSpeed
  of "autoMoveStop":
    gameSettings.autoMoveStop = value.AutoMoveBreak
  of "messagesOrder":
    gameSettings.messagesOrder = value.MessagesOrder
  of "autoSave":
    gameSettings.autoSave = value.AutoSaveTime
  of "difficulty":
    newGameSettings.difficultyLevel = value.DifficultyType
  of "waitMinutes":
    gameSettings.waitMinutes = value
  else:
    discard

proc getAdaInterfaceTheme(): cstring {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  return gameSettings.interfaceTheme.cstring

proc setAdaInterfaceTheme(value: cstring) {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  gameSettings.interfaceTheme = $value

proc getAdaStringSetting(name: cstring): cstring {.raises: [], tags: [],
    exportc, contractual.} =
  ## Temporary C binding
  case $name
  of "playerName":
    return newGameSettings.playerName.cstring
  of "shipName":
    return newGameSettings.shipName.cstring
  of "playerFaction":
    return newGameSettings.playerFaction.cstring
  of "playerCareer":
    return newGameSettings.playerCareer.cstring
  of "startingBase":
    return newGameSettings.startingBase.cstring

proc setAdaStringSetting(name, value: cstring) {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  case $name
  of "playerName":
    newGameSettings.playerName = $value
  of "shipName":
    newGameSettings.shipName = $value
  of "playerFaction":
    newGameSettings.playerFaction = $value
  of "playerCareer":
    newGameSettings.playerCareer = $value
  of "startingBase":
    newGameSettings.startingBase = $value

proc getAdaFloatSetting(name: cstring): cfloat {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  case $name
  of "enemyDamageBonus":
    return newGameSettings.enemyDamageBonus.cfloat
  of "playerDamageBonus":
    return newGameSettings.playerDamageBonus.cfloat
  of "enemyMeleeDamageBonus":
    return newGameSettings.enemyMeleeDamageBonus.cfloat
  of "playerMeleeDamageBonus":
    return newGameSettings.playerMeleeDamageBonus.cfloat
  of "experienceBonus":
    return newGameSettings.experienceBonus.cfloat
  of "reputationBonus":
    return newGameSettings.reputationBonus.cfloat
  of "upgradeCostBonus":
    return newGameSettings.upgradeCostBonus.cfloat
  of "pricesBonus":
    return newGameSettings.pricesBonus.cfloat

proc setAdaFloatSetting(name: cstring; value: cfloat) {.raises: [], tags: [],
    exportc, contractual.} =
  ## Temporary C binding
  case $name
  of "enemyDamageBonus":
    newGameSettings.enemyDamageBonus = value
  of "playerDamageBonus":
    newGameSettings.playerDamageBonus = value
  of "enemyMeleeDamageBonus":
    newGameSettings.enemyMeleeDamageBonus = value
  of "playerMeleeDamageBonus":
    newGameSettings.playerMeleeDamageBonus = value
  of "experienceBonus":
    newGameSettings.experienceBonus = value
  of "reputationBonus":
    newGameSettings.reputationBonus = value
  of "upgradeCostBonus":
    newGameSettings.upgradeCostBonus = value
  of "pricesBonus":
    newGameSettings.pricesBonus = value

proc getAdaGender(): char {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  return newGameSettings.playerGender

proc setAdaGender(value: char) {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  newGameSettings.playerGender = value
