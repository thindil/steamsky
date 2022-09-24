# Copyright 2022 Bartek thindil Jasicki
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

import std/[parsecfg, streams, strutils]
import game, ships

type
  AutoMoveBreak* = enum
    ## FUNCTION
    ##
    ## When to stop auto movement of the player's ship: never, on encounter any
    ## ship, friendly ship, enemy ship
    never, any, friendly, enemy

  MessagesOrder* = enum
    ## FUNCTION
    ##
    ## In what order show the last messages: older messages first, newer messages
    ## first
    olderFirst, newerFirst

  AutoSaveTime* = enum
    ## FUNCTION
    ##
    ## When save the game automatically: never, after dock to a base, after
    ## undock from a base, every game day, every game month, every game year
    none, dock, undock, daily, monthly, yearly

  GameSettingsRecord* = object
    ## FUNCTION
    ##
    ## Used to store the game's configuration
    autoRest*: bool ## If true, auto rest when pilot or engineer need a rest
    undockSpeed*: ShipSpeed ## The default speed of the player's ship after undock from a base
    autoCenter*: bool ## If true, back to the player's ship after setting destination for it
    autoReturn*: bool ## If true, set the destination for the player's ship to the base after
                        ## finishing a mission
    autoFinish*: bool ## If true, automatically finish the mission if the player's ships is in
                        ## the proper base
    lowFuel*: range[1..10_000] ## The amount of fuel at which the game will show the warning
                                 ## about it
    lowDrinks*: range[1..10_000] ## The amount of drinks at which the game will show the warning
                                   ## about it
    lowFood*: range[1..10_000] ## The amount of food at which the game will show the warning
                                 ## about it
    autoMoveStop*: AutoMoveBreak ## When stop the player's ship's auto movement
    windowWidth*: cint ## The game window default width
    windowHeight: cint ## The game window default height
    messagesLimit: range[10..5_000] ## The max amount of messages to show in the game
    savedMessages: range[5..200] ## The max amount of messages to save to a file
    helpFontSize: range[2..51] ## The size of a font used in help
    mapFontSize: range[2..51] ## The size of a font used on the map
    interfaceFontSize: range[2..51] ## The size of a font used in the game interface
    interfaceTheme: cstring ## The name of the current theme of the game interface
    messagesOrder: MessagesOrder ## In what order the messages should be shown
    autoAskForBases: bool ## If true, auto ask for new bases when the player's ship is
                            ## docked to a base
    autoAskForEvents: bool ## If true, auto ask for new events when the player's ship is
                             ## docked to a base
    showTooltips: bool ## Show the in-game tooltips with help information
    showLastMessages: bool ## Show the last messages window below the map
    messagesPosition: Natural ## The height of the last messages window
    fullScreen: bool ## Run the game in full screen mode
    autoCloseMessagesTime: range[1..60] ## The amount of seconds after which messages' dialogs
                                          ## wil be closed
    autoSave: AutoSaveTime ## How often the game should save itself automatically
    topicsPosition: Natural ## The height of the topics' window position in help window
    showNumbers: bool ## If true, show numbers for speed, skills, attributes, etc.
    rightButton: bool ## If true, use the right mouse button for show menus in various lists
    listsLimit: range[5..100] ## The amount of items displayed in various lists

  BonusType* = range[0.0..5.0]
    ## FUNCTION
    ##
    ## Points' multiplier from various game's settings

  DifficultyType* = enum
    ## FUNCTION
    ##
    ## The level of the game's difficulty. All setttings except custom are preset
    ## levels
    veryEasy, easy, normal, hard, veryHard, custom

  NewGameRecord* = object
    ## FUNCTION
    ##
    ## Used to store the default settings for the new game
    playerName*: cstring ## The player's character name
    playerGender*: char ## The player's character gender
    shipName*: cstring ## The player's ship name
    playerFaction*: cstring ## The player's character faction
    playerCareer*: cstring ## The player's character career
    startingBase*: cstring ## The type of the starting base
    enemyDamageBonus*: cfloat ## The bonus to damage for enemies in ship to ship combat
    playerDamageBonus*: cfloat ## The bonus to damage for the player's character and crew in
                                   ## ship to ship combat
    enemyMeleeDamageBonus*: cfloat ## The bonus to damage for enemies in melee combat
    playerMeleeDamageBonus*: cfloat ## The bonus to damage for the player's character and crew
                                        ## in melee combat
    experienceBonus*: cfloat ## The bonus to the gained by player's character and crew experience
    reputationBonus*: cfloat ## The bonus to the gained the player's character reputation in bases
    upgradeCostBonus*: cfloat ## The bonus to costs of upgrades the player's ship
    pricesBonus*: cfloat ## The bonus to prices in bases
    difficultyLevel*: cstring ## The preset level of difficulty for the game

const
  defaultGameSettings* = GameSettingsRecord(autoRest: true,
    undockSpeed: fullSpeed, autoCenter: true, autoReturn: true,
    autoFinish: true, lowFuel: 100, lowDrinks: 50, lowFood: 25,
    autoMoveStop: never, windowWidth: 800, windowHeight: 600,
    messagesLimit: 500, savedMessages: 10, helpFontSize: 14, mapFontSize: 16,
    interfaceFontSize: 14, interfaceTheme: "steamsky",
    messagesOrder: olderFirst, autoAskForBases: false, autoAskForEvents: false,
    showTooltips: true, showLastMessages: true, messagesPosition: 213,
    fullScreen: false, autoCloseMessagesTime: 6, autoSave: none,
    topicsPosition: 200, showNumbers: false, rightButton: false, listsLimit: 25)
    ## The default setting for the game

  defaultNewGameSettings* = NewGameRecord(playerName: "Laeran",
    playerGender: 'M', shipName: "Anaria", playerFaction: "POLEIS",
    playerCareer: "general", startingBase: "Any", enemyDamageBonus: 1.0,
    playerDamageBonus: 1.0, enemyMeleeDamageBonus: 1.0,
    playerMeleeDamageBonus: 1.0, experienceBonus: 1.0, reputationBonus: 1.0,
    upgradeCostBonus: 1.0, pricesBonus: 1.0, difficultyLevel: "normal")
    ## The default setting for the new game

var
  newGameSettings*: NewGameRecord = defaultNewGameSettings ## The settings for new game
  gameSettings*: GameSettingsRecord = defaultGameSettings ## The general settings for the game

proc loadConfig*() {.sideEffect, raises: [], tags: [RootEffect].} =
  ## FUNCTION
  ##
  ## Load the game and new game settings from the file
  let fileName = saveDirectory & "game.cfg"
  var configFile = newFileStream(filename = fileName, mode = fmRead)
  if configFile == nil:
    return
  var parser: CfgParser
  try:
    parser.open(input = configFile, filename = fileName)
  except OSError, IOError, Exception:
    echo "Can't initialize configuration file parser. Reason: " &
        getCurrentExceptionMsg()
    return

  proc parseAdaFloat(value: string): cfloat =
    ## Temporary function, for backward compatibility with Ada code
    var newValue = value
    newValue.removeSuffix(c = 'E')
    return newValue.parseFloat().cfloat

  while true:
    try:
      let entry = parser.next()
      case entry.kind
      of cfgEof:
        break
      of cfgKeyValuePair, cfgOption:
        case entry.key
        of "PlayerName":
          newGameSettings.playerName = entry.value.cstring
        of "PlayerGender":
          newGameSettings.playerGender = entry.value[0]
        of "ShipName":
          newGameSettings.shipName = entry.value.cstring
        of "PlayerFaction":
          newGameSettings.playerFaction = entry.value.cstring
        of "PlayerCareer":
          newGameSettings.playerCareer = entry.value.cstring
        of "StartingBase":
          newGameSettings.startingBase = entry.value.cstring
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
          newGameSettings.difficultyLevel = ($parseEnum[DifficultyType](
              entry.value.toLowerAscii)).cstring
        of "AutoRest":
          gameSettings.autoRest = entry.value.parseBool()
        of "UndockSpeed":
          gameSettings.undockSpeed = parseEnum[ShipSpeed](
              entry.value.toLowerAscii)
        of "AutoCenter":
          gameSettings.autoCenter = entry.value.parseBool()
        of "AutoReturn":
          gameSettings.autoReturn = entry.value.parseBool()
        of "AutoFinish":
          gameSettings.autoFinish = entry.value.parseBool()
        of "LowFuel":
          gameSettings.lowFuel = entry.value.parseInt().cint
        of "LowDrinks":
          gameSettings.lowDrinks = entry.value.parseInt().cint
        of "LowFood":
          gameSettings.lowFood = entry.value.parseInt().cint
        of "AutoMoveStop":
          gameSettings.autoMoveStop = parseEnum[AutoMoveBreak](
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
          gameSettings.interfaceTheme = entry.value.cstring
        of "MessagesOrder":
          gameSettings.messagesOrder = parseEnum[MessagesOrder](
              entry.value.toLowerAscii)
        of "AutoAskForBases":
          gameSettings.autoAskForBases = entry.value.parseBool()
        of "AutoAskForEvents":
          gameSettings.autoAskForEvents = entry.value.parseBool()
        of "ShowTooltips":
          gameSettings.showTooltips = entry.value.parseBool()
        of "ShowLastMessages":
          gameSettings.showLastMessages = entry.value.parseBool()
        of "MessagesPosition":
          gameSettings.messagesPosition = entry.value.parseInt().cint
        of "FullScreen":
          gameSettings.fullScreen = entry.value.parseBool()
        of "AutoCloseMessagesTime":
          gameSettings.autoCloseMessagesTime = entry.value.parseInt().cint
        of "AutoSave":
          gameSettings.autoSave = parseEnum[AutoSaveTime](
              entry.value.toLowerAscii)
        of "TopicsPosition":
          gameSettings.topicsPosition = entry.value.parseInt().cint
        of "ShowNumbers":
          gameSettings.showNumbers = entry.value.parseBool()
        of "RightButton":
          gameSettings.rightButton = entry.value.parseBool()
        of "ListsLimit":
          gameSettings.listsLimit = entry.value.parseInt().cint
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

proc loadAdaConfig*(adaNewGameSettings: var NewGameRecord) {.exportc.} =
  loadConfig()
  adaNewGameSettings = newGameSettings
