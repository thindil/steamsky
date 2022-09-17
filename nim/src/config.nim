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

import ships

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

const defaultGameSettings* = GameSettingsRecord(autoRest: true,
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
