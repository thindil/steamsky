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
    undockSpeed*: ShipSpeed
    autoCenter*: bool
    autoReturn*: bool
    autoFinish*: bool
    lowFuel*: range[1..10_000]
    lowDrinks*: range[1..10_000]
    lowFood*: range[1..10_000]
    autoMoveStop*: AutoMoveBreak
    windowWidth*: cint
    windowHeight: cint
    messagesLimit: range[10..5_000]
    savedMessages: range[5..200]
    helpFontSize: range[2..51]
    mapFontSize: range[2..51]
    interfaceFontSize: range[2..51]
    interfaceTheme: cstring
    messagesOrder: MessagesOrder
    autoAskForBases: bool
    autoAskForEvents: bool
    showTooltips: bool
    showLastMessages: bool
    messagesPosition: Natural
    fullScreen: bool
    autoCloseMessagesTime: range[1..60]
    autoSave: AutoSaveTime
    topicsPosition: Natural
    showNumbers: bool
    rightButton: bool
    listsLimit: range[5..100]
