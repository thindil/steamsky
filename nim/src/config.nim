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

type
  AutoMoveBreak* = enum
    never, any, friendly, enemy
  GameSettingsRecord* = object
    ## FUNCTION
    ##
    ## Used to store the game's configuration
    autoRest*: bool
    undockSpeed*: bool
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
