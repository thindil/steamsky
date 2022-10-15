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

import std/os

type
  DateRecord* = object
    ## FUNCTION
    ##
    ## Used to store the game's time
    year*: range[0..4_000_000] ## The game's year
    month*: range[0..24] ## The game's month
    day*: range[0..62] ## The game's day
    hour*: range[0..48] ## The game's hour
    minutes*: range[0..120] ## The game's minutes

  DataAction* = enum
    # FUNCTION
    #
    # Possible actions to do when loading game data
    add, update, remove

  MapXRange* = range[1..1_024] ## The size of the game map in X axis
  MapYRange* = range[1..1_024] ## The size of the game map in Y axis
  ReputationRange* = range[-100..100] ## The range of possible reputation levels
  AttributesArray* = array[1 .. 2, Natural] ## 1 - Attribute level, 2 - Attribute experience

var
  saveDirectory*: string = "data" & DirSep & "saves" & DirSep
    ## FUNCTION
    ##
    ## The directory where the saved games and logs are stored
