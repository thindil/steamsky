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

{.used.}

type
  MessageType = enum
    ## FUNCTION
    ##
    ## Type of an in-game message
    default, combatMessage, tradeMessage, orderMessage, craftMessage, otherMessage, missionMessage

  MessageColor = enum
    ## FUNCTION
    ##
    ## The color used to show a message
    white, yellow, green, red, blue, cyan

  MessageData = object
    ## FUNCTION
    ##
    ## Used to store data about the game's messages
    message: string ## The message itself
    kind: MessageType ## The type of message
    color: MessageColor ## The color used to show the message

var messagesList: seq[MessageData] ## The list of in-game messages

func formattedTime*(year: cint, month: cint, day: cint, hour: cint,
    minutes: cint): cstring {.gcsafe, raises: [], tags: [], exportc.} =
  ## FUNCTION
  ##
  ## Format the selected the game time, add leading zeroes, marks between
  ## values, etc.
  ##
  ## PARAMETERS
  ##
  ## * year    - The amount of years to format
  ## * month   - The amount of months to format
  ## * day     - The amount of days to format
  ## * hour    - The amount of hours to format
  ## * minutes - The amount of minutes to format
  ##
  ## RETURNS
  ##
  ## The string with formatted time
  var formattedTime: string = $year & "-"
  if month < 10:
    formattedTime.add("0")
  formattedTime.add($month & "-")
  if day < 10:
    formattedTime.add("0")
  formattedTime.add($day & " ")
  if hour < 10:
    formattedTime.add("0")
  formattedTime.add($hour & ":")
  if minutes < 10:
    formattedTime.add("0")
  formattedTime.add($minutes)
  return formattedTime.cstring
