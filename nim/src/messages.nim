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

import config

type
  MessageType* = enum
    ## FUNCTION
    ##
    ## Type of an in-game message
    default, combatMessage, tradeMessage, orderMessage, craftMessage,
        otherMessage, missionMessage

  MessageColor* = enum
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

  MessageDataC* = object
    message*: cstring
    kind: cint
    color: cint

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

proc addMessage*(message: cstring; kind: cint; color: cint = ord(
    white)) {.raises: [], tags: [], exportc.} =
  ## FUNCTION
  ##
  ## Add the message to the messages list. Delete the oldest message if the
  ## adding will reach the max limit of messages
  ##
  ## PARAMETERS
  ##
  ## * message - The message to add
  ## * kind    - The kind of the message to add
  ## * color   - The color used to draw the message
  if messagesList.len() == gameSettings.messagesLimit:
    messagesList.delete(i = 0)
  messagesList.add(y = MessageData(message: $message, kind: kind.MessageType,
      color: color.MessageColor))

proc getLastMessageIndex*(): cint {.raises: [], tags: [], exportc.} =
  ## FUNCTION
  ##
  ## Get the index of the last message in the messagesList
  ##
  ## RETURNS
  ##
  ## The index of the last message in the messagesList
  return (messagesList.len() - 1).cint

proc getMessage*(messageIndex: cint; kind: cint): MessageDataC {.raises: [],
    tags: [], exportc.} =
  ## FUNCTION
  ##
  ## Get the selected message of the selected type
  ##
  ## PARAMETERS
  ##
  ## * messageIndex - The index of the message. If positive, it is the index from
  ##                  the begining. If negative it is the index from the last
  ##                  message
  ## * kind         - The type of the message to get
  ##
  ## RETURNS
  ##
  ## The selected message data or empty message if the message with the selected
  ## index doesn't exist
  result = MessageDataC(message: "", kind: 0, color: 0)
  if messageIndex - 1 > messagesList.len():
    return
  var index: cint
  if messageIndex < 1:
    if messagesList.len() + messageIndex > 0:
      if kind == ord(default):
        index = messagesList.len().cint - messageIndex
        return MessageDataC(message: messagesList[index].message.cstring,
            kind: ord(messagesList[index].kind).cint, color: ord(messagesList[
            index].color).cint)
      index = 1
      for i in countdown(messagesList.len() - 1, 0):
        let message = MessageDataC(message: messagesList[i].message.cstring,
            kind: ord(messagesList[i].kind).cint, color: ord(messagesList[i].color).cint)
        if message.kind == kind:
          index.dec()
        if index == messageIndex:
          return message
    return
  if kind == ord(default):
    index = messageIndex - 1
    return MessageDataC(message: messagesList[index].message.cstring, kind: ord(
        messagesList[index].kind).cint, color: ord(messagesList[
        index].color).cint)
  index = 0
  for i in countup(0, messagesList.len() - 1):
    let message = MessageDataC(message: messagesList[i].message.cstring,
        kind: ord(messagesList[i].kind).cint, color: ord(messagesList[i].color).cint)
    if message.kind == kind:
      index.inc()
    if index == messageIndex:
      return message
