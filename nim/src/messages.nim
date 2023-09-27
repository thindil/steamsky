# Copyright 2022-2023 Bartek thindil Jasicki
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

import config, game, types

type
  MessageDataC* = object
    message*: cstring
    kind*: cint
    color*: cint

func formattedTime(year: cint, month: cint, day: cint, hour: cint,
    minutes: cint): cstring {.gcsafe, raises: [], tags: [], exportc.} =
  ## Format the selected the game time, add leading zeroes, marks between
  ## values, etc.
  ##
  ## * year    - The amount of years to format
  ## * month   - The amount of months to format
  ## * day     - The amount of days to format
  ## * hour    - The amount of hours to format
  ## * minutes - The amount of minutes to format
  ##
  ## Returns the string with formatted time
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

func formattedTime*(year: int, month: int, day: int, hour: int,
    minutes: int): string {.gcsafe, raises: [], tags: [].} =
  ## Format the selected the game time, add leading zeroes, marks between
  ## values, etc.
  ##
  ## * year    - The amount of years to format
  ## * month   - The amount of months to format
  ## * day     - The amount of days to format
  ## * hour    - The amount of hours to format
  ## * minutes - The amount of minutes to format
  ##
  ## Returns the string with formatted time
  result = $year & "-"
  if month < 10:
    result.add("0")
  result.add($month & "-")
  if day < 10:
    result.add("0")
  result.add($day & " ")
  if hour < 10:
    result.add("0")
  result.add($hour & ":")
  if minutes < 10:
    result.add("0")
  result.add($minutes)

proc addMessage*(message: string; mType: MessageType;
    color: MessageColor = white) {.sideEffect, raises: [], tags: [].} =
  ## Add the message to the messages list. Delete the oldest message if the
  ## adding will reach the max limit of messages. Same as above, but uses
  ## the standard types instead of compatible string or int.
  ##
  ## * message - The message to add
  ## * mType   - The type of the message to add
  ## * color   - The color used to draw the message
  if messagesList.len() == gameSettings.messagesLimit:
    messagesList.delete(i = 0)
  messagesList.add(y = MessageData(message: "[" & formattedTime(gameDate.year,
      gameDate.month, gameDate.day, gameDate.hour, gameDate.minutes) & "] " &
      message, kind: mType, color: color))

proc addMessage(message: cstring; kind: cint; color: cint = ord(
    white)) {.sideEffect, raises: [], tags: [], exportc.} =
  ## Add the message to the messages list. Delete the oldest message if the
  ## adding will reach the max limit of messages
  ##
  ## * message - The message to add
  ## * kind    - The kind of the message to add
  ## * color   - The color used to draw the message
  if messagesList.len() == gameSettings.messagesLimit:
    messagesList.delete(i = 0)
  messagesList.add(y = MessageData(message: "[" & $formattedTime(gameDate.year,
      gameDate.month, gameDate.day, gameDate.hour, gameDate.minutes) & "] " &
      $message, kind: kind.MessageType, color: color.MessageColor))

proc getLastMessageIndex*(): cint {.raises: [], tags: [], exportc.} =
  ## Get the index of the last message in the messagesList
  ##
  ## Returns the index of the last message in the messagesList
  return (messagesList.len() - 1).cint

proc getMessage*(messageIndex: cint; kind: cint): MessageDataC {.raises: [],
    tags: [], exportc.} =
  ## Get the selected message of the selected type
  ##
  ## * messageIndex - The index of the message. If positive, it is the index from
  ##                  the begining. If negative it is the index from the last
  ##                  message
  ## * kind         - The type of the message to get
  ##
  ## Returns the selected message data or empty message if the message with the selected
  ## index doesn't exist
  result = MessageDataC(message: "", kind: 0, color: 0)
  if messageIndex - 1 > messagesList.len():
    return
  var index: cint
  if messageIndex < 1:
    if messagesList.len() + messageIndex > 0:
      if kind == ord(MessageType.default):
        index = messagesList.len().cint + messageIndex - 1
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
  if kind == ord(MessageType.default):
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

proc clearMessages*() {.raises: [], tags: [], exportc.} =
  ## Remove all the in-game messages
  messagesList = @[]

proc messagesAmount*(kind: cint): cint {.raises: [], tags: [], exportc.} =
  ## Get the amount of the messages of the selected type
  ##
  ## * kind - The type of messages which amount will be get
  ##
  ## Returns the amount of the selected type of messages
  if kind == ord(MessageType.default):
    return messagesList.len().cint
  result = 0
  for message in messagesList:
    if ord(message.kind).cint == kind:
      result.inc()

proc restoreMessage*(message: string; kind: MessageType = MessageType.default;
    color: MessageColor = white) {.raises: [], tags: [].} =
  ## Restore the selected message from the save file
  ##
  ## * message - The text of the message to restore
  ## * kind    - The kind of the message to restore
  ## * color   - The color used to draw the message
  messagesList.add(MessageData(message: message, kind: kind, color: color))

proc restoreMessage(message: cstring; kind: cint = ord(
    MessageType.default).cint; color: cint = ord(white).cint) {.raises: [],
    tags: [], exportc.} =
  ## Restore the selected message from the save file
  ##
  ## * message - The text of the message to restore
  ## * kind    - The kind of the message to restore
  ## * color   - The color used to draw the message
  messagesList.add(MessageData(message: $message, kind: kind.MessageType,
      color: color.MessageColor))
