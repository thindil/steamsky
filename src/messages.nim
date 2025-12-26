# Copyright 2022-2025 Bartek thindil Jasicki
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

## Provides code related to in-game messages, like adding, removing, finding
## them, etc.

import contracts
import config, game, types

proc formattedTime*(time: DateRecord = gameDate): string {.raises: [], tags: [],
    contractual.} =
  ## Format the selected the game time, add leading zeroes, marks between
  ## values, etc.
  ##
  ## * time - The time to format. Default value is the current in-game date
  ##
  ## Returns the string with formatted time
  result = $time.year & "-"
  if time.month < 10:
    result.add(y = "0")
  result.add(y = $time.month & "-")
  if time.day < 10:
    result.add(y = "0")
  result.add(y = $time.day & " ")
  if time.hour < 10:
    result.add(y = "0")
  result.add(y = $time.hour & ":")
  if time.minutes < 10:
    result.add(y = "0")
  result.add(y = $time.minutes)

proc addMessage*(message: MessageText; mType: MessageType;
    color: MessageColor = white) {.raises: [], tags: [], contractual.} =
  ## Add the message to the messages list. Delete the oldest message if the
  ## adding will reach the max limit of messages. Same as above, but uses
  ## the standard types instead of compatible string or int.
  ##
  ## * message - The message to add
  ## * mType   - The type of the message to add
  ## * color   - The color used to draw the message
  require:
    message.len > 0
  body:
    if messagesList.len() == gameSettings.messagesLimit:
      messagesList.delete(i = 0)
    messagesList.add(y = MessageData(message: "[" & formattedTime(
        time = gameDate) & "] " & message, kind: mType, color: color))

proc getLastMessageIndex*(): int {.raises: [], tags: [], contractual.} =
  ## Get the index of the last message in the messagesList
  ##
  ## Returns the index of the last message in the messagesList
  return (messagesList.len() - 1)

proc getMessage*(messageIndex: int; kind: MessageType = default): MessageData {.raises: [
    ], tags: [], contractual.} =
  ## Get the selected message of the selected type
  ##
  ## * messageIndex - The index of the message. If positive, it is the index from
  ##                  the begining. If negative it is the index from the last
  ##                  message
  ## * kind         - The type of the message to get
  ##
  ## Returns the selected message data or empty message if the message with the selected
  ## index doesn't exist
  result = MessageData(message: "", kind: default, color: white)
  if messageIndex - 1 > messagesList.len:
    return
  var index: int = -1
  if messageIndex < 1:
    if messagesList.len + messageIndex > 0:
      if kind == default:
        index = messagesList.len + messageIndex - 1
        return MessageData(message: messagesList[index].message,
            kind: messagesList[index].kind, color: messagesList[index].color)
      index = 1
      for i in countdown(a = messagesList.len - 1, b = 0):
        let message: MessageData = MessageData(message: messagesList[i].message,
            kind: messagesList[i].kind, color: messagesList[i].color)
        if message.kind == kind:
          index.dec()
        if index == messageIndex:
          return message
    return
  if kind == MessageType.default:
    index = messageIndex - 1
    return MessageData(message: messagesList[index].message, kind:
      messagesList[index].kind, color: messagesList[
      index].color)
  index = 0
  for i in countup(a = 0, b = messagesList.len - 1):
    let message: MessageData = MessageData(message: messagesList[i].message,
        kind: messagesList[i].kind, color: messagesList[i].color)
    if message.kind == kind:
      index.inc()
    if index == messageIndex:
      return message

proc clearMessages*() {.raises: [], tags: [], contractual.} =
  ## Remove all the in-game messages
  messagesList = @[]

proc messagesAmount*(kind: MessageType = default): int {.raises: [], tags: [],
    contractual.} =
  ## Get the amount of the messages of the selected type
  ##
  ## * kind - The type of messages which amount will be get
  ##
  ## Returns the amount of the selected type of messages
  if kind == default:
    return messagesList.len
  result = 0
  for message in messagesList:
    if message.kind == kind:
      result.inc

proc restoreMessage*(message: MessageText; kind: MessageType = MessageType.default;
    color: MessageColor = white) {.raises: [], tags: [], contractual.} =
  ## Restore the selected message from the save file
  ##
  ## * message - The text of the message to restore
  ## * kind    - The kind of the message to restore
  ## * color   - The color used to draw the message
  require:
    message.len > 0
  body:
    messagesList.add(y = MessageData(message: message, kind: kind, color: color))
