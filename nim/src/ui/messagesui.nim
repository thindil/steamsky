# Copyright 2024 Bartek thindil Jasicki
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

import std/strutils
import ../[tk, types]

proc showMessage(message: MessageData; messageView: string;
    messagesType: MessageType) =
  if message.kind != messagesType and messagesType != default:
    return
  let messageTag = (if message.color != white: " [list " & (
      $message.color).toLowerAscii & "]" else: "")
  tclEval(script = messageView & " insert end {" & message.message & "\n}" & messageTag)

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the crew UI
  try:
    discard
  except:
    showError(message = "Can't add a Tcl command.")

# Temporary code for interfacing with Ada

proc showAdaMessage(message, messageView: cstring; color, mType,
    messagesType: cint) {.sideEffect, raises: [], tags: [], exportc.} =
  let message = MessageData(message: $message, color: color.MessageColor,
      kind: mType.MessageType)
  showMessage(message = message, messageView = $messageView,
      messagesType = messagesType.MessageType)
