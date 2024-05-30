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

import std/[os, strutils]
import ../[game, tk, types]
import coreui

proc showMessage(message: MessageData; messageView: string;
    messagesType: MessageType) {.sideEffect, raises: [], tags: [].} =
  ## Show the selected message to a player
  ##
  ## * message      - the message to show
  ## * messageView  - the Tcl treeview name in which the message will be show
  ## * messagesType - the selected type of messages to show
  if message.kind != messagesType and messagesType != default:
    return
  let messageTag = (if message.color != white: " [list " & (
      $message.color).toLowerAscii & "]" else: "")
  tclEval(script = messageView & " insert end {" & message.message & "\n}" & messageTag)

proc showLastMessagesCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.exportc.} =
  let
    messagesFrame = mainPaned & ".messagesframe"
    messagesCanvas = messagesFrame & ".canvas"
  if tclEval2(script = "winfo exists " & messagesCanvas) == "0":
    tclEvalFile(fileName = dataDirectory & "ui" & DirSep & "messages.tcl")
    tclEval(script = "bind " & messagesFrame & " <Configure> {ResizeCanvas %W.canvas %w %h}")
  elif tclEval2(script = "winfo ismapped " & messagesCanvas) == "1" and argc == 1:
    tclEval(script = "InvokeButton " & closeButton)
    tclEval(script = "grid remove " & closeButton)
    return tclOk
  let typeBox = messagesCanvas & ".messages.options.types"
  if argc == 1:
    tclEval(script = typeBox & " current 0")
  let searchEntry = messagesCanvas & ".messages.options.search"
  tclEval(script = searchEntry & " delete 0 end")
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the crew UI
  try:
    discard
#    addCommand("ShowLastMessages", showLastMessagesCommand)
  except:
    showError(message = "Can't add a Tcl command.")

# Temporary code for interfacing with Ada

proc showAdaMessageUI(message, messageView: cstring; color, mType,
    messagesType: cint) {.sideEffect, raises: [], tags: [], exportc.} =
  let message = MessageData(message: $message, color: color.MessageColor,
      kind: mType.MessageType)
  showMessage(message = message, messageView = $messageView,
      messagesType = messagesType.MessageType)
