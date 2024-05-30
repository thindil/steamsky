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
import ../[config, game, messages, tk, types]
import coreui, utilsui2

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
  var messagesFrame = mainPaned & ".messagesframe"
  let messagesCanvas = messagesFrame & ".canvas"
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
  let messagesView = messagesCanvas & ".messages.list.view"
  tclEval(script = messagesView & " configure -state normal")
  tclEval(script = messagesView & " delete 1.0 end")
  let messagesType: MessageType = (if argc == 1: default else: ($argv[
      1]).parseInt.MessageType)
  if messagesAmount(kind = messagesType) == 0:
    tclEval(script = messagesView & " insert end {There are no messages of that type.}")
  else:
    if gameSettings.messagesOrder == olderFirst:
      for i in 1 .. messagesAmount():
        showMessage(message = getMessage(messageIndex = i),
            messageView = messagesView, messagesType = messagesType)
    else:
      for i in countdown(messagesAmount(), 1):
        showMessage(message = getMessage(messageIndex = i),
            messageView = messagesView, messagesType = messagesType)
  tclEval(script = messagesView & " configure -state disabled")
  tclEval(script = "grid " & closeButton & " -row 0 -column 1")
  messagesFrame = messagesCanvas & ".messages"
  tclEval(script = messagesCanvas & " configure -height [expr " & tclEval2(
      script = mainPaned & " sashpos 0") & " - 20] -width " & tclEval2(
      script = mainPaned & " cget -width"))
  tclEval(script = "update")
  tclEval(script = messagesCanvas & " create window 0 0 -anchor nw -window " & messagesFrame)
  tclEval(script = "update")
  tclEval(script = messagesCanvas & " configure -scrollregion [list " &
      tclEval2(script = messagesCanvas & " bbox all") & "]")
  showScreen(newScreenName = "messagesframe")
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
