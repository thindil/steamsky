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
import coreui, dialogs, utilsui2

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
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Show the list of last messages to a player
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowLastMessages messagestype
  ## MessagesType is the type of messages to show, default all
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
  let messagesType: MessageType = try:
      (if argc == 1: default else: ($argv[1]).parseInt.MessageType)
    except:
      return showError(message = "Can't get messages type.")
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

proc selectMessagesCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Show only messages of the selected type
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SelectMessages
  let typeBox = mainPaned & ".messagesframe.canvas.messages.options.types"
  return showLastMessagesCommand(clientData = clientData, interp = interp,
      argc = 2, argv = @["SelectMessages", tclEval2(script = typeBox &
      " current")].allocCStringArray)

proc deleteMessagesCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Delete all messages
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## DeleteMessages
  showQuestion(question = "Are you sure you want to clear all messages?",
      res = "messages")
  return tclOk

proc searchMessagesCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.exportc.} =
  let
    frameName = mainPaned & ".messagesframe.canvas.messages"
    messagesView = frameName & ".list.view"
  tclEval(script = messagesView & " configure -state normal")
  tclEval(script = messagesView & " delete 1.0 end")
  let
    searchText = $argv[1]
    typeBox = frameName & ".options.types"
    messagesType = tclEval2(script = typeBox & " current").parseInt.MessageType
  if searchText.len == 0:
    if gameSettings.messagesOrder == olderFirst:
      for i in 1 .. messagesAmount():
        showMessage(message = getMessage(messageIndex = i),
            messageView = messagesView, messagesType = messagesType)
    else:
      for i in countdown(1, messagesAmount()):
        showMessage(message = getMessage(messageIndex = i),
            messageView = messagesView, messagesType = messagesType)
    tclSetResult(value = "1")
    return tclOk
  if gameSettings.messagesOrder == olderFirst:
    for i in 1 .. messagesAmount():
      let message = getMessage(messageIndex = i)
      if message.message.find(sub = searchText) > -1:
        showMessage(message = message, messageView = messagesView, messagesType = messagesType)
  else:
    for i in countdown(1, messagesAmount()):
      let message = getMessage(messageIndex = i)
      if message.message.find(sub = searchText) > -1:
        showMessage(message = message, messageView = messagesView, messagesType = messagesType)
  tclEval(script = messagesView & " configure -state disable")
  tclSetResult(value = "1")
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the crew UI
  try:
    discard
#    addCommand("ShowLastMessages", showLastMessagesCommand)
#    addCommand("SelectMessages", selectMessagesCommand)
#    addCommand("DeleteMessages", deleteMessagesCommand)
#    addCommand("SearchMessages", searchMessagesCommand)
  except:
    showError(message = "Can't add a Tcl command.")

# Temporary code for interfacing with Ada

proc showAdaMessageUI(message, messageView: cstring; color, mType,
    messagesType: cint) {.sideEffect, raises: [], tags: [], exportc.} =
  let message = MessageData(message: $message, color: color.MessageColor,
      kind: mType.MessageType)
  showMessage(message = message, messageView = $messageView,
      messagesType = messagesType.MessageType)
