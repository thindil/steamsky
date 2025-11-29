# Copyright 2024-2025 Bartek thindil Jasicki
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

## Provides code related to showing last in-game messages like showing their
## list, selecting and deleting messages, etc.

import std/strutils
import contracts, nimalyzer
import ../[config, messages, tk, types]
import coreui, dialogs, errordialog, utilsui2

proc showMessage(message: MessageData; messageView: string;
    messagesType: MessageType) {.raises: [], tags: [], cdecl, contractual,
    ruleOff: "params".} =
  ## Show the selected message to a player
  ##
  ## * message      - the message to show
  ## * messageView  - the Tcl treeview name in which the message will be show
  ## * messagesType - the selected type of messages to show
  if message.kind != messagesType and messagesType != default:
    return
  let messageTag: string = (if message.color == white: "" else: " [list " & (
      $message.color).toLowerAscii & "]")
  tclEval(script = messageView & " insert end {" & message.message & "\n}" & messageTag)

proc showLastMessagesCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
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
  var messagesFrame: string = mainPaned & ".messagesframe"
  let messagesCanvas: string = messagesFrame & ".canvas"
  if tclEval2(script = "winfo exists " & messagesCanvas) == "0":
    tclEval(script = """
      ttk::frame .gameframe.paned.messagesframe
      set messagescanvas [canvas .gameframe.paned.messagesframe.canvas \
         -xscrollcommand [list .gameframe.paned.messagesframe.scrollx set]]
      pack $messagescanvas -side top -fill both
      pack [ttk::scrollbar .gameframe.paned.messagesframe.scrollx \
         -orient horizontal -command [list $messagescanvas xview]] -fill x
      ::autoscroll::autoscroll .gameframe.paned.messagesframe.scrollx
      set messagesframe [ttk::frame $messagescanvas.messages]
      # Messages options
      grid [ttk::frame $messagesframe.options] -sticky w
      grid [ttk::combobox $messagesframe.options.types \
         -values [list All Combat Trade Orders Craft Others Missions] \
         -state readonly -width 10]
      tooltip::tooltip $messagesframe.options.types \
         {Select the type of messages to show}
      bind $messagesframe.options.types <<ComboboxSelected>> SelectMessages
      $messagesframe.options.types current 0
      grid [ttk::entry $messagesframe.options.search -validate key \
         -validatecommand {SearchMessages %P} -width 30] -row 0 -column 1
      tooltip::tooltip $messagesframe.options.search \
         {Search for the selected text in the messages}
      grid [ttk::button $messagesframe.options.delete -text {Delete all messages} \
         -command DeleteMessages] -row 0 -column 2
      tooltip::tooltip $messagesframe.options.delete {Clear all messages}
      # Messages list
      grid [ttk::frame $messagesframe.list] -sticky nwes
      set messagesview2 [text $messagesframe.list.view -width 10 -height 10 \
         -yscrollcommand [list $messagesframe.list.scrolly set]]
      $messagesview2 tag configure yellow -foreground \
         [ttk::style lookup Messages -yellow]
      $messagesview2 tag configure green -foreground \
         [ttk::style lookup Messages -green]
      $messagesview2 tag configure red -foreground \
         [ttk::style lookup Messages -red]
      $messagesview2 tag configure cyan -foreground \
         [ttk::style lookup Messages -cyan]
      $messagesview2 tag configure blue -foreground \
         [ttk::style lookup Messages -blue]
      $messagesview2 tag configure gray -foreground \
         [ttk::style lookup Messages -gray]
      pack [ttk::scrollbar $messagesframe.list.scrolly -orient vertical \
         -command [list $messagesview2 yview]] -side right -fill y
      pack $messagesview2 -side top -fill both -expand true
      ::autoscroll::autoscroll $messagesframe.list.scrolly
      SetScrollbarBindings $messagescanvas $messagesframe.list.scrolly
      bind $messagescanvas <Configure> {
         $messagesview2 configure -height [expr [winfo height $messagescanvas] \
            / [font metrics InterfaceFont -linespace] - 1] -width [expr %w \
            / [font measure InterfaceFont {  }] + 4]
      }
    """)
    tclEval(script = "bind " & messagesFrame & " <Configure> {ResizeCanvas %W.canvas %w %h}")
  elif tclEval2(script = "winfo ismapped " & messagesCanvas) == "1" and argc == 1:
    tclEval(script = "InvokeButton " & closeButton)
    tclEval(script = "grid remove " & closeButton)
    return tclOk
  let typeBox: string = messagesCanvas & ".messages.options.types"
  if argc == 1:
    tclEval(script = typeBox & " current 0")
  let searchEntry: string = messagesCanvas & ".messages.options.search"
  tclEval(script = searchEntry & " delete 0 end")
  let messagesView: string = messagesCanvas & ".messages.list.view"
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
      for i in 1..messagesAmount():
        showMessage(message = getMessage(messageIndex = i),
            messageView = messagesView, messagesType = messagesType)
    else:
      for i in countdown(a = messagesAmount(), b = 1):
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
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
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
  let typeBox: string = mainPaned & ".messagesframe.canvas.messages.options.types"
  return showLastMessagesCommand(clientData = clientData, interp = interp,
      argc = 2, argv = @["SelectMessages", tclEval2(script = typeBox &
      " current")].allocCStringArray)

proc deleteMessagesCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], cdecl,
        contractual.} =
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
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Show only this messages which contains the selected sequence
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SearchMessages text
  ## Text is the string to search in the messages
  let
    frameName: string = mainPaned & ".messagesframe.canvas.messages"
    messagesView: string = frameName & ".list.view"
  tclEval(script = messagesView & " configure -state normal")
  tclEval(script = messagesView & " delete 1.0 end")
  let
    searchText: string = $argv[1]
    typeBox: string = frameName & ".options.types"
    messagesType: MessageType = try:
        tclEval2(script = typeBox & " current").parseInt.MessageType
      except:
        return showError(message = "Can't get messages' type.")
  if searchText.len == 0:
    if gameSettings.messagesOrder == olderFirst:
      for i in 1..messagesAmount():
        showMessage(message = getMessage(messageIndex = i),
            messageView = messagesView, messagesType = messagesType)
    else:
      for i in countdown(a = 1, b = messagesAmount()):
        showMessage(message = getMessage(messageIndex = i),
            messageView = messagesView, messagesType = messagesType)
    tclSetResult(value = "1")
    return tclOk
  if gameSettings.messagesOrder == olderFirst:
    for i in 1..messagesAmount():
      let message: MessageData = getMessage(messageIndex = i)
      if message.message.find(sub = searchText) > -1:
        showMessage(message = message, messageView = messagesView,
            messagesType = messagesType)
  else:
    for i in countdown(a = 1, b = messagesAmount()):
      let message: MessageData = getMessage(messageIndex = i)
      if message.message.find(sub = searchText) > -1:
        showMessage(message = message, messageView = messagesView,
            messagesType = messagesType)
  tclEval(script = messagesView & " configure -state disable")
  tclSetResult(value = "1")
  return tclOk

proc addCommands*() {.raises: [], tags: [WriteIOEffect, TimeEffect, RootEffect],
    contractual.} =
  ## Adds Tcl commands related to the crew UI
  try:
    addCommand(name = "ShowLastMessages", nimProc = showLastMessagesCommand)
    addCommand(name = "SelectMessages", nimProc = selectMessagesCommand)
    addCommand(name = "DeleteMessages", nimProc = deleteMessagesCommand)
    addCommand(name = "SearchMessages", nimProc = searchMessagesCommand)
  except:
    showError(message = "Can't add a Tcl command.")
