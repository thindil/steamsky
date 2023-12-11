# Copyright 2023 Bartek thindil Jasicki
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
import ../[config, messages, tk]
import coreui

proc showScreen*(newScreenName: string) {.sideEffect, raises: [], tags: [].} =
  ## Clear the old screen and show the selected to the player
  ##
  ## * newScreenName - the Tcl name of the screen which will be show
  if tclGetVar(varName = "mappreview") == "1" and newScreenName != "mapframe":
    tclUnsetVar(varName = "mappreview")
  const
    paned = mainPaned & ".controls.buttons"
    messagesFrame = mainPaned & ".controls.messages"
  let interp = getInterp()
  if tclEval(script = mainPaned & " panes") == tclError:
    return
  let
    tclResult = $interp.tclGetResult()
    oldSubWindow = tclResult.split()[0]
    subWindow = mainPaned & "." & $newScreenName
  if tclEval(script = mainPaned & " forget " & oldSubWindow) == tclError:
    return
  if tclEval(script = mainPaned & " insert 0 " & subWindow &
      " -weight 1") == tclError:
    return
  if newScreenName in ["optionsframe", "messagesframe"] or
      not gameSettings.showLastMessages:
    if tclEval(script = "grid remove " & messagesFrame) == tclError:
      return
    if newScreenName != "mapframe":
      if tclEval(script = "winfo height " & mainPaned) == tclError:
        return
      let newPos = $interp.tclGetResult()
      if tclEval(script = mainPaned & " sashpos 0 " & newPos) == tclError:
        return
  else:
    if oldSubWindow in [mainPaned & ".messagesframe", mainPaned &
        ".optionsframe"]:
      if tclEval(script = mainPaned & " sashpos 0 " & $(
          gameSettings.windowHeight - gameSettings.messagesPosition)) == tclError:
        return
    if tclEval(script = "grid " & messagesFrame) == tclError:
      return
  if newScreenName == "mapframe":
    if tclEval(script = "grid " & paned) == tclError:
      return
  else:
    if tclEval(script = "grid remove " & paned) == tclError:
      return

proc updateMessages*() =
  let messagesView = mainPaned & ".controls.messages.view"
  tclEval(script = messagesView & " configure -state normal")
  tclEval(script = messagesView & " delete 1.0 end")
  var loopStart = 0 - messagesAmount()
  if loopStart == 0:
    return
  if loopStart < -10:
    loopStart = -10
  if gameSettings.messagesOrder == olderFirst:
    for i in loopStart .. -1:
      let message = getMessage(messageIndex = i + 1)

# Temporary code for interfacing with Ada

proc showAdaScreen(newScreenName: cstring) {.exportc, raises: [], tags: [].} =
  showScreen($newScreenName)
