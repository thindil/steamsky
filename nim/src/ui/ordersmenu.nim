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

import ../[shipscrew, stories, tk, types]
import coreui, dialogs, dialogs2

proc showOrdersCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
  var ordersMenu = createDialog(name = ".gameframe.orders",
      title = "Ship orders")
  if tclEval2(script = "winfo ismapped " & ordersMenu) == "1":
    return closeDialogCommand(clientData = clientData, interp = interp,
        argc = argc, argv = argv)
  if tclGetVar(varName = "gamestate") == "combat":
    tclEval(script = "busy forget " & mainPaned)
    tclEval(script = "busy forget " & gameHeader)
    tclEval(script = "destroy " & ordersMenu)
    return tclOk
  var haveTrader = false
  if findMember(order = talk) > -1:
    haveTrader = true
  if currentStory.index.len > 0:
    let step = (if currentStory.currentStep == -1: storiesList[
        currentStory.index].startingStep elif currentStory.currentStep >
        -1: storiesList[currentStory.index].steps[
        currentStory.currentStep] else: storiesList[
        currentStory.index].finalStep)
  return tclOk

proc addCommands*() =
  addCommand("ShowOrders", showOrdersCommand)

# Temporary code for interfacing with Ada

proc addAdaOrdersMenuCommands() {.raises: [], tags: [RootEffect], exportc.} =
  try:
    addCommands()
  except:
    echo getCurrentExceptionMsg()

