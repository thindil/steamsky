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

## Provides code for debug UI, like showing the UI, changing various things in
## the game, etc.

import std/[strutils, tables]
import contracts, nimalyzer
import ../[basestypes, events, game, gamesaveload, items, maps, shipscargo, tk, types]
import errordialog, mapsui

proc refreshModuleCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
    TimeEffect, RootEffect], cdecl, contractual, ruleOff: "params".} =
  ## Refresh the information about selected module
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## RefreshModule
  const
    frameName: string = ".debugdialog.main.ship"
    moduleCombo: string = frameName & ".module"
  let
    moduleIndex: int = try:
        tclEval2(script = moduleCombo & " current").parseInt
      except:
        return showError(message = "Can't get the module's index.")
    protoCombo: string = frameName & ".proto"
  try:
    tclEval(script = protoCombo & " set {" & modulesList[playerShip.modules[
        moduleIndex].protoIndex].name & "}")
  except:
    return showError(message = "Can't get the proto module.")
  var spinBox: string = frameName & ".weight"
  tclEval(script = spinBox & " set " & $playerShip.modules[moduleIndex].weight)
  spinBox = frameName & ".dur"
  tclEval(script = spinBox & " set " & $playerShip.modules[
      moduleIndex].durability)
  spinBox = frameName & ".maxdur"
  tclEval(script = spinBox & " set " & $playerShip.modules[
      moduleIndex].maxDurability)
  spinBox = frameName & ".upgrade"
  tclEval(script = spinBox & " set " & $playerShip.modules[
      moduleIndex].upgradeProgress)
  return tclOk

proc refreshMemberCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Refresh the information about selected crew member
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## RefreshMember
  const frameName: string = ".debugdialog.main.crew"
  var comboBox: string = frameName & ".member"
  let
    memberIndex: int = try:
        tclEval2(script = comboBox & " current").parseInt
      except:
        return showError(message = "Can't get the member index.")
    member: MemberData = playerShip.crew[memberIndex]
  var spinBox: string = frameName & ".stats2.health"
  tclEval(script = spinBox & " set " & $member.health)
  spinBox = frameName & ".stats2.thirst"
  tclEval(script = spinBox & " set " & $member.thirst)
  spinBox = frameName & ".stats2.hunger"
  tclEval(script = spinBox & " set " & $member.hunger)
  spinBox = frameName & ".stats2.tired"
  tclEval(script = spinBox & " set " & $member.tired)
  spinBox = frameName & ".stats2.morale"
  tclEval(script = spinBox & " set " & $member.morale[1])
  spinBox = frameName & ".stats2.loyalty"
  tclEval(script = spinBox & " set " & $member.loyalty)
  var
    memberFrame: string = frameName & ".stats"
    rows: Natural = try:
        tclEval2(script = "grid size " & memberFrame).split[1].parseInt
      except:
        return showError(message = "Can't get the amount of rows.")
  deleteWidgets(startIndex = 1, endIndex = rows - 1, frame = memberFrame)
  for index, attribute in member.attributes:
    let label: string = memberFrame & ".label" & $(index + 1)
    tclEval(script = "ttk::label " & label & " -text {" & attributesList[
        index].name & "}")
    tclEval(script = "grid " & label)
    spinBox = memberFrame & ".value" & $(index + 1)
    tclEval(script = "ttk::spinbox " & spinBox &
        " -from 1 -to 50 -validate key -validatecommand {ValidateSpinbox %W %P " &
        frameName & ".change} -width 5")
    tclEval(script = spinBox & " set " & $attribute.level)
    tclEval(script = "grid " & spinBox & " -column 1 -row " & $(index + 1))
  memberFrame = frameName & ".skills"
  rows = try:
      tclEval2(script = "grid size " & memberFrame).split[1].parseInt
    except:
      return showError(message = "Can't get the amount of rows (2).")
  deleteWidgets(startIndex = 1, endIndex = rows - 1, frame = memberFrame)
  var skillsIndexes: seq[Natural] = @[]
  for index, skill in member.skills:
    let label: string = memberFrame & ".label" & $(index + 1)
    try:
      tclEval(script = "ttk::label " & label & " -text {" & skillsList[
          skill.index].name & "}")
    except:
      return showError(message = "Can't add the skill label.")
    tclEval(script = "grid " & label)
    spinBox = memberFrame & ".value" & $(index + 1)
    tclEval(script = "ttk::spinbox " & spinBox &
        " -from 1 -to 100 -validate key -validatecommand {ValidateSpinbox %W %P " &
        frameName & ".change} -width 5")
    tclEval(script = spinBox & " set " & $skill.level)
    tclEval(script = "grid " & spinBox & " -column 1 -row " & $(index + 1))
    skillsIndexes.add(y = skill.index)
  var skillsListValues: string = ""
  for index, skill in skillsList:
    if index notin skillsIndexes:
      skillsListValues.add(y = " " & skill.name)
  comboBox = frameName & ".addskill.skills"
  tclEval(script = comboBox & " configure -values [list" & $skillsListValues & "]")
  tclEval(script = comboBox & " current 0")
  return tclOk

proc refreshCargoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Refresh the information about the player ship cargo
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## RefreshCargo
  const
    frameName: string = ".debugdialog.main.cargo"
    cargoCombo: string = frameName & ".update"
  let
    itemIndex: Natural = try:
        tclEval2(script = cargoCombo & " current").parseInt
      except:
        return showError(message = "Can't get the item index.")
    amountBox: string = frameName & ".updateamount"
  tclEval(script = amountBox & " set " & $playerShip.cargo[itemIndex].amount)
  return tclOk

proc refreshEventsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Refresh the list of events
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## RefreshEvents
  const
    frameName: string = ".debugdialog.main.world.deleteevent"
    eventsButton: string = frameName & ".deleteevent"
    eventsBox: string = frameName & ".delete"
  if eventsList.len == 0:
    tclEval(script = "grid remove " & eventsButton)
    tclEval(script = "grid remove " & eventsBox)
    return tclOk
  tclEval(script = "grid " & eventsButton)
  tclEval(script = "grid " & eventsBox)
  var valuesList: string = ""
  for index, event in eventsList:
    try:
      case event.eType
      of enemyShip:
        valuesList.add(y = " {Enemy ship: " & protoShipsList[
            event.shipIndex].name & "}")
      of attackOnBase:
        valuesList.add(y = " {Attack on base: " & protoShipsList[
            event.shipIndex].name & "}")
      of disease:
        valuesList.add(y = " {Disease in base: " & skyBases[skyMap[event.skyX][
            event.skyY].baseIndex].name & "}")
      of doublePrice:
        valuesList.add(y = " {Double price in base: " & skyBases[skyMap[
            event.skyX][event.skyY].baseIndex].name & "}")
      of fullDocks:
        valuesList.add(y = " {Full docks in base: " & skyBases[skyMap[
            event.skyX][event.skyY].baseIndex].name & "}")
      of enemyPatrol:
        valuesList.add(y = " {Enemy patrol: " & protoShipsList[
            event.shipIndex].name & "}")
      of trader:
        valuesList.add(y = " {Trader: " & protoShipsList[event.shipIndex].name & "}")
      of friendlyShip:
        valuesList.add(y = " {Friendly ship: " & protoShipsList[
            event.shipIndex].name & "}")
      else:
        discard
    except:
      return showError(message = "Can't add event to the list.")
  tclEval(script = eventsBox & " configure -values [list" & $valuesList & "]")
  tclEval(script = eventsBox & " current 0")
  return tclOk

proc refreshCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Refresh the whole game information
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## Refresh
  const frameName: string = ".debugdialog.main"
  var spinBox: string = frameName & ".ship.x"
  tclEval(script = spinBox & " set " & $playerShip.skyX)
  spinBox = frameName & ".ship.y"
  tclEval(script = spinBox & " set " & $playerShip.skyY)
  var valuesList: string = ""
  for module in playerShip.modules:
    valuesList.add(y = " {" & module.name & "}")
  var comboBox: string = frameName & ".ship.module"
  tclEval(script = comboBox & " configure -values [list" & valuesList & "]")
  tclEval(script = comboBox & " current 0")
  discard refreshModuleCommand(clientData = clientData, interp = interp,
      argc = argc, argv = argv)
  comboBox = frameName & ".crew.member"
  valuesList = ""
  for member in playerShip.crew:
    valuesList.add(y = " {" & member.name & "}")
  tclEval(script = comboBox & " configure -values [list" & valuesList & "]")
  tclEval(script = comboBox & " current 0")
  discard refreshMemberCommand(clientData = clientData, interp = interp,
      argc = argc, argv = argv)
  comboBox = frameName & ".cargo.update"
  valuesList = ""
  for item in playerShip.cargo:
    valuesList.add(y = " {" & getItemName(item = item, damageInfo = false,
        toLower = false) & "}")
  tclEval(script = comboBox & " configure -values [list" & valuesList & "]")
  tclEval(script = comboBox & " current 0")
  discard refreshCargoCommand(clientData = clientData, interp = interp,
      argc = argc, argv = argv)
  discard refreshEventsCommand(clientData = clientData, interp = interp,
      argc = argc, argv = argv)
  return tclOk

proc refreshBaseCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Refresh the information about the selected base
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## RefreshBase
  const
    frameName: string = ".debugdialog.main.bases"
    nameEntry: string = frameName & ".name"
  let baseName: string = tclEval2(script = nameEntry & " get")
  var baseIndex: int = 0
  for index, base in skyBases:
    if base.name == baseName:
      baseIndex = index
      break
  if baseIndex == 0:
    return tclOk
  var comboBox: string = frameName & ".type"
  try:
    tclEval(script = comboBox & " set " & basesTypesList[skyBases[
        baseIndex].baseType].name)
  except:
    return showError(message = "Can't set base's type.")
  comboBox = frameName & ".owner"
  try:
    tclEval(script = comboBox & " set " & factionsList[skyBases[
        baseIndex].owner].name)
  except:
    return showError(message = "Can't set owner's name.")
  comboBox = frameName & ".size"
  tclEval(script = comboBox & " current " & $(skyBases[baseIndex].size.ord))
  var spinBox: string = frameName & ".population"
  tclEval(script = spinBox & " set " & $skyBases[baseIndex].population)
  spinBox = frameName & ".reputation"
  tclEval(script = spinBox & " set " & $skyBases[baseIndex].reputation.level)
  spinBox = frameName & ".money"
  if skyBases[baseIndex].cargo.len > 0:
    tclEval(script = spinBox & " set " & $skyBases[baseIndex].cargo[0].amount)
  else:
    tclEval(script = spinBox & " set 0")
  return tclOk

proc debugSaveGameCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
        RootEffect], cdecl, contractual.} =
  ## Save the game
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## DebugSaveGame
  try:
    saveGame(prettyPrint = true)
  except:
    return showError(message = "Can't save the game.")
  return tclOk

proc debugMoveShipCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Move the player ship
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## DebugMoveShip
  const frameName: string = ".debugdialog.main.ship"
  var spinBox: string = frameName & ".x"
  playerShip.skyX = try:
      tclEval2(script = spinBox & " get").parseInt
    except:
      return showError(message = "Can't get X coord.")
  spinBox = frameName & ".y"
  playerShip.skyY = try:
      tclEval2(script = spinBox & " get").parseInt
    except:
      return showError(message = "Can't get Y coord.")
  showSkyMap(clear = true)
  return tclOk

proc debugUpdateModuleCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Update the selected module
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## DebugUpdateModule
  const
    frameName: string = ".debugdialog.main.ship"
    moduleBox: string = frameName & ".module"
  let
    moduleIndex: int = try:
        tclEval2(script = moduleBox & " current").parseInt
      except:
        return showError(message = "Can't get module index.")
    protoCombo: string = frameName & ".proto"
  var value: string = tclEval2(script = protoCombo & " get")
  for index, module in modulesList:
    if module.name == value:
      value = ""
      playerShip.modules[moduleIndex].protoIndex = index
      break
  var spinBox: string = frameName & ".weight"
  playerShip.modules[moduleIndex].weight = try:
      tclEval2(script = spinBox &
        " get").parseInt
    except:
      return showError(message = "Can't set weight.")
  spinBox = frameName & ".dur"
  playerShip.modules[moduleIndex].durability = try:
      tclEval2(script = spinBox &
        " get").parseInt
    except:
      return showError(message = "Can't set durability.")
  spinBox = frameName & ".maxdur"
  playerShip.modules[moduleIndex].maxDurability = try:
      tclEval2(script = spinBox &
        " get").parseInt
    except:
      return showError(message = "Can't set max durability.")
  spinBox = frameName & ".upgrade"
  playerShip.modules[moduleIndex].upgradeProgress = try:
      tclEval2(script = spinBox &
        " get").parseInt
    except:
      return showError(message = "Can't set upgrade progress.")
  return tclOk

proc debugAddSkillCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Add a new skill to the selected crew member
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## DebugAddSkill
  const frameName: string = ".debugdialog.main.crew"
  var comboBox: string = frameName & ".member"
  let memberIndex: int = try:
      tclEval2(script = comboBox & " current").parseInt
    except:
      return showError(message = "Can't get member index.")
  comboBox = frameName & ".addskill.skills"
  var skillName: string = tclEval2(script = comboBox & " get")
  for index, skill in skillsList:
    if skill.name == skillName:
      playerShip.crew[memberIndex].skills.add(y = SkillInfo(index: index,
          level: 1, experience: 0))
      return refreshMemberCommand(clientData = clientData, interp = interp,
          argc = argc, argv = argv)
  return tclOk

proc debugUpdateMemberCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Update the selected crew member
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## DebugUpdateMember
  const
    frameName: string = ".debugdialog.main.crew"
    comboBox: string = frameName & ".member"
  let
    memberIndex: int = try:
        tclEval2(script = comboBox & " current").parseInt
      except:
        return showError(message = "Can't get member index.")
  var spinBox: string = frameName & ".stats2.health"
  try:
    playerShip.crew[memberIndex].health = tclEval2(script = spinBox &
        " get").parseInt
  except:
    return showError(message = "Can't set member health.")
  spinBox = frameName & ".stats2.thirst"
  try:
    playerShip.crew[memberIndex].thirst = tclEval2(script = spinBox &
        " get").parseInt
  except:
    return showError(message = "Can't set member thirst.")
  spinBox = frameName & ".stats2.hunger"
  try:
    playerShip.crew[memberIndex].hunger = tclEval2(script = spinBox &
        " get").parseInt
  except:
    return showError(message = "Can't set member hunger.")
  spinBox = frameName & ".stats2.tired"
  try:
    playerShip.crew[memberIndex].tired = tclEval2(script = spinBox &
        " get").parseInt
  except:
    return showError(message = "Can't set member tired.")
  spinBox = frameName & ".stats2.morale"
  try:
    playerShip.crew[memberIndex].morale[1] = tclEval2(script = spinBox &
        " get").parseInt
  except:
    return showError(message = "Can't set member morale.")
  spinBox = frameName & ".stats2.loyalty"
  try:
    playerShip.crew[memberIndex].loyalty = tclEval2(script = spinBox &
        " get").parseInt
  except:
    return showError(message = "Can't set member loyalty.")
  for index, attrib in playerShip.crew[memberIndex].attributes.mpairs:
    spinBox = frameName & ".stats.value" & $(index + 1)
    try:
      attrib.level = tclEval2(script = spinBox & " get").parseInt
    except:
      return showError(message = "Can't set member attribute.")
  for index, skill in playerShip.crew[memberIndex].skills.mpairs:
    spinBox = frameName & ".skills.value" & $(index + 1)
    try:
      skill.level = tclEval2(script = spinBox & " get").parseInt
    except:
      return showError(message = "Can't set member skill.")
  return tclOk

proc debugAddItemCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Add a new item to the player ship cargo
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## DebugAddItem
  const
    frameName: string = ".debugdialog.main.cargo"
    itemEntry: string = frameName & ".add"
    itemBox: string = frameName & ".amount"
  let itemName: string = tclEval2(script = itemEntry & " get")
  var itemIndex: int = -1
  for index, item in itemsList:
    if item.name == itemName:
      itemIndex = index
      break
  if itemIndex == -1:
    return tclOk
  try:
    updateCargo(ship = playerShip, protoIndex = itemIndex, amount = tclEval2(
        script = itemBox & " get").parseInt, quality = normal)
  except:
    return showError(message = "Can't update the cargo.")
  return refreshCommand(clientData = clientData, interp = interp, argc = argc, argv = argv)

proc debugUpdateItemCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Update the amount of an item in the player ship cargo
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## DebugUpdateItem
  const
    frameName: string = ".debugdialog.main.cargo"
    itemCombo: string = frameName & ".update"
    itemBox: string = frameName & ".updateamount"
  let
    itemIndex: int = try:
        tclEval2(script = itemCombo & " current").parseInt
      except:
        return showError(message = "Can't geet item index.")
  try:
    updateCargo(ship = playerShip, amount = tclEval2(script = itemBox &
        " get").parseInt, cargoIndex = itemIndex, quality = playerShip.cargo[
            itemIndex].quality)
  except:
    return showError(message = "Can't update the cargo.")
  return refreshCommand(clientData = clientData, interp = interp, argc = argc, argv = argv)

proc debugUpdateBaseCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Update the selected base
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## DebugUpdateBase
  const
    frameName: string = ".debugdialog.main.bases"
    baseEntry: string = frameName & ".name"
  let baseName: string = tclEval2(script = baseEntry & " get")
  var baseIndex: ExtendedBasesRange = 0
  for index, base in skyBases:
    if base.name == baseName:
      baseIndex = index
      break
  if baseIndex == 0:
    return tclOk
  var baseCombo: string = frameName & ".type"
  for index, baseType in basesTypesList:
    if baseType.name == tclEval2(script = baseCombo & " get"):
      skyBases[baseIndex].baseType = index
      break
  baseCombo = frameName & ".owner"
  for index, faction in factionsList:
    if faction.name == tclEval2(script = baseCombo & " get"):
      skyBases[baseIndex].owner = index
      break
  baseCombo = frameName & ".size"
  try:
    skyBases[baseIndex].size = parseEnum[BasesSize](s = tclEval2(
        script = baseCombo & " get").toLowerAscii)
  except:
    return showError(message = "Can't set the base's size.")
  var baseBox: string = frameName & ".population"
  try:
    skyBases[baseIndex].population = tclEval2(script = baseBox &
        " get").parseInt
  except:
    return showError(message = "Can't set the base's population.")
  baseBox = frameName & ".reputation"
  try:
    skyBases[baseIndex].reputation.level = tclEval2(script = baseBox &
        " get").parseInt
  except:
    return showError(message = "Can't set the base's reputation.")
  baseBox = frameName & ".money"
  try:
    skyBases[baseIndex].cargo[0].amount = tclEval2(script = baseBox &
        " get").parseInt
  except:
    return showError(message = "Can't set the base's money.")
  return tclOk

proc debugAddShipCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Add a new ship based event to the game
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## DebugAddShip
  const
    frameName: string = ".debugdialog.main.world"
    shipEntry: string = frameName & ".ship"
  let shipName: string = tclEval2(script = shipEntry & " get")
  var shipBox: string = frameName & ".x"
  let npcShipX: int = try:
      tclEval2(script = shipBox & " get").parseInt
    except:
      return showError(message = "Can't get X coord.")
  shipBox = frameName & ".y"
  let npcShipY: int = try:
      tclEval2(script = shipBox & " get").parseInt
    except:
      return showError(message = "Can't get Y coord.")
  shipBox = frameName & ".duration"
  let duration: int = try:
      tclEval2(script = shipBox & " get").parseInt
    except:
      return showError(message = "Can't get duration.")
  var friendlyShips: seq[Positive] = @[]
  try:
    generateFriendlyShips(ships = friendlyShips)
  except:
    return showError(message = "Can't generate friendly ships.")
  var traders: seq[Positive] = @[]
  generateTraders(ships = traders)
  for index, ship in protoShipsList:
    if ship.name == shipName:
      if index in traders:
        eventsList.add(y = EventData(skyX: npcShipX, skyY: npcShipY,
            time: duration, eType: trader, shipIndex: index))
      elif index in friendlyShips:
        eventsList.add(y = EventData(skyX: npcShipX, skyY: npcShipY,
            time: duration, eType: friendlyShip, shipIndex: index))
      else:
        eventsList.add(y = EventData(skyX: npcShipX, skyY: npcShipY,
            time: duration, eType: enemyShip, shipIndex: index))
      skyMap[npcShipX][npcShipY].eventIndex = eventsList.high
      return refreshCommand(clientData = clientData, interp = interp,
          argc = argc, argv = argv)
  return tclOk

proc toggleItemEntryCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], cdecl,
        contractual.} =
  ## Show or hide item entry for bases events
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ToggleItemEntry
  const
    frameName: string = ".debugdialog.main.world"
    eventCombo: string = frameName & ".event"
    itemEntry: string = frameName & ".item"
    itemLabel: string = frameName & ".itemlbl"
  if tclEval2(script = eventCombo & " current") == "1":
    tclEval(script = "grid " & itemLabel)
    tclEval(script = "grid " & itemEntry)
  else:
    tclEval(script = "grid remove " & itemLabel)
    tclEval(script = "grid remove " & itemEntry)
  return tclOk

proc debugAddEventCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Add a new base event to the game
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## DebugAddEvent
  const
    frameName: string = ".debugdialog.main.world"
    eventEntry: string = frameName & ".base"
  var
    eventName: string = tclEval2(script = eventEntry & " get")
    baseIndex: ExtendedBasesRange = 0
  for index, base in skyBases:
    if base.name == eventName:
      baseIndex = index
      break
  if baseIndex == 0:
    return tclOk
  var eventBox: string = frameName & ".event"
  let
    eventType: int = try:
        tclEval2(script = eventBox & " current").parseInt
      except:
        return showError(message = "Can't get event type.")
    durationBox: string = frameName & ".baseduration"
    duration: int = try:
        tclEval2(script = durationBox & " get").parseInt
      except:
        return showError(message = "Can't get event duration.")
  var added: bool = true
  case eventType
  of 0:
    eventsList.add(y = EventData(skyX: skyBases[baseIndex].skyX, skyY: skyBases[
        baseIndex].skyY, time: duration, eType: disease))
  of 1:
    eventBox = frameName & ".item"
    eventName = tclEval2(script = eventBox & " get")
    added = false
    for index, item in itemsList:
      if item.name == eventName:
        eventsList.add(y = EventData(skyX: skyBases[baseIndex].skyX,
            skyY: skyBases[baseIndex].skyY, time: duration, eType: doublePrice,
            itemIndex: index))
        added = true
  of 2:
    eventsList.add(y = EventData(skyX: skyBases[baseIndex].skyX, skyY: skyBases[
        baseIndex].skyY, time: duration, eType: fullDocks))
  else:
    discard
  if not added:
    return tclOk
  skyMap[skyBases[baseIndex].skyX][skyBases[
      baseIndex].skyY].eventIndex = eventsList.high
  return refreshCommand(clientData = clientData, interp = interp, argc = argc, argv = argv)

proc debugDeleteEventCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Remove the selected event from the game
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## DebugDeleteEvent
  const eventBox: string = ".debugdialog.main.world.deleteevent.delete"
  try:
    deleteEvent(eventIndex = tclEval2(script = eventBox & " current").parseInt)
  except:
    return showError(message = "Can't delete event.")
  return refreshCommand(clientData = clientData, interp = interp, argc = argc, argv = argv)

proc showDebugUi*() {.raises: [], tags: [WriteIOEffect, TimeEffect, RootEffect],
    contractual.} =
  ## Show debug ui to the player
  tclEval(script = """
      toplevel .debugdialog -class Dialog \
         -background [ttk::style lookup . -background] -relief solid -borderwidth 2
      wm title .debugdialog {Steam Sky - Debug menu}
      if {$tcl_platform(os) == "Linux"} {
         wm attributes .debugdialog -type dialog
      }
      grid [ttk::frame .debugdialog.buttons] -sticky n
      grid [ttk::frame .debugdialog.main] -column 1 -row 0 -sticky news
      proc ShowFrame {framename} {
         Refresh
         grid remove [grid slaves .debugdialog.main]
         grid $framename
      }
      grid [ttk::button .debugdialog.buttons.ship -text Ship \
         -command {ShowFrame .debugdialog.main.ship}]
      grid [ttk::button .debugdialog.buttons.crew -text Crew \
         -command {ShowFrame .debugdialog.main.crew}]
      grid [ttk::button .debugdialog.buttons.cargo -text Cargo \
         -command {ShowFrame .debugdialog.main.cargo}]
      grid [ttk::button .debugdialog.buttons.bases -text Bases \
         -command {ShowFrame .debugdialog.main.bases}]
      grid [ttk::button .debugdialog.buttons.world -text World \
         -command {ShowFrame .debugdialog.main.world}]
      grid [ttk::button .debugdialog.buttons.refresh -text Refresh \
         -command Refresh]
      grid [ttk::button .debugdialog.buttons.save -text {Save game} \
         -command DebugSaveGame]
      # Ship options
      set shipframe [ttk::frame .debugdialog.main.ship]
      grid [ttk::button $shipframe.move -text {Move ship} -command DebugMoveShip]
      grid [ttk::label $shipframe.lblx -text {X:}] -column 1 -row 0
      grid [ttk::spinbox $shipframe.x -from 1 -to 1024 -validate key \
         -validatecommand {ValidateSpinbox %W %P $shipframe.move} -width 5] -column 2 -row 0 \
         -sticky w
      grid [ttk::label $shipframe.lbly -text {Y:}] -column 3 -row 0 -sticky w
      grid [ttk::spinbox $shipframe.y -from 1 -to 1024 -validate key \
         -validatecommand {ValidateSpinbox %W %P $shipframe.move} -width 5] -column 4 -row 0 \
         -sticky w
      grid [ttk::label $shipframe.modulelbl -text {Module:}]
      grid [ttk::combobox $shipframe.module -state readonly] -column 1 -row 1 \
         -columnspan 4
      bind $shipframe.module <<ComboboxSelected>> RefreshModule
      grid [ttk::label $shipframe.protolbl -text {Prototype:}]
      grid [ttk::combobox $shipframe.proto -state readonly]  -column 1 -row 2 \
         -columnspan 4 -sticky w
      grid [ttk::label $shipframe.weightlbl -text {Weight:}]
      grid [ttk::spinbox $shipframe.weight -from 0 -to 100000 -validate key \
         -validatecommand {ValidateSpinbox %W %P $shipframe.change} -width 5] -column 1 -row 3 \
         -columnspan 4 -sticky w
      grid [ttk::label $shipframe.durlbl -text {Durability:}]
      grid [ttk::spinbox $shipframe.dur -from 0 -to 1000 -validate key \
         -validatecommand {ValidateSpinbox %W %P $shipframe.change} -width 5] -column 1 -row 4 \
         -columnspan 4 -sticky w
      grid [ttk::label $shipframe.maxdurlbl -text {Max durability:}]
      grid [ttk::spinbox $shipframe.maxdur -from 0 -to 1000 -validate key \
         -validatecommand {ValidateSpinbox %W %P $shipframe.change} -width 5] -column 1 -row 5 \
         -columnspan 4 -sticky w
      grid [ttk::label $shipframe.upgradelbl -text {Upgrade progress:}]
      grid [ttk::spinbox $shipframe.upgrade -from 0 -to 100000 -validate key \
         -validatecommand {ValidateSpinbox %W %P $shipframe.change} -width 5] -column 1 -row 6 \
         -columnspan 4 -sticky w
      grid [ttk::button $shipframe.change -text Change -command DebugUpdateModule] \
         -columnspan 5
      # Crew options
      set crewframe [ttk::frame .debugdialog.main.crew]
      grid [ttk::label $crewframe.memberlbl -text Member] -column 1
      grid [ttk::combobox $crewframe.member -state readonly -width 10] -column 2 \
         -row 0
      bind $crewframe.member <<ComboboxSelected>> RefreshMember
      grid [ttk::frame $crewframe.stats2] -columnspan 2
      grid [ttk::label $crewframe.stats2.healthlbl -text Health]
      grid [ttk::spinbox $crewframe.stats2.health -from 1 -to 100 -validate key \
         -validatecommand {ValidateSpinbox %W %P $crewframe.change} -width 5] -column 1 -row 0
      grid [ttk::label $crewframe.stats2.thirstlbl -text Thirst]
      grid [ttk::spinbox $crewframe.stats2.thirst -from 0 -to 100 -validate key \
         -validatecommand {ValidateSpinbox %W %P $crewframe.change} -width 5] -column 1 -row 1
      grid [ttk::label $crewframe.stats2.hungerlbl -text Hunger]
      grid [ttk::spinbox $crewframe.stats2.hunger -from 0 -to 100 -validate key \
         -validatecommand {ValidateSpinbox %W %P $crewframe.change} -width 5] -column 1 -row 2
      grid [ttk::label $crewframe.stats2.tiredlbl -text Tired]
      grid [ttk::spinbox $crewframe.stats2.tired -from 0 -to 100 -validate key \
         -validatecommand {ValidateSpinbox %W %P $crewframe.change} -width 5] -column 1 -row 3
      grid [ttk::label $crewframe.stats2.moralelbl -text Morale]
      grid [ttk::spinbox $crewframe.stats2.morale -from 0 -to 100 -validate key \
         -validatecommand {ValidateSpinbox %W %P $crewframe.change} -width 5] -column 1 -row 4
      grid [ttk::label $crewframe.stats2.loyaltylbl -text Loyalty]
      grid [ttk::spinbox $crewframe.stats2.loyalty -from 0 -to 100 -validate key \
         -validatecommand {ValidateSpinbox %W %P $crewframe.change} -width 5] -column 1 -row 5
      grid [ttk::frame $crewframe.stats] -column 2 -row 1 -sticky n
      grid [ttk::label $crewframe.stats.name -text Name]
      grid [ttk::label $crewframe.stats.level -text Level] -column 1 -row 0
      grid [ttk::frame $crewframe.skills] -column 3 -row 1 -sticky n
      grid [ttk::label $crewframe.skills.name -text Name]
      grid [ttk::label $crewframe.skills.level -text Level] -column 1 -row 0
      grid [ttk::button $crewframe.change -text Change -command DebugUpdateMember] \
         -columnspan 2
      grid [ttk::frame $crewframe.addskill] -column 2 -row 2 -columnspan 2
      grid [ttk::button $crewframe.addskill.add -text {Add skill}\
         -command DebugAddSkill]
      grid [ttk::combobox $crewframe.addskill.skills -state readonly -width 15] \
         -column 1 -row 0
      # Cargo options
      set cargoframe [ttk::frame .debugdialog.main.cargo]
      grid [ttk::button $cargoframe.addbutton -text Add -command DebugAddItem]
      grid [ttk::combobox $cargoframe.add -width 15] -column 1 -row 0
      grid [ttk::label $cargoframe.amountlbl -text {Amount:}]
      grid [ttk::spinbox $cargoframe.amount -from 1 -to 1000000 -validate key \
         -validatecommand {ValidateSpinbox %W %P $cargoframe.addbutton} -width 15] -column 1 -row 1
      grid [ttk::label $cargoframe.qualitylbl -text {Quality:}]
      grid [ttk::combobox $cargoframe.addquality -width 15 \
        -values [list Poor Low Normal Good Excellent] -state readonly] -column 1 \
        -row 2
      grid [ttk::button $cargoframe.updatebutton -text Update \
         -command DebugUpdateItem] -pady {50 0}
      grid [ttk::combobox $cargoframe.update -state readonly -width 15] -column 1 \
         -row 3 -pady {50 0}
      grid [ttk::label $cargoframe.amount2lbl -text {Amount:}]
      grid [ttk::spinbox $cargoframe.updateamount -from 1 -to 1000000 -validate key \
         -validatecommand {ValidateSpinbox %W %P $cargoframe.updatebutton} -width 15] -column 1 -row 4
      grid [ttk::label $cargoframe.qualitylbl2 -text {Quality:}]
      grid [ttk::combobox $cargoframe.updatequality -width 15 \
        -values [list Poor Low Normal Good Excellent] -state readonly] -column 1 \
        -row 5
      bind $cargoframe.update <<ComboboxSelected>> RefreshCargo
      # Bases options
      set basesframe [ttk::frame .debugdialog.main.bases]
      grid [ttk::label $basesframe.lbl1 -text {Base:}]
      grid [ttk::combobox $basesframe.name -width 15] -column 1 -row 0
      bind $basesframe.name <<ComboboxSelected>> RefreshBase
      bind $basesframe.name <Return> RefreshBase
      grid [ttk::label $basesframe.lbl2 -text {Type:}]
      grid [ttk::combobox $basesframe.type -state readonly -width 15] -column 1 -row 1
      grid [ttk::label $basesframe.lbl3 -text {Owner:}]
      grid [ttk::combobox $basesframe.owner -state readonly -width 15] -column 1 -row 2
      grid [ttk::label $basesframe.lbl4 -text {Size:}]
      grid [ttk::combobox $basesframe.size -state readonly \
         -values [list Small Medium Big] -width 15] -column 1 -row 3
      grid [ttk::label $basesframe.lbl5 -text {Population:}]
      grid [ttk::spinbox $basesframe.population -from 0 -to 10000 -validate key \
         -validatecommand {ValidateSpinbox %W %P $basesframe.update} -width 15] -column 1 -row 4
      grid [ttk::label $basesframe.lbl6 -text {Reputation:}]
      grid [ttk::spinbox $basesframe.reputation -from -100 -to 100 -validate key \
         -validatecommand {ValidateSpinbox %W %P $basesframe.update} -width 15] -column 1 -row 5
      grid [ttk::label $basesframe.lbl7 -text {Money:}]
      grid [ttk::spinbox $basesframe.money -from 1 -to 1000000 -validate key \
         -validatecommand {ValidateSpinbox %W %P $basesframe.update} -width 15] -column 1 -row 6
      grid [ttk::button $basesframe.update -text {Update} -command DebugUpdateBase] \
         -columnspan 2
      # World options
      set worldframe [ttk::frame .debugdialog.main.world]
      grid [ttk::label $worldframe.shiplbl -text {Ship:}]
      grid [ttk::combobox $worldframe.ship -width 20] -column 1 -row 0
      grid [ttk::label $worldframe.xlbl -text {X:}]
      grid [ttk::spinbox $worldframe.x -from 1 -to 1024 -validate key \
         -validatecommand {ValidateSpinbox %W %P $worldframe.addship} -width 20] -column 1 -row 1
      $worldframe.x set 1
      grid [ttk::label $worldframe.ylbl -text {Y:}]
      grid [ttk::spinbox $worldframe.y -from 1 -to 1024 -validate key \
         -validatecommand {ValidateSpinbox %W %P $worldframe.addship} -width 20] -column 1 -row 2
      $worldframe.y set 1
      grid [ttk::label $worldframe.durationlbl -text {Duration:}]
      grid [ttk::spinbox $worldframe.duration -from 60 -to 1000 -validate key \
         -validatecommand {ValidateSpinbox %W %P $worldframe.addship} -width 20] -column 1 -row 3
      $worldframe.duration set 60
      grid [ttk::button $worldframe.addship -text {Add ship} -command DebugAddShip] \
         -columnspan 2
      grid [ttk::label $worldframe.baselbl -text {Base:}] -column 2 -row 0
      grid [ttk::combobox $worldframe.base -width 15] -column 3 -row 0
      grid [ttk::label $worldframe.eventlbl -text {Event:}] -column 2 -row 1
      grid [ttk::combobox $worldframe.event -state readonly \
         -values [list Disease {Double price} {Full docks}] -width 15] -column 3 \
         -row 1
      bind $worldframe.event <<ComboboxSelected>> ToggleItemEntry
      grid [ttk::label $worldframe.itemlbl -text {Item:}] -column 2 -row 2
      grid [ttk::combobox $worldframe.item -width 15] -column 3 -row 2
      grid [ttk::label $worldframe.duration2lbl -text {Duration:}] -column 2 -row 3
      grid [ttk::spinbox $worldframe.baseduration -from 15 -to 12000 -validate key \
         -validatecommand {ValidateSpinbox %W %P $worldframe.addevent} -width 15] -column 3 -row 3
      grid [ttk::button $worldframe.addevent -text {Add event} \
         -command DebugAddEvent] -column 2 -row 4 -columnspan 2
      set deleteeventframe [ttk::frame $worldframe.deleteevent]
      grid [ttk::button $deleteeventframe.deleteevent -text {Delete event} \
         -command DebugDeleteEvent]
      grid [ttk::combobox $deleteeventframe.delete -state readonly -width 30] -column 1 -row 0
      grid $deleteeventframe -columnspan 4 -pady {50 0}
      grid $shipframe
      wm geometry .debugdialog +[expr ([winfo vrootwidth .debugdialog] / 2) \
         - 200]+[expr [winfo vrootheight .debugdialog] / 3]
    """)
  try:
    addCommand(name = "Refresh", nimProc = refreshCommand)
    addCommand(name = "RefreshModule", nimProc = refreshModuleCommand)
    addCommand(name = "RefreshMember", nimProc = refreshMemberCommand)
    addCommand(name = "RefreshCargo", nimProc = refreshCargoCommand)
    addCommand(name = "RefreshEvents", nimProc = refreshEventsCommand)
    addCommand(name = "RefreshBase", nimProc = refreshBaseCommand)
    addCommand(name = "DebugSaveGame", nimProc = debugSaveGameCommand)
    addCommand(name = "DebugMoveShip", nimProc = debugMoveShipCommand)
    addCommand(name = "DebugUpdateModule", nimProc = debugUpdateModuleCommand)
    addCommand(name = "DebugAddSkill", nimProc = debugAddSkillCommand)
    addCommand(name = "DebugUpdateMember", nimProc = debugUpdateMemberCommand)
    addCommand(name = "DebugAddItem", nimProc = debugAddItemCommand)
    addCommand(name = "DebugUpdateItem", nimProc = debugUpdateItemCommand)
    addCommand(name = "DebugUpdateBase", nimProc = debugUpdateBaseCommand)
    addCommand(name = "DebugAddShip", nimProc = debugAddShipCommand)
    addCommand(name = "ToggleItemEntry", nimProc = toggleItemEntryCommand)
    addCommand(name = "DebugAddEvent", nimProc = debugAddEventCommand)
    addCommand(name = "DebugDeleteEvent", nimProc = debugDeleteEventCommand)
  except:
    showError(message = "Can't add a Tcl command.")
  var valuesList: string = ""
  for baseType in basesTypesList.values:
    valuesList.add(y = " {" & baseType.name & "}")
  const frameName: string = ".debugdialog.main.bases"
  var comboBox: string = frameName & ".type"
  tclEval(script = comboBox & " configure -values [list" & valuesList & "]")
  valuesList = ""
  comboBox = frameName & ".owner"
  for faction in factionsList.values:
    valuesList.add(y = " {" & faction.name & "}")
  tclEval(script = comboBox & " configure -values [list" & valuesList & "]")
  valuesList = ""
  comboBox = frameName & ".name"
  for base in skyBases:
    valuesList.add(y = " {" & base.name & "}")
  tclEval(script = comboBox & " configure -values [list" & valuesList & "]")
  comboBox = ".debugdialog.main.world.base"
  tclEval(script = comboBox & " configure -values [list" & valuesList & "]")
  valuesList = ""
  comboBox = ".debugdialog.main.ship.proto"
  for module in modulesList.values:
    if module.name.len > 0:
      valuesList.add(y = " {" & module.name & "}")
  tclEval(script = comboBox & " configure -values [list" & valuesList & "]")
  valuesList = ""
  comboBox = ".debugdialog.main.cargo.add"
  for item in itemsList.values:
    valuesList.add(y = " {" & item.name & "}")
  tclEval(script = comboBox & " configure -values [list" & valuesList & "]")
  comboBox = ".debugdialog.main.world.item"
  tclEval(script = comboBox & " configure -values [list" & valuesList & "]")
  valuesList = ""
  comboBox = ".debugdialog.main.world.ship"
  for index, ship in protoShipsList:
    valuesList.add(y = " {" & ship.name & "}")
  tclEval(script = comboBox & " configure -values [list" & valuesList & "]")
  tclEval(script = "Refresh")
