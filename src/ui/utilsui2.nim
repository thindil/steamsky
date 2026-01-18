# Copyright 2023-2025 Bartek thindil Jasicki
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

## Provides various procedures related to the game's UI, like showing a screen,
## updating the messages list, convert amount of minutes to date etc.  Split
## from the utilsui module to avoid circular dependencies.

import std/[strutils, tables]
import contracts
import ../[config, crew, game, items, messages, shipscrew, shipsmovement, tk, types]
import coreui, dialogs, errordialog

type
  TravelArray* = array[1..2, Natural]
    ## Used to store data about travel: first is amount of minutes needed to
    ## reach, the second is amount of fuel needed.

proc showScreen*(newScreenName: string) {.raises: [], tags: [], contractual.} =
  ## Clear the old screen and show the selected to the player
  ##
  ## * newScreenName - the Tcl name of the screen which will be show
  if tclGetVar(varName = "mappreview") == "1" and newScreenName != "mapframe":
    tclUnsetVar(varName = "mappreview")
  const
    paned: string = mainPaned & ".controls.buttons"
    messagesFrame: string = mainPaned & ".controls.messages"
  let interp: PInterp = getInterp()
  if tclEval(script = mainPaned & " panes") == tclError:
    return
  let
    tclResult: string = $interp.tclGetResult()
    oldSubWindow: string = tclResult.split()[0]
    subWindow: string = mainPaned & "." & $newScreenName
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
      let newPos: string = $interp.tclGetResult()
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

proc updateMessages*() {.raises: [], tags: [], contractual.} =
  ## Update the list of in-game messages, delete old ones and show the
  ## newest to the player
  let messagesView: string = mainPaned & ".controls.messages.view"
  tclEval(script = messagesView & " configure -state normal")
  tclEval(script = messagesView & " delete 1.0 end")
  var loopStart: int = 0 - messagesAmount()
  if loopStart == 0:
    return
  if loopStart < -10:
    loopStart = -10

  proc showMessage(message: MessageData) {.raises: [], tags: [], contractual.} =
    ## Show the selected message
    ##
    ## * message - the message to show
    const tagNames: array[1..5, string] = ["yellow", "green", "red", "blue", "cyan"]
    if message.color == white:
      tclEval(script = messagesView & " insert end {" & message.message & "}")
    else:
      tclEval(script = messagesView & " insert end {" & message.message &
          "} [list " & tagNames[message.color.ord] & "]")

  if gameSettings.messagesOrder == olderFirst:
    for i in loopStart .. -1:
      showMessage(message = getMessage(messageIndex = i + 1))
      if i < -1:
        tclEval(script = messagesView & " insert end {\n}")
    tclEval(script = "update")
    tclEval(script = messagesView & " see end")
  else:
    for i in countdown(a = -1, b = loopStart):
      showMessage(message = getMessage(messageIndex = i + 1))
      if i > loopStart:
        tclEval(script = messagesView & " insert end {\n}")
  tclEval(script = messagesView & " configure -state disable")

proc getSkillMarks*(skillIndex: Positive;
    memberIndex: Natural): string {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], contractual.} =
  ## Get the marks with information about the skill level for the selected
  ## skill for the selected crew member
  ##
  ## * skillIndex  - the index of the skill to check
  ## * memberIndex - the index of the player's ship's crew member to check
  ##
  ## The string with one "+" sign if the crew member known the skill, the
  ## string with twi "+" sings if the crew member has the highest level in
  ## the skill of the all crew members. Otherwise return an empty string.
  var
    skillValue: int = 0
    crewIndex: int = -1
  try:
    for index, member in playerShip.crew:
      if getSkillLevel(member = member, skillIndex = skillIndex) > skillValue:
        skillValue = getSkillLevel(member = member, skillIndex = skillIndex)
        crewIndex = index
    if getSkillLevel(member = playerShip.crew[memberIndex],
        skillIndex = skillIndex) > 0:
      result = " +"
  except:
    showError(message = "Can't get the crew member skill level.")
    return ""
  if memberIndex == crewIndex:
    result &= "+"

proc travelInfo*(distance: Natural): TravelArray {.raises: [], tags: [
    WriteIOEffect, TimeEffect, RootEffect], contractual.} =
  ## Count the ETA and the fuel usage for the selected distance
  ##
  ## * Distance - Distance in map fields to destination point
  ##
  ## The result is the array with two values, the first is estimated time to
  ## travel the distance, the second is the amount of fuel needed to travel
  ## the distance.
  result = [0, 0]
  if distance == 0:
    return
  let speed: float = try:
      realSpeed(ship = playerShip, infoOnly = true) / 1_000
    except:
      showError(message = "Can't count the player's ship speed.")
      return
  if speed == 0.0:
    return
  var minutesDiff: int = (100.0 / speed).int
  case playerShip.speed
  of quarterSpeed:
    if minutesDiff < 60:
      minutesDiff = 60
  of halfSpeed:
    if minutesDiff < 30:
      minutesDiff = 30
  of fullSpeed:
    if minutesDiff < 15:
      minutesDiff = 15
  else:
    discard
  minutesDiff *= distance
  var rests, restTime: int = 0
  for index, member in playerShip.crew:
    if member.order notin {pilot, engineer}:
      continue
    let tired: int = (minutesDiff / 15).int + member.tired
    if (tired / (80 + member.attributes[conditionIndex].level)).int > rests:
      rests = (tired / (80 + member.attributes[conditionIndex].level)).int
    if rests > 0:
      let cabinIndex: int = findCabin(memberIndex = index)
      var tempTime: int = 0
      if cabinIndex > -1:
        let
          damage: float = 1.0 - (playerShip.modules[cabinIndex].durability.float /
              playerShip.modules[cabinIndex].maxDurability.float)
        var cabinBonus: int = playerShip.modules[cabinIndex].cleanliness - (
            playerShip.modules[cabinIndex].cleanliness.float * damage).int
        if cabinBonus == 0:
          cabinBonus = 1
        tempTime = ((80.0 + member.attributes[conditionIndex].level.float) /
            cabinBonus.float).int * 15
        if tempTime == 0:
          tempTime = 15
      else:
        tempTime = (80 + member.attributes[conditionIndex].level) * 15
      tempTime += 15
      if tempTime > restTime:
        restTime = tempTime
  result[1] = minutesDiff + (rests * restTime)
  result[2] = abs(x = distance * countFuelNeeded()) + (rests * (restTime / 10).int)

proc minutesToDate*(minutes: int; infoText: var string) {.raises: [
    ], tags: [], contractual.} =
  ## Convert the game minutes to the game time in days, hours, etc
  ##
  ## * minutes  - the amount of minutes to convert
  ## * infoText - the string to which the converted time will be added
  ##
  ## Returns the updated infoText paramater with converted minutes to the game
  ## time
  var
    travelTime: DateRecord = DateRecord()
    minutesDiff: int = minutes
  while minutesDiff > 0:
    case minutesDiff:
    of 518_401..int.high:
      minutesDiff -= 518_400
      travelTime.year.inc()
    of 43_201..518_400:
      minutesDiff -= 43_200
      travelTime.month.inc()
      if travelTime.month > 12:
        travelTime.year.inc()
        travelTime.month = 1
    of 1_441..43_200:
      minutesDiff -= 1_440
      travelTime.day.inc()
      if travelTime.day > 31:
        travelTime.month.inc()
        if travelTime.month > 12:
          travelTime.year.inc()
          travelTime.month = 1
    of 61..1_440:
      minutesDiff -= 60
      travelTime.hour.inc()
      if travelTime.hour > 23:
        travelTime.hour = 0
        travelTime.day.inc()
        if travelTime.day > 31:
          travelTime.day = 1
          travelTime.month.inc()
          if travelTime.month > 12:
            travelTime.month = 1
            travelTime.year.inc()
    else:
      travelTime.minutes = minutesDiff
      minutesDiff = 0
    if travelTime.year == 4_000_000:
      break
  if travelTime.year > 0:
    infoText = infoText & " " & $travelTime.year & "y"
  if travelTime.month > 0:
    infoText = infoText & " " & $travelTime.month & "m"
  if travelTime.day > 0:
    infoText = infoText & " " & $travelTime.day & "d"
  if travelTime.hour > 0:
    infoText = infoText & " " & $travelTime.hour & "h"
  if travelTime.minutes > 0:
    infoText = infoText & " " & $travelTime.minutes & "mins"

proc showInventoryItemInfo*(parent: string; itemIndex: Natural;
    memberIndex: int; button1: ButtonSettings = emptyButtonSettings;
    button2: ButtonSettings = emptyButtonSettings) {.raises: [
    KeyError], tags: [WriteIOEffect, TimeEffect, RootEffect], contractual.} =
  ## Show info about selected item in ship cargo or crew member inventory
  ##
  ## * parent      - The name of the parent widget
  ## * itemIndex   - Index of item (can be inventory or ship cargo)
  ## * memberIndex - If item is in crew member inventory, crew index of member,
  ##                 otherwise 0
  ## * button1     - The settings for the first optional button. If empty, the
  ##                 button will not show. Default value is empty.
  ## * button2     - The setting for the second optional button. If empty,
  ##                 the button will not show. Default value is empty.
  var
    protoIndex: Natural = 0
    itemInfo, quality, maxDurability, weight: string = ""
  if memberIndex > -1:
    let item: InventoryData = playerShip.crew[memberIndex].inventory[itemIndex]
    protoIndex = item.protoIndex
    quality = $item.quality
    if item.maxDurability != defaultItemDurability:
      if gameSettings.showNumbers:
        maxDurability = $item.maxDurability
      else:
        maxDurability = (if item.maxDurability < defaultItemDurability: "Less"
          else: "More") & " durable"
    if item.weight != 0:
      weight = $item.weight
    if item.durability < item.maxDurability:
      itemInfo = getItemDamage(itemDurability = item.durability,
        withColors = true) & '\n'
  else:
    let item: InventoryData = playerShip.cargo[itemIndex]
    protoIndex = item.protoIndex
    quality = $item.quality
    if item.maxDurability != defaultItemDurability:
      if gameSettings.showNumbers:
        maxDurability = $item.maxDurability
      else:
        maxDurability = (if item.maxDurability < defaultItemDurability: "Less"
          else: "More") & " durable"
    if item.weight != 0:
      weight = $item.weight
    if item.durability < item.maxDurability:
      itemInfo = getItemDamage(itemDurability = item.durability,
        withColors = true) & '\n'
  itemInfo.add(y = "Weight: {gold}" & (if weight.len > 0: weight
      else: $itemsList[protoIndex].weight) & " kg{/gold}")
  if itemsList[protoIndex].itemType == weaponType:
    itemInfo.add(y = "\nSkill: {gold}" & skillsList[itemsList[protoIndex].value[
        3]].name & "/" & attributesList[skillsList[itemsList[protoIndex].value[
        3]].attribute].name & "{/gold}")
    if itemsList[protoIndex].value[4] == 1:
      itemInfo.add(y = "\n{gold}Can be used with shield.{/gold}")
    else:
      itemInfo.add(y = "\n{gold}Can't be used with shield (two-handed weapon).{/gold}")
    itemInfo.add(y = "\nDamage type: {gold}")
    itemInfo.add(y = case itemsList[protoIndex].value[5]
      of 1:
        "cutting"
      of 2:
        "impaling"
      of 3:
        "blunt"
      else:
        "")
    itemInfo.add(y = "{/gold}")
  let itemTypes: array[6, string] = [weaponType, chestArmor, headArmor, armsArmor, legsArmor, shieldType]
  for itemType in itemTypes:
    if itemsList[protoIndex].itemType == itemType:
      itemInfo.add(y = "\nDamage chance: {gold}" & getItemChanceToDamage(
          itemData = itemsList[protoIndex].value[1]) &
          "\n{/gold}Strength: {gold}" & $itemsList[protoIndex].value[2] & "{/gold}")
      break
  if itemsList[protoIndex].itemType in toolsList:
    itemInfo.add(y = "\nDamage chance: {gold}" & getItemChanceToDamage(
        itemData = itemsList[protoIndex].value[1]) & "{/gold}")
  if itemsList[protoIndex].itemType.len > 4 and itemsList[protoIndex].itemType[
      0 .. 3] == "Ammo" or itemsList[protoIndex].itemType == "Harpoon":
    itemInfo.add(y = "\nStrength: {gold}" & $itemsList[protoIndex].value[1] & "{/gold}")
  if protoIndex != moneyIndex:
    itemInfo.add(y = "\nQuality: {gold}" & quality.capitalizeAscii & "{/gold}")
  if maxDurability.len > 0:
    itemInfo.add(y = "\nMax durability: {gold}" & maxDurability & "{/gold}")
  if itemsList[protoIndex].description.len > 0:
    itemInfo.add(y = "\n\n" & itemsList[protoIndex].description)
  if parent == ".":
    showInfo(text = itemInfo, title = (if memberIndex > -1: getItemName(
        item = playerShip.crew[memberIndex].inventory[itemIndex],
        damageInfo = false, toLower = false) else: getItemName(
        item = playerShip.cargo[itemIndex], damageInfo = false,
        toLower = false)), button1 = button1, button2 = button2)
  else:
    showInfo(text = itemInfo, parentName = parent, title = (if memberIndex >
        -1: getItemName(item = playerShip.crew[memberIndex].inventory[
        itemIndex], damageInfo = false, toLower = false) else: getItemName(
        item = playerShip.cargo[itemIndex], damageInfo = false,
        toLower = false)), button1 = button1, button2 = button2)

proc setFonts*(newSize: Positive; fontType: FontTypes) {.raises: [],
    tags: [], contractual.} =
  ##  Set all the game fonts to the selected size
  ##
  ##  newSize  - The new size of the selected font's type
  ##  fontType - The type of the font
  const
    helpFonts: array[4, string] = ["HelpFont", "BoldHelpFont",
        "UnderlineHelpFont", "ItalicHelpFont"]
    interfaceFonts: array[3, string] = ["InterfaceFont", "OverstrikedFont", "UnderlineFont"]
  case fontType
  of mapFont:
    gameSettings.mapFontSize = newSize
    tclEval(script = "font configure MapFont -size " &
        $gameSettings.mapFontSize)
  of helpFont:
    gameSettings.helpFontSize = newSize
    for fontName in helpFonts:
      tclEval(script = "font configure " & fontName & " -size " &
          $gameSettings.helpFontSize)
  of interfaceFont:
    gameSettings.interfaceFontSize = newSize
    for fontName in interfaceFonts:
      tclEval(script = "font configure " & fontName & " -size " &
          $gameSettings.interfaceFontSize)
