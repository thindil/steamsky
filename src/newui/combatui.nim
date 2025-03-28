# Copyright 2025 Bartek thindil Jasicki
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
# along with Steam Sky.  if, see <http://www.gnu.org/licenses/>.

## Provides code related to all types of combat, like between the ships,
## boarding, giving orders to crew members, etc.

import std/[math, strbasics, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[combat, config, crewinventory, game, maps, shipscrew, types]
import coreui, dialogs, errordialog, header, themes, utilsui2

const
  pilotOrders: array[4, string] = ["Go closer", "Keep distance", "Evade", "Escape"]
  engineerOrders: array[4, string] = ["All stop", "Quarter speed", "Half speed", "Full speed"]
  gunnersOrders: array[1..6, string] = ["Don't shoot", "Precise fire",
      "Fire at will", "Aim for their engine", "Aim for their weapon", "Aim for their hull"]

var
  pilotList, engineerList, gunnerList: seq[string] = @["Nobody"]
  pilotIndex, engineerIndex: Natural = 0
  expandedSection: Natural = 0
  gunnersIndex: seq[Natural] = @[]

proc updateCrewLists() {.raises: [], tags: [RootEffect], contractual.} =
  ## Update the list of available crew members for all positions in combat
  pilotList = @["Nobody"]
  engineerList = @["Nobody"]
  for index, member in playerShip.crew:
    if member.skills.len > 0:
      pilotList.add(y = member.name & getSkillMarks(skillIndex = pilotingSkill,
          memberIndex = index))
      engineerList.add(y = member.name & getSkillMarks(
          skillIndex = engineeringSkill, memberIndex = index))
      gunnerList.add(y = member.name & getSkillMarks(skillIndex = gunnerySkill,
          memberIndex = index))

proc setCombat*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Set the combat UI and combat itself
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  try:
    if skyMap[playerShip.skyX][playerShip.skyY].eventIndex > -1 and
        enemyName != protoShipsList[eventsList[skyMap[playerShip.skyX][
        playerShip.skyY].eventIndex].shipIndex].name:
      let combatStarted = startCombat(enemyIndex = eventsList[skyMap[
          playerShip.skyX][playerShip.skyY].eventIndex].shipIndex,
          newCombat = false)
      if not combatStarted:
        return
  except:
    dialog = setError(message = "Can't start the combat.")
    return
  state = combat
  dialog = none
  engineerOrder = 3
  pilotIndex = findMember(order = pilot) + 1
  engineerIndex = findMember(order = engineer) + 1
  gunnersIndex = @[]
  for gun in guns:
    gunnersIndex.add(y = playerShip.modules[gun[1]].owner[0] + 1)
  updateCrewLists()

proc getGunSpeed(position: Natural; index: Positive): string {.raises: [
    KeyError], tags: [], contractual.} =
  ## Get the information about the fire rate of the selected gun
  ##
  ## * position - the index of the gun on the guns list
  ## * index    - the index of the order for the gun
  ##
  ## Returns the string with information about the gun's speed.
  result = ""
  var gunSpeed: int = modulesList[playerShip.modules[guns[position][
      1]].protoIndex].speed
  case index
  of 1:
    gunSpeed = 0
  of 3:
    discard
  else:
    gunSpeed = (if gunSpeed > 0: (gunSpeed.float /
        2.0).ceil.int else: gunSpeed - 1)
  if gunSpeed > 0:
    return "(" & $gunSpeed & "/round)"
  elif gunSpeed < 0:
    return "(1/" & $gunSpeed & " rounds)"

proc showPartyMenu(dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Assign the player's ship's crew members to a boarding party or defenders
  ##
  ## * dialog   - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog
  if dialog notin {boardingDialog, defendingDialog}:
    return
  try:
    const
      width: float = 400
      height: float = 400

    updateDialog(width = width, height = height)
    popup(pType = staticPopup, title = "Assign crew members to " &
      (if dialog == boardingDialog: "boarding party" else: "defenders"),
      x = dialogX, y = dialogY, w = width, h = height,
      flags = {windowBorder, windowTitle}):
      setLayoutRowStatic(height = 35, cols = 2, width = 35)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Select all crew members")
      imageButton(image = images[selectAllIcon]):
        discard
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Unselect all crew members")
      imageButton(image = images[unselectAllIcon]):
        discard
      setLayoutRowDynamic(height = 30, cols = 1)
      var party: seq[bool] = @[]
      for member in playerShip.crew:
        if dialog == boardingDialog:
          party.add(y = member.order == boarding)
        else:
          party.add(y = member.order == defend)
      labelButton(title = "Close"):
        closePopup()
        dialog = none
  except:
    dialog = setError(message = "Can't show the party dialog")

proc showCombat*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the combat UI
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  state = combat
  showHeader(dialog = dialog)
  # Draw dialogs
  showQuestion(dialog = dialog, state = state)
  showMessage(dialog = dialog)
  showInfo(dialog = dialog)
  showPartyMenu(dialog = dialog)
  # Draw UI
  if pilotList.len != playerShip.crew.len + 1:
    updateCrewLists()
  let height: float = (windowHeight - 35 - gameSettings.messagesPosition.float)
  if expandedSection == 0:
    setLayoutRowDynamic(height = height / 2, cols = 2)
  else:
    setLayoutRowDynamic(height = height, cols = 1)
  # The player's ship's crew orders
  if expandedSection in {0, 1}:
    group(title = "Your ship crew orders:", flags = {windowBorder, windowTitle}):
      setLayoutRowStatic(height = 35, cols = 1, width = 35)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Maximize/minimize the ship crew orders")
      imageButton(image = (if expandedSection == 0: images[expandIcon] else: images[contractIcon])):
        if expandedSection == 1:
          expandedSection = 0
        else:
          expandedSection = 1
      setLayoutRowDynamic(height = 35, cols = 3)
      label(str = "Position", alignment = centered)
      label(str = "Member", alignment = centered)
      label(str = "Order", alignment = centered)
      # Show pilot settings
      if pilotIndex == 0:
        setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.33.cfloat, 0.33])
      else:
        setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.33.cfloat, 0.33, 0.33])
      label(str = "Pilot:", alignment = left)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Select the crew member which will be the pilot during the combat. The sign + after name means that this crew member has piloting skill, the sign ++ after name means that his/her piloting skill is the best in the crew")
      let newPilot = comboList(items = pilotList,
          selected = pilotIndex, itemHeight = 25, x = 200, y = 150)
      if pilotIndex > 0:
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(),
              text = "Select the order for the pilot")
        let newOrder = comboList(items = pilotOrders,
            selected = (pilotOrder - 1), itemHeight = 25, x = 200, y = 150)
        if newOrder != pilotOrder - 1:
          pilotOrder = newOrder + 1
      if newPilot != pilotIndex:
        if newPilot > 0:
          try:
            giveOrders(ship = playerShip, memberIndex = newPilot - 1,
                givenOrder = pilot)
          except:
            dialog = setError(message = "Can't give order to the pilot.")
        else:
          try:
            giveOrders(ship = playerShip, memberIndex = pilotIndex - 1,
                givenOrder = rest)
          except:
            dialog = setError(message = "Can't give rest order to the pilot.")
        pilotIndex = findMember(order = pilot) + 1
        engineerIndex = findMember(order = engineer) + 1
      # Show engineer settings
      if engineerIndex == 0:
        setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.33.cfloat, 0.33])
      else:
        setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.33.cfloat, 0.33, 0.33])
      label(str = "Engineer:", alignment = left)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Select the crew member which will be the engineer during the combat. The sign + after name means that this crew member has engineering skill, the sign ++ after name means that his/her engineering skill is the best in the crew")
      let newEngineer = comboList(items = engineerList,
          selected = engineerIndex, itemHeight = 25, x = 200, y = 150)
      if engineerIndex > 0:
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(),
              text = "Select the order for the engineer")
        let newOrder = comboList(items = engineerOrders,
            selected = (engineerOrder - 1), itemHeight = 25, x = 200, y = 150)
        if newOrder != engineerOrder - 1:
          engineerOrder = newOrder + 1
      if newEngineer != engineerIndex:
        if newEngineer > 0:
          try:
            giveOrders(ship = playerShip, memberIndex = newEngineer - 1,
                givenOrder = engineer)
          except:
            dialog = setError(message = "Can't give order to the engineer.")
        else:
          try:
            giveOrders(ship = playerShip, memberIndex = engineerIndex - 1,
                givenOrder = rest)
          except:
            dialog = setError(message = "Can't give rest order to the engineer.")
        engineerIndex = findMember(order = engineer) + 1
        pilotIndex = findMember(order = pilot) + 1
      # Show the guns settings
      for gunIndex, gun in guns.mpairs:
        var hasGunner = playerShip.modules[gun[1]].owner[0] > 0
        if hasGunner:
          setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.33.cfloat, 0.33, 0.33])
        else:
          setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.33.cfloat, 0.33])
        var
          haveAmmo: bool = false
          ammoAmount: Natural = 0
        let aIndex: int = (if playerShip.modules[gun[1]].mType ==
            ModuleType2.gun: playerShip.modules[gun[
            1]].ammoIndex else: playerShip.modules[gun[1]].harpoonIndex)
        try:
          if aIndex in playerShip.cargo.low .. playerShip.cargo.high and
              itemsList[playerShip.cargo[aIndex].protoIndex].itemType ==
                  itemsTypesList[modulesList[
              playerShip.modules[gun[1]].protoIndex].value]:
            ammoAmount = playerShip.cargo[aIndex].amount
            haveAmmo = true
        except:
          dialog = setError(message = "Can't show the player's ship's gun settings. No proto item with index: " &
              $playerShip.cargo[aIndex].protoIndex, e = nil)
          return
        if not haveAmmo:
          ammoAmount = 0
          for itemIndex, item in itemsList:
            try:
              if item.itemType == itemsTypesList[modulesList[playerShip.modules[
                  gun[1]].protoIndex].value - 1]:
                let ammoIndex: int = findItem(inventory = playerShip.cargo,
                    protoIndex = itemIndex)
                if ammoIndex > -1:
                  ammoAmount = ammoAmount + playerShip.cargo[ammoIndex].amount
            except:
              dialog = setError(message = "Can't show the gun's ammo information. No proto module with index: " &
                  $playerShip.modules[gun[1]].protoIndex, e = nil)
              return
        wrapLabel(str = playerShip.modules[gun[1]].name & ": (Ammo: " &
            $ammoAmount & ")")
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(),
              text = "Select the crew member which will be operating the gun during the combat. The sign + after name means that this crew member has gunnery skill, the sign ++ after name means that his/her gunnery skill is the best in the crew")
        let newGunner = comboList(items = gunnerList,
            selected = gunnersIndex[gunIndex], itemHeight = 25, x = 200, y = 150)
        hasGunner = newGunner > 0
        if hasGunner:
          var gunnerOrders: array[1..6, string] = gunnersOrders
          for orderIndex, order in gunnersOrders:
            try:
              gunnerOrders[orderIndex] = order & getGunSpeed(
                  position = gunIndex, index = orderIndex)
            except:
              dialog = setError(message = "Can't show gunner's order.")
              return
          if gameSettings.showTooltips:
            addTooltip(bounds = getWidgetBounds(),
                text = "Select the order for the gunner. Shooting in the selected part of enemy ship is less precise but always hit the selected part.")
          let newOrder = comboList(items = gunnerOrders,
              selected = (gun[2] - 1), itemHeight = 25, x = 200, y = 150)
          if newOrder != gun[2] - 1:
            gun[2] = newOrder + 1
        if newGunner != gunnersIndex[gunIndex]:
          gunnersIndex[gunIndex] = newGunner
      # Show boarding/defending settings
      try:
        if (harpoonDuration > 0 or game.enemy.harpoonDuration > 0) and
            protoShipsList[enemyShipIndex].crew.len > 0:
          setLayoutRowDynamic(height = 35, cols = 1)
          var boardingParty, defenders: string = ""
          for member in playerShip.crew:
            case member.order
            of boarding:
              boardingParty = boardingParty & member.name & ", "
            of defend:
              defenders = defenders & member.name & ", "
            else:
              discard
          if boardingParty.len > 0:
            boardingParty.strip(chars = {' ', ','})
          else:
            boardingParty = "None"
          if defenders.len > 0:
            defenders.strip(chars = {' ', ','})
          else:
            defenders = "None"
          if gameSettings.showTooltips:
            addTooltip(bounds = getWidgetBounds(),
                text = "Set your boarding party. If you join it, you will be able to give orders them, but not your gunners or engineer.")
          labelButton(title = "Boarding party:"):
            dialog = boardingDialog
            setDialog(x = windowWidth / 4)
          var labelHeight = ceil(x = getTextWidth(text = boardingParty) / (if expandedSection == 1: windowWidth.float else: (windowWidth.float / 2.0))) * 35.0
          setLayoutRowDynamic(height = labelHeight, cols = 1)
          wrapLabel(str = boardingParty)
          setLayoutRowDynamic(height = 35, cols = 1)
          if gameSettings.showTooltips:
            addTooltip(bounds = getWidgetBounds(),
                text = "Set your ship's defenders against the enemy party.")
          labelButton(title = "Defenders:"):
            dialog = defendingDialog
            setDialog(x = windowWidth / 4)
          labelHeight = ceil(x = getTextWidth(text = defenders) / (if expandedSection == 1: windowWidth.float else: (windowWidth.float / 2.0))) * 35.0
          setLayoutRowDynamic(height = labelHeight, cols = 1)
          wrapLabel(str = defenders)
      except:
        dialog = setError(message = "Can't show information about boarding party and defenders.")
  # The enemy's ship's info
  if expandedSection in {0, 2}:
    group(title = "Enemy info:", flags = {windowBorder, windowTitle}):
      setLayoutRowStatic(height = 35, cols = 1, width = 35)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Maximize/minimize the enemy's ship info")
      imageButton(image = (if expandedSection == 0: images[expandIcon] else: images[contractIcon])):
        if expandedSection == 2:
          expandedSection = 0
        else:
          expandedSection = 2
      setLayoutRowDynamic(height = 25, cols = 2)
      label(str = "Name:")
      colorLabel(str = enemyName, color = theme.colors[goldenColor])
      label(str = "Type:")
      colorLabel(str = game.enemy.ship.name, color = theme.colors[goldenColor])
      label(str = "Home:")
      colorLabel(str = skyBases[game.enemy.ship.homeBase].name, color = theme.colors[goldenColor])
      label(str = "Distance:")
      colorLabel(str = (if game.enemy.distance >= 15_000: "Escaped" elif game.enemy.distance in
        10_000 ..
        15_000: "Long" elif game.enemy.distance in 5_000 ..
        10_000: "Medium" elif game.enemy.distance in 1_000 ..
        5_000: "Short" else: "Close"), color = theme.colors[goldenColor])
      label(str = "Status:")
      var enemyInfo: string = ""
      if game.enemy.distance < 15_000:
        if game.enemy.ship.modules[0].durability == 0:
          enemyInfo = enemyInfo & "Destroyed"
        else:
          var enemyStatus: string = "Ok"
          for module in game.enemy.ship.modules:
            if module.durability < module.maxDurability:
              enemyStatus = "Damaged"
              break
          enemyInfo = enemyInfo & enemyStatus
        for module in game.enemy.ship.modules:
          if module.durability > 0:
            try:
              case modulesList[module.protoIndex].mType
              of armor:
                enemyInfo = enemyInfo & " (armored)"
              of gun:
                enemyInfo = enemyInfo & " (gun)"
              of batteringRam:
                enemyInfo = enemyInfo & " (battering ram)"
              of harpoonGun:
                enemyInfo = enemyInfo & " (harpoon gun)"
              else:
                discard
            except:
              dialog = setError(message = "Can't show information about the enemy's ship. No proto module with index:" &
                  $module.protoIndex, e = nil)
      else:
        enemyInfo = enemyInfo & "Unknown"
      colorLabel(str = enemyInfo, color = theme.colors[goldenColor])
  setLayoutRowDynamic(height = 35, cols = 1)
  labelButton(title = "Next turn"):
    try:
      combatTurn()
    except:
      dialog = setError(message = "Can't make next turn in combat.")
  showGameMenu(dialog = dialog)
