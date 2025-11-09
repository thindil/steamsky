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

import std/[math, strbasics, strutils, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[combat, config, crewinventory, game, items, messages, shipscrew, shipmodules, shipsmovement, types]
import coreui, dialogs, errordialog, header, messagesui, setui, themes

const
  pilotOrders: array[4, string] = ["Go closer", "Keep distance", "Evade", "Escape"]
  engineerOrders: array[4, string] = ["All stop", "Quarter speed", "Half speed", "Full speed"]
  gunnersOrders: array[1..6, string] = ["Don't shoot", "Precise fire",
      "Fire at will", "Aim for their engine", "Aim for their weapon", "Aim for their hull"]

var expandedSection: Natural = 0

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

proc showPartyMenu*(dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Assign the player's ship's crew members to a boarding party or defenders
  ##
  ## * dialog   - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog
  const
    width: float = 400
    height: float = 400

  let windowName: string = "Assign crew members to " &
    (if dialog == boardingDialog: "boarding party" else: "defenders")

  updateDialog(width = width, height = height)
  window(name = windowName, x = dialogX, y = dialogY, w = width, h = height,
    flags = {windowBorder, windowTitle, windowMovable}):
    setLayoutRowStatic(height = 35, cols = 2, width = 35)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Select all crew members")
    imageButton(image = images[selectAllIcon]):
      if dialog == boardingDialog:
        for checked in boardingParty.mitems:
          checked = true
      else:
        for checked in defenders.mitems:
          checked = true
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Unselect all crew members")
    imageButton(image = images[unselectAllIcon]):
      if dialog == boardingDialog:
        for checked in boardingParty.mitems:
          checked = false
      else:
        for checked in defenders.mitems:
          checked = false
    setLayoutRowDynamic(height = 30, cols = 1)
    for index, member in playerShip.crew:
      if dialog == boardingDialog:
        checkbox(label = member.name, checked = boardingParty[index])
      else:
        checkbox(label = member.name, checked = defenders[index])
    setLayoutRowDynamic(height = 30, cols = 2)
    imageLabelButton(image = images[assignCrewIcon], text = "Assign",
      alignment = right):
      for index, member in playerShip.crew:
        let
          order: CrewOrders = (if dialog == boardingDialog: boarding else: defend)
          selected: bool = (if dialog == boardingDialog: boardingParty[index] else: defenders[index])
        if member.order == order and not selected:
          try:
            giveOrders(ship = playerShip, memberIndex = index, givenOrder = rest)
          except:
            dialog = setError(message = "Can't give order to not selected crew member.")
            return
        elif selected and member.order != order:
          try:
            giveOrders(ship = playerShip, memberIndex = index, givenOrder = order,
                moduleIndex = -1, checkPriorities = false)
          except:
            dialog = setError(message = "Can't give order to selected crew member.")
            return
          if order == boarding:
            boardingOrders.add(y = 0)
      updateParties()
      pilotIndex = findMember(order = pilot) + 1
      engineerIndex = findMember(order = engineer) + 1
      dialog = none
    imageLabelButton(image = images[cancelIcon], text = "Close",
      alignment = right):
      dialog = none

  windowSetFocus(name = windowName)

proc showCombat*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the combat UI
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  if showHeader(dialog = dialog, close = (if endCombat: CloseDestination.map
    else: CloseDestination.none), state = state):
    return
  # Draw UI
  if pilotList.len != playerShip.crew.len + 1:
    updateCrewLists()
  let
    height: float = (windowHeight - 35 - gameSettings.messagesPosition.float)
    faction = try:
        factionsList[playerShip.crew[0].faction]
      except:
        dialog = setError(message = "Can't get the player's faction.")
        return
  if expandedSection == 0:
    setLayoutRowDynamic(height = height / 2, cols = 2)
  else:
    setLayoutRowDynamic(height = height, cols = 1)
  # The player's ship's crew orders
  if expandedSection in {0, 1}:
    group(title = "Your ship crew orders:", flags = {windowBorder, windowTitle}):
      if dialog != none:
        windowDisable()
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
          if "sentientships" in faction.flags:
            addMessage(message = "Order for ship was set on: " & pilotOrders[
              newOrder], mType = combatMessage)
          else:
            addMessage(message = "Order for " & playerShip.crew[findMember(
                order = pilot)].name & " was set on: " & pilotOrders[newOrder],
                mType = combatMessage)
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
          if "sentientships" in faction.flags:
            addMessage(message = "Order for ship was set on: " & engineerOrders[
              newOrder], mType = combatMessage)
          else:
            addMessage(message = "Order for " & playerShip.crew[findMember(
                order = engineer)].name & " was set on: " & engineerOrders[
                newOrder], mType = combatMessage)
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
                    protoIndex = itemIndex, itemQuality = any)
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
            addMessage(message = "Order for " & playerShip.crew[
                playerShip.modules[guns[gunIndex][1]].owner[0]].name &
                " was set on: " & gunnerOrders[gun[2]], mType = combatMessage)
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
      if dialog != none:
        windowDisable()
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
      label(str = "Speed:")
      enemyInfo = ""
      if game.enemy.distance < 15_000:
        case game.enemy.ship.speed
        of fullStop:
          enemyInfo = enemyInfo & "Stopped"
        of quarterSpeed:
          enemyInfo = enemyInfo & "Slow"
        of halfSpeed:
          enemyInfo = enemyInfo & "Medium"
        of fullSpeed:
          enemyInfo = enemyInfo & "Fast"
        else:
          discard
        if game.enemy.ship.speed != fullStop:
          let speedDiff: int = try:
              realSpeed(ship = game.enemy.ship) - realSpeed(ship = playerShip)
            except:
              dialog = setError(message = "Can't count the speed difference.")
              return
          if speedDiff > 250:
            enemyInfo = enemyInfo & " (much faster)"
          elif speedDiff > 0:
            enemyInfo = enemyInfo & " (faster)"
          elif speedDiff == 0:
            enemyInfo = enemyInfo & " (equal)"
          elif speedDiff > -250:
            enemyInfo = enemyInfo & " (slower)"
          else:
            enemyInfo = enemyInfo & " (much slower)"
      else:
        enemyInfo = enemyInfo & "Unknown"
      colorLabel(str = enemyInfo, color = theme.colors[goldenColor])
  # The player's ship's status
  if expandedSection in {0, 3}:
    group(title = "Your ship status:", flags = {windowBorder, windowTitle}):
      if dialog != none:
        windowDisable()
      setLayoutRowStatic(height = 35, cols = 1, width = 35)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Maximize/minimize the ship status info")
      imageButton(image = (if expandedSection == 0: images[expandIcon] else: images[contractIcon])):
        if expandedSection == 3:
          expandedSection = 0
        else:
          expandedSection = 3
      setLayoutRowDynamic(height = 25, cols = 2)
      for module in playerShip.modules:
        if module.durability > 0:
          label(str = module.name)
        else:
          colorLabel(str = module.name, color = theme.colors[grayColor])
        var damagePercent: int = ((module.durability.float / module.maxDurability.float) * 100.0).int
        changeStyle(field = progressbar,
          color = (if damagePercent == 100: theme.colors[greenColor]
            elif damagePercent > 24: theme.colors[yellowColor]
            else: theme.colors[redColor])):
          progressBar(value = damagePercent, maxValue = 100, modifyable = false)
  # The enemy's ship's status
  if expandedSection in {0, 4}:
    group(title = "Enemy ship status:", flags = {windowBorder, windowTitle}):
      if dialog != none:
        windowDisable()
      setLayoutRowStatic(height = 35, cols = 1, width = 35)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Maximize/minimize the ship status info")
      imageButton(image = (if expandedSection == 0: images[expandIcon] else: images[contractIcon])):
        if expandedSection == 4:
          expandedSection = 0
        else:
          expandedSection = 4
      setLayoutRowDynamic(height = 25, cols = 2)
      if endCombat:
        game.enemy.distance = 100
      for module in game.enemy.ship.modules.mitems:
        if endCombat:
          module.durability = 0
        let moduleName: string = try: (if game.enemy.distance > 1_000:
              getModuleType(moduleIndex = module.protoIndex) else:
              modulesList[module.protoIndex].name)
            except:
              dialog = setError(message = "Can't show the enemy's ship's module name")
              return
        if module.durability > 0:
          label(str = moduleName)
        else:
          colorLabel(str = moduleName, color = theme.colors[grayColor])
        var damagePercent: int = ((module.durability.float / module.maxDurability.float) * 100.0).int
        changeStyle(field = progressbar,
          color = (if damagePercent == 100: theme.colors[greenColor]
            elif damagePercent > 24: theme.colors[yellowColor]
            else: theme.colors[redColor])):
          progressBar(value = damagePercent, maxValue = 100, modifyable = false)
  if endCombat:
    inCombat = false
  else:
    setLayoutRowDynamic(height = 35, cols = 1)
    labelButton(title = "Next turn"):
      try:
        combatTurn()
      except:
        dialog = setError(message = "Can't make next turn in combat.")
      if playerShip.crew[0].order == boarding:
        state = boarding
  let heightDiff: float = (if endCombat: 55 else: 90)
  showLastMessages(theme = theme, dialog = dialog, inCombat = true,
    height = windowHeight - heightDiff - height - 20)

proc showBoardingInfo(index: Natural; inCrew: bool = true; dialog: var GameDialog)
  {.raises: [], tags: [RootEffect], contractual.} =
  ## Show information about the selected mob in the boarding combat
  ##
  ## * index  - the index of the mob which information will be shown
  ## * inCrew - if true, the index is in the player's ship's crew
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog.
  var info = "Uses: "
  if inCrew:
    for item in playerShip.crew[index].equipment:
      if item > -1:
        info = info & "\n" & getItemName(item = playerShip.crew[
            index].inventory[item])
  else:
    for item in game.enemy.ship.crew[index].equipment:
      if item > -1:
        info = info & "\n" & getItemName(item = game.enemy.ship.crew[
            index].inventory[item])
  dialog = setInfo(text = info, title = "More info")

proc showBoarding*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the boarding UI
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  if showHeader(dialog = dialog, close = (if endCombat: CloseDestination.map else:
    CloseDestination.none), state = state):
    return
  let height: float = (windowHeight - 35 - gameSettings.messagesPosition.float)
  setLayoutRowDynamic(height = height, cols = (if expandedSection == 0: 2 else: 1))
  var ordersList: seq[string] = @[]
  for member in game.enemy.ship.crew:
    ordersList.add(y = "Attack " & member.name)
  ordersList.add(y = "Back to the ship")
  # The player's ship's crew info
  if expandedSection in {0, 1}:
    group(title = "Your crew:", flags = {windowBorder, windowTitle}):
      if dialog != none:
        windowDisable()
      setLayoutRowStatic(height = 35, cols = 1, width = 35)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Maximize/minimize your crew list")
      imageButton(image = (if expandedSection == 0: images[expandIcon] else:
        images[contractIcon])):
        if expandedSection == 1:
          expandedSection = 0
        else:
          expandedSection = 1
      setLayoutRowDynamic(height = 25, cols = 3)
      label(str = "Member", alignment = centered)
      label(str = "Health", alignment = centered)
      label(str = "Order", alignment = centered)
      var orderIndex: Natural = 0
      for index, member in playerShip.crew:
        if member.order != boarding:
          continue
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(),
              text = "Show information about the crew member.")
        labelButton(title = member.name):
          showBoardingInfo(index = index, dialog = dialog)
        var health: int = member.health
        changeStyle(field = progressbar,
          color = (if health == 100: theme.colors[greenColor]
            elif health > 24: theme.colors[yellowColor]
            else: theme.colors[redColor])):
          if gameSettings.showTooltips:
            addTooltip(bounds = getWidgetBounds(),
                text = "The crew member's health.")
          progressBar(value = health, maxValue = 100, modifyable = false)
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(),
              text = "The crew member current order.")
        let newOrder = comboList(items = ordersList,
          selected = (if boardingOrders[orderIndex] > -1: boardingOrders[orderIndex]
          else: ordersList.high), itemHeight = 25, x = 200, y = 150)
        if newOrder != boardingOrders[orderIndex]:
          boardingOrders[orderIndex] = (if newOrder == game.enemy.ship.crew.len: -1
            else: newOrder)
        orderIndex.inc
  # The enemy's ship's crew info
  if expandedSection in {0, 2}:
    group(title = "Your crew:", flags = {windowBorder, windowTitle}):
      if dialog != none:
        windowDisable()
      setLayoutRowStatic(height = 35, cols = 1, width = 35)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Maximize/minimize enemy's ship's crew list")
      imageButton(image = (if expandedSection == 0: images[expandIcon] else:
        images[contractIcon])):
        if expandedSection == 1:
          expandedSection = 0
        else:
          expandedSection = 1
      setLayoutRowDynamic(height = 25, cols = 3)
      label(str = "Member", alignment = centered)
      label(str = "Health", alignment = centered)
      label(str = "Order", alignment = centered)
      for index, member in game.enemy.ship.crew:
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(),
              text = "Show information about the enemy's ship's crew member.")
        labelButton(title = member.name):
          showBoardingInfo(index = index, inCrew = false, dialog = dialog)
        var health: int = member.health
        changeStyle(field = progressbar,
          color = (if health == 100: theme.colors[greenColor]
            elif health > 24: theme.colors[yellowColor]
            else: theme.colors[redColor])):
          if gameSettings.showTooltips:
            addTooltip(bounds = getWidgetBounds(),
                text = "The enemy's ships's crew member's health.")
          progressBar(value = health, maxValue = 100, modifyable = false)
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(),
              text = "The enemy's ship's crew member current order.")
        label(str = ($member.order).capitalizeAscii)
  if endCombat:
    inCombat = false
  else:
    setLayoutRowDynamic(height = 35, cols = 1)
    labelButton(title = "Next turn"):
      try:
        combatTurn()
      except:
        dialog = setError(message = "Can't make next turn in combat.")
      if playerShip.crew[0].order != boarding:
        state = combat
        updateParties()
  let heightDiff: float = (if endCombat: 55 else: 90)
  showLastMessages(theme = theme, dialog = dialog, inCombat = true, height = windowHeight - heightDiff - height)
