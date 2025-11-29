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
# along with Steam Sky.  if, see <http://www.gnu.org/licenses/>.

## Provides code related to the game's dialogs, like showing questions, etc.

import std/[colors, os, math, strutils, tables, times]
import contracts, nuklear/nuklear_sdl_renderer
import ../[bases, basescargo, config, crewinventory, game, game2, items, maps,
    messages, missions, shipscargo, shipscrew, shipscrew2, stories, types,
    trades, utils]
import coreui, errordialog, setui, themes

type
  QuestionType* = enum
    ## Types of questions, used to set actions to the player's response
    deleteSave, showDeadStats, quitGame, resignGame, homeBase, finishGame,
      dismissMember, deleteMessages
  QuestionData = object
    question, data: string
    qType: QuestionType
    lines: float
  MessageData = object
    text, title: string
    lines, started: float
  ButtonSettings* = object
    ## Used to store information about a button in a dialog
    text*: string    ## Text to show on the button
    code*: proc(dialog: var GameDialog) ## The code to execute when the button was pressed
    icon*: int       ## The index of the icon to show on the button
    tooltip*: string ## The tooltip text associated with the button
    color*: string   ## The color of the button's text
  TextData = object
    text: string
    lines: float
    color: Color
  InfoData = object
    data: seq[TextData]
    title: string
    button1, button2: ButtonSettings
    widgetsAmount: seq[Positive]
  ManipulateType* = enum
    ## Types of action, used to manipulate items, like selling or buying items
    sellAction, buyAction, takeAction, dropAction, moveAction, giveAction, dropCargoAction
  ManipulateData = object
    itemIndex: int
    amount, maxAmount, cost, allCost, data: Natural
    title, warning: string

const emptyButtonSettings*: ButtonSettings = ButtonSettings(text: "", code: nil,
    icon: -1, tooltip: "",
    color: "") ## Empty Button setting, used to disable the selected button

var
  questionData: QuestionData = QuestionData(question: "", data: "")
  messageData: MessageData = MessageData(text: "", title: "Info")
  answered*: bool = false ## If true, the question was answered
  infoData: InfoData = InfoData(data: @[], title: "",
      button1: emptyButtonSettings, button2: emptyButtonSettings)
  manipulateData: ManipulateData = ManipulateData(itemIndex: 0, maxAmount: 0,
      cost: 0, title: "")
  timer: float = gameSettings.autoCloseMessagesTime.float * 1000.0

proc setQuestion*(question: string; qType: QuestionType;
    data: string = ""): GameDialog {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the data related to the current in-game question
  ##
  ## * question - the question which will be asked to the player
  ## * qType    - the type of the question, used to set action to the player's
  ##              answer
  ## * dialog   - the current in-game dialog displayed on the screen
  ##
  ## Returns the questionDialog if the message was set, otherwise
  ## errorDialog
  setDialog()
  try:
    var needLines: float = ceil(x = getTextWidth(text = question) / 350)
    if needLines < 1.0:
      needLines = 1.0
    questionData = QuestionData(question: question, data: data,
        lines: needLines, qType: qType)
    answered = false
    result = questionDialog
  except:
    result = setError(message = "Can't set the question.")

proc setMessage*(message, title: string): GameDialog {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Set the data related to the current in-game message
  ##
  ## * message - the message which will be show to the player
  ## * title   - the title of the message's dialog
  ##
  ## Returns the messageDialog if the message was set, otherwise
  ## errorDialog
  setDialog()
  try:
    var needLines: float = ceil(x = getTextWidth(text = message) / 250)
    if needLines < 1.0:
      needLines = 1.0
    messageData = MessageData(text: message, title: title, lines: needLines,
        started: cpuTime())
    timer = gameSettings.autoCloseMessagesTime.float * 1000.0
    result = messageDialog
  except:
    result = setError(message = "Can't set the message.")

proc showQuestion*(dialog: var GameDialog; state: var GameState) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the current question to the player
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ## * state  - the current game's state
  ##
  ## Returns the parameter dialog and state. The first is modified only when
  ## the player closed the dialog and the second when the player quit the game.
  if dialog != questionDialog:
    return
  try:
    const
      width: float = 350
      height: float = 150

    proc setMainMenu(dialog: var GameDialog; state: var GameState) {.raises: [],
    tags: [], contractual.} =
      ## Set the main window for the main game menu
      ##
      ## * dialog - the current in-game dialog displayed on the screen
      ## * state  - the current game's state
      ##
      ## Returns the parameter dialog and state.
      state = endGame
      nuklearResizeWin(width = menuWidth, height = menuHeight)
      nuklearSetWindowPos(x = windowCentered, y = windowCentered)
      nuklearSetWindowResizable(resizable = false)
      closePopup()
      dialog = none

    updateDialog(width = width, height = height)
    popup(pType = staticPopup, title = "Question", x = dialogX, y = dialogY,
        w = width, h = height, flags = {windowBorder, windowTitle,
        windowNoScrollbar, windowMovable}):
      setLayoutRowDynamic(height = 30 * questionData.lines, cols = 1)
      wrapLabel(str = questionData.question)
      setLayoutRowDynamic(height = 30, cols = 2)
      labelButton(title = "Yes"):
        closePopup()
        dialog = none
        case questionData.qType
        of deleteSave:
          try:
            removeFile(file = questionData.data)
            closePopup()
            dialog = none
          except:
            dialog = setError(message = "Can't remove the save file.")
        of quitGame:
          try:
            endGame(save = true)
            setMainMenu(dialog = dialog, state = state)
          except:
            dialog = setError(message = "Can't end the game.")
        of resignGame:
          try:
            death(memberIndex = 0, reason = "resignation", ship = playerShip)
            closePopup()
            dialog = none
          except:
            dialog = setError(message = "Can't kill the player.")
        of homeBase:
          let moneyAmount: Natural = moneyAmount(inventory = playerShip.cargo)
          if moneyAmount == 0:
            dialog = setMessage(message = "You don't have any " & moneyName &
                " for change ship home base.", title = "No money")
            return
          let price: Natural = questionData.data.parseInt
          if moneyAmount < price:
            dialog = setMessage(message = "You don't have enough " & moneyName &
                " for change ship home base.", title = "No money")
            return
          playerShip.homeBase = skyMap[playerShip.skyX][
              playerShip.skyY].baseIndex
          updateMoney(memberIndex = -1, amount = -price, quality = any)
          addMessage(message = "You changed your ship home base to: " &
              skyBases[playerShip.homeBase].name, mType = otherMessage)
          let traderIndex: int = findMember(order = talk)
          gainExp(amount = 1, skillNumber = talkingSkill,
              crewIndex = traderIndex)
          try:
            updateGame(minutes = 10)
          except:
            dialog = setError(message = "Can't update the game.")
            return
        of finishGame:
          try:
            death(memberIndex = 0, reason = "retired after finished the game",
                ship = playerShip)
            closePopup()
            dialog = setQuestion(question = "You are dead. Would you like to see your game statistics?",
                qType = showDeadStats)
          except:
            dialog = setError(message = "Can't kill the player.")
            return
        of dismissMember:
          let
            baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
                playerShip.skyY].baseIndex
            memberIndex: int = try:
                questionData.data.parseInt
              except:
                dialog = setError(message = "Can't get the member index.")
                return
          addMessage(message = "You dismissed " & playerShip.crew[
              memberIndex].name & ".", mType = orderMessage)
          try:
            deleteMember(memberIndex = memberIndex, ship = playerShip)
          except:
            dialog = setError(message = "Can't delete the member.")
            return
          skyBases[baseIndex].population.inc
          for index, _ in playerShip.crew:
            try:
              updateMorale(ship = playerShip, memberIndex = index,
                  value = getRandom(min = -5, max = -1))
            except:
              dialog = setError(message = "Can't update the crew's morale.")
              return
          refreshCrewList()
        of deleteMessages:
          discard
        of showDeadStats:
          discard
      labelButton(title = "No"):
        if questionData.qType == showDeadStats:
          endGame(save = false)
          setMainMenu(dialog = dialog, state = state)
        else:
          closePopup()
          dialog = none
      if dialog == none:
        questionData = QuestionData(question: "", data: "")
        answered = true
  except:
    answered = true
    dialog = setError(message = "Can't show the question")

proc addCloseButton*(dialog: var GameDialog; icon: IconsNames = exitIcon;
    color: ColorsNames = buttonTextColor; isPopup: bool = true;
    label: string = "Close") {.raises: [], tags: [], contractual.} =
  ## Add the close button to the dialog
  ##
  ## * dialog  - the current in-game dialog displayed on the screen
  ## * icon    - the icon used on the button. Default is exitIcon
  ## * color   - the color of the text on the button
  ## * isPopup - if true, the dialog is a popup, otherwise it is a window
  ## * label   - the label to show on the button
  ##
  ## Returns the parameter dialog. It is modified only when the player closed
  ## the dialog.
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(),
        text = "Close the dialog [Escape key]")
  setButtonStyle(field = textNormal, color = theme.colors[color])
  imageLabelButton(image = images[icon], text = label, alignment = right):
    if isPopup:
      closePopup()
    dialog = none
  restoreButtonStyle()

proc showMessage*(dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the current question to the player
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the parameter dialog. It is modified only when the player closed
  ## the dialog.
  if dialog != messageDialog:
    return
  if timer == 0.0 and dialog == messageDialog:
    dialog = none
    return
  try:
    const
      width: float = 350
      height: float = 200

    updateDialog(width = width, height = height)
    popup(pType = staticPopup, title = messageData.title, x = dialogX,
        y = dialogY, w = width, h = height, flags = {windowBorder, windowTitle,
        windowNoScrollbar, windowMovable}):
      setLayoutRowDynamic(height = 30 * messageData.lines, cols = 1)
      wrapLabel(str = messageData.text)
      setLayoutRowDynamic(height = 30, cols = 1)
      addCloseButton(dialog = dialog, label = "Close " & $((timer /
          1000.0).ceil.int))
  except:
    dialog = setError(message = "Can't show the message")

var infoWidth: float = 0.0

proc setInfo*(text, title: string; button1: ButtonSettings = emptyButtonSettings;
    button2: ButtonSettings = emptyButtonSettings): GameDialog {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Set the data related to the current in-game info dialog
  ##
  ## * text    - the text to show in the dialog. Can use special tags for
  ##             colors, like `{gold}{/gold}`
  ## * title   - the title of the dialog
  ## * button1 - the settings for the first optional button. If empty, the
  ##             button will not show
  ## * button2 - the settings for the second optional button. If empty, the
  ##             button will not show
  ##
  ## Returns the infoDialog if the info was set, otherwise errorDialog
  setDialog(x = windowWidth / 5.0)
  try:
    infoWidth = windowWidth / 1.5
    var
      startIndex: int = 0
      tagIndex: int = text.find(sub = '{')
      parts: seq[TextData] = @[]
      widgetsAmount: seq[Positive] = @[]
      lineWidth, wAmount: Natural = 0
    while true:
      if tagIndex == -1:
        tagIndex = text.len
      var
        partText: string = text[startIndex..tagIndex - 1]
        needLines: float = ceil(x = getTextWidth(text = partText) / infoWidth)
        newLines: float = partText.count(sub = '\n').float + 1.0
      if needLines < 1.0:
        needLines = 1.0
      if needLines < newLines:
        needLines = newLines
      parts.add(y = TextData(text: partText, color: theme.colors[
          foregroundColor], lines: needLines))
      lineWidth += getTextWidth(text = partText).Natural
      wAmount.inc
      if lineWidth > infoWidth.Natural or tagIndex == text.len:
        widgetsAmount.add(y = wAmount)
        break
      startIndex = tagIndex
      tagIndex = text.find(sub = '}', start = startIndex)
      let tagName: string = text[startIndex + 1..tagIndex - 1]
      startIndex = tagIndex + 1
      tagIndex = text.find(sub = "{/" & tagName & "}", start = startIndex)
      partText = text[startIndex..tagIndex - 1]
      needLines = ceil(x = getTextWidth(text = partText) / infoWidth)
      if needLines < 1.0:
        needLines = 1.0
      parts.add(y = TextData(text: partText, color: case tagName
        of "gold":
          theme.colors[goldenColor]
        of "green":
          theme.colors[greenColor]
        of "red:":
          theme.colors[redColor]
        else:
          theme.colors[foregroundColor], lines: needLines))
      if needLines > 1:
        widgetsAmount.add(y = 1)
        lineWidth = 0
        wAmount = 0
      lineWidth += getTextWidth(text = partText).Natural
      if lineWidth <= infoWidth.Natural:
        wAmount.inc
      widgetsAmount.add(y = wAmount)
      wAmount = 0
      lineWidth = 0
      startIndex = tagIndex + tagName.len + 3
      if text[startIndex] == '\n':
        startIndex.inc
      tagIndex = text.find(sub = '{', start = startIndex)
    infoData = InfoData(data: parts, button1: button1, button2: button2,
        widgetsAmount: widgetsAmount, title: title)
    return infoDialog
  except:
    return setError(message = "Can't set the message.")

proc showInfo*(dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the info to the player
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the parameter dialog. It is modified only when the player closed
  ## the dialog.
  if dialog != infoDialog:
    return
  try:
    var height: float = 90
    for data in infoData.data:
      height += (30 * data.lines).float
    updateDialog(width = infoWidth, height = height)
    popup(pType = staticPopup, title = infoData.title, x = dialogX,
        y = dialogY, w = infoWidth, h = height, flags = {windowBorder,
        windowTitle, windowNoScrollbar, windowMovable}):
      var index: Natural = 0
      for wAmount in infoData.widgetsAmount:
        if wAmount == 1:
          setLayoutRowDynamic(height = 30 * infoData.data[index].lines, cols = 1)
          colorWrapLabel(str = infoData.data[index].text, color = infoData.data[index].color)
        else:
          setLayoutRowDynamic(height = 30, cols = wAmount)
          for index2 in index..index + wAmount - 1:
            colorWrapLabel(str = infoData.data[index2].text,
                color = infoData.data[index2].color)
        index += wAmount
      var cols: Positive = 3
      if infoData.button1 == emptyButtonSettings:
        cols.dec
      if infoData.button2 == emptyButtonSettings:
        cols.dec
      setLayoutRowDynamic(height = 30, cols = cols)
      # Draw the first optional button
      if infoData.button1 != emptyButtonSettings:
        let button: ButtonSettings = infoData.button1
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(),
              text = button.tooltip)
        if button.icon > -1:
          if button.text.len == 0:
            imageButton(image = images[button.icon.IconsNames]):
              button.code(dialog = dialog)
          else:
            imageLabelButton(image = images[button.icon.IconsNames],
                text = button.text, alignment = right):
              button.code(dialog = dialog)
        else:
          labelButton(title = button.text):
            button.code(dialog = dialog)
      # Draw close button
      addCloseButton(dialog = dialog)
      # Draw the second optional button
      if infoData.button2 != emptyButtonSettings:
        let button: ButtonSettings = infoData.button2
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(),
              text = button.tooltip)
        if button.icon > -1:
          if button.text.len == 0:
            imageButton(image = images[button.icon.IconsNames]):
              button.code(dialog = dialog)
          else:
            imageLabelButton(image = images[button.icon.IconsNames],
                text = button.text, alignment = right):
              button.code(dialog = dialog)
        else:
          labelButton(title = button.text):
            button.code(dialog = dialog)
  except:
    dialog = setError(message = "Can't show the info")

proc updateMaxAmount(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Update max allowed amount of items to give
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  let
    item: InventoryData = playerShip.cargo[manipulateData.itemIndex]
    memberIndex: Natural = manipulateData.data
  manipulateData.maxAmount = try:
        (freeInventory(memberIndex = memberIndex, amount = 0).float / itemsList[
            item.protoIndex].weight.float).Natural
      except:
        dialog = setError(message = "Can't count the max amount.")
        return
  if item.amount < manipulateData.maxAmount:
    manipulateData.maxAmount = item.amount

proc setManipulate*(action: ManipulateType; iIndex: int;
    mIndex: Natural = 0): GameDialog {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the data related to the current in-game manipulate item dialog
  ##
  ## * action - the action used to manipulate items, like selling or buying
  ## * iIndex - the index of the item to manipulate, in the player's ship's
  ##            cargo (if positive) or in a trader's cargo (if negative)
  ## * mIndex - the index of the player's ship's crew member in which inventory
  ##            the item is stored. Used for moving items from or to the ship's
  ##            cargo
  ##
  ## Returns the type of dialog if the dialog was set, otherwise errorDialog
  setDialog(x = windowWidth / 5.0)
  case action
  of buyAction, sellAction:
    let (protoIndex, maxSellAmount, maxBuyAmount, price, _) = try:
        getTradeData(iIndex = iIndex)
      except:
        return setError(message = "Can't get the trade's data.")
    try:
      manipulateData = ManipulateData(itemIndex: iIndex, maxAmount: (
          if action == buyAction: maxBuyAmount else: maxSellAmount),
          cost: price, title: (if action == buyAction: "Buy " else: "Sell ") &
          itemsList[protoIndex].name, amount: 1, warning: "", allCost: price)
    except:
      return setError(message = "Can't set the manipulate data.")
    if action == buyAction:
      return buyDialog
    return sellDialog
  of takeAction, dropAction:
    let (protoIndex, maxAmount, cargoMaxAmount, _) = try:
        getLootData(itemIndex = iIndex)
      except:
        return setError(message = "Can't get the trade's data.")
    try:
      manipulateData = ManipulateData(itemIndex: iIndex, maxAmount: (
          if action == takeAction: maxAmount else: cargoMaxAmount),
          cost: 0, title: (if action == takeAction: "Take " else: "Drop ") &
          itemsList[protoIndex].name, amount: 1, warning: "", allCost: 0)
    except:
      return setError(message = "Can't set the manipulate data.")
    if action == takeAction:
      return takeDialog
    return dropDialog
  of moveAction:
    manipulateData = ManipulateData(itemIndex: iIndex,
        maxAmount: playerShip.crew[mIndex].inventory[iIndex].amount,
        title: "Move " & getItemName(item = playerShip.crew[mIndex].inventory[
        iIndex], damageInfo = false, toLower = false) & " to ship cargo",
        warning: "", allCost: 0, amount: 1, data: mIndex)
    return moveDialog
  of giveAction:
    manipulateData = ManipulateData(itemIndex: iIndex, maxAmount: 1,
        title: "Give " & getItemName(item = playerShip.cargo[iIndex],
        damageInfo = false, toLower = false) & " to a crew member", warning: "",
        allCost: 0, amount: 1, data: 0)
    result = giveDialog
    updateMaxAmount(dialog = result)
  of dropCargoAction:
    manipulateData = ManipulateData(itemIndex: iIndex,
        maxAmount: playerShip.cargo[iIndex].amount, title: "Drop " &
        getItemName(item = playerShip.cargo[iIndex], damageInfo = false,
        toLower = false) & " from ship cargo", warning: "", allCost: 0, amount: 1)
    return dropCargoDialog

proc updateCost(amount, cargoIndex: Natural; dialog: GameDialog) {.raises: [
    KeyError], tags: [], contractual.} =
  ## Update cost of the item and the warning message
  ##
  ## * amount     - the amount of the item
  ## * cargoIndex - the index of the item in the player's ship's cargo
  ## * dialog     - the type of dialog to show
  if manipulateData.cost > 0:
    manipulateData.allCost = manipulateData.amount * manipulateData.cost
    countPrice(price = manipulateData.allCost, traderIndex = findMember(
        order = talk), reduce = dialog == buyDialog)
  else:
    manipulateData.allCost = manipulateData.amount
  manipulateData.warning = ""
  if dialog == buyDialog:
    if getItemAmount(itemType = fuelType) - manipulateData.allCost <=
        gameSettings.lowFuel:
      manipulateData.warning = "You will spend " & moneyName & " below low level of fuel."
  elif dialog in {sellDialog, dropDialog, giveDialog, dropCargoDialog}:
    let action: string = case dialog
      of sellDialog:
        "sell"
      of giveDialog:
        "give"
      else:
        "drop"
    if itemsList[playerShip.cargo[cargoIndex].protoIndex].itemType == fuelType:
      let amount: int = getItemAmount(itemType = fuelType) - amount
      if amount <= gameSettings.lowFuel:
        manipulateData.warning = "You will " & action & " amount below low lewel of fuel."
    for member in playerShip.crew:
      let faction: FactionData = factionsList[member.faction]
      if itemsList[playerShip.cargo[cargoIndex].protoIndex].itemType in
          faction.drinksTypes:
        let amount: int = getItemsAmount(iType = "Drinks") - amount
        if amount <= gameSettings.lowDrinks:
          manipulateData.warning = "You will " & action & " amount below low lewel of drinks."
          break
      elif itemsList[playerShip.cargo[cargoIndex].protoIndex].itemType in
          faction.foodTypes:
        let amount: int = getItemsAmount(iType = "Food") - amount
        if amount <= gameSettings.lowFood:
          manipulateData.warning = "You will " & action & " amount below low lewel of food."
          break

proc showManipulateItem*(dialog: var GameDialog): bool {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the dialog to manipulate the selected item(s) to the player
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the parameter dialog. It is modified only when the player closed
  ## the dialog. Returns true if an item was sold or bought, otherwise false
  result = false
  try:
    const height: float = 220
    let width: float = windowWidth / 1.5
    updateDialog(width = width, height = height)
    window(name = manipulateData.title, x = dialogX, y = dialogY, w = width,
        h = height, flags = {windowBorder, windowTitle, windowNoScrollbar,
        windowMovable}):
      var baseCargoIndex, cargoIndex, protoIndex: int = -1
      if manipulateData.itemIndex < 0:
        baseCargoIndex = manipulateData.itemIndex.abs
        if dialog == takeDialog:
          baseCargoIndex.dec
        let baseIndex: int = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
        protoIndex = skyBases[baseIndex].cargo[baseCargoIndex].protoIndex
        cargoIndex = findItem(inventory = playerShip.cargo,
            protoIndex = protoIndex, itemQuality = skyBases[baseIndex].cargo[
                baseCargoIndex].quality)
      else:
        cargoIndex = manipulateData.itemIndex
        if dialog == dropDialog:
          cargoIndex.dec
      if cargoIndex > -1:
        protoIndex = playerShip.cargo[cargoIndex].protoIndex
        if baseCargoIndex == -1 and dialog != moveDialog:
          baseCargoIndex = findBaseCargo(protoIndex = protoIndex,
              quality = playerShip.cargo[cargoIndex].quality)
      setLayoutRowDynamic(height = 30, cols = 2)
      # Set target (give dialog only)
      if dialog == giveDialog:
        label(str = "To:")
        let newMember: Natural = comboList(items = crewList,
            selected = manipulateData.data, itemHeight = 25, x = 200, y = 150)
        if newMember != manipulateData.data:
          manipulateData.data = newMember
          updateMaxAmount(dialog = dialog)
      # Set amount
      label(str = "Amount (max: " & $manipulateData.maxAmount & "):")
      let newValue: int = property2(name = "#", min = 1,
          val = manipulateData.amount, max = manipulateData.maxAmount, step = 1,
          incPerPixel = 1)
      if newValue != manipulateData.amount:
        manipulateData.amount = newValue
        updateCost(amount = newValue, cargoIndex = cargoIndex, dialog = dialog)
      # Amount buttons
      const amounts: array[1..3, Positive] = [100, 500, 1000]
      var cols: Positive = 1
      for amount in amounts:
        if amount < manipulateData.maxAmount:
          cols.inc
      setLayoutRowDynamic(height = 30, cols = cols)
      for i in 1..cols - 1:
        labelButton(title = $amounts[i]):
          manipulateData.amount = amounts[i]
          updateCost(amount = amounts[i], cargoIndex = cargoIndex,
              dialog = dialog)
      labelButton(title = "Max"):
        manipulateData.amount = manipulateData.maxAmount
        updateCost(amount = manipulateData.amount, cargoIndex = cargoIndex,
            dialog = dialog)
      # Labels
      if manipulateData.cost > 0:
        setLayoutRowDynamic(height = 30, cols = 2)
        label(str = "Total " & (if dialog == buyDialog: "cost:" else: "gain:"))
        colorLabel(str = $manipulateData.allCost & " " & moneyName,
            color = theme.colors[goldenColor])
      setLayoutRowDynamic(height = 30, cols = 1)
      colorLabel(str = manipulateData.warning, color = theme.colors[redColor])
      # Action (buy, sell, etc) button
      type ActionData = object
        icon: IconsNames
        label: string
      let actionButton: ActionData = case dialog
        of buyDialog:
          ActionData(icon: buyIcon, label: "Buy")
        of sellDialog:
          ActionData(icon: sellIcon, label: "Sell")
        of takeDialog:
          ActionData(icon: giveIcon, label: "Take")
        of dropDialog:
          ActionData(icon: dropIcon, label: "Drop")
        of moveDialog:
          ActionData(icon: moveIcon, label: "Move")
        of giveDialog:
          ActionData(icon: giveColoredIcon, label: "Give")
        of dropCargoDialog:
          ActionData(icon: dropColoredIcon, label: "Drop")
        else:
          ActionData(icon: buyIcon, label: "Invalid")
      setLayoutRowDynamic(height = 30, cols = 2)
      setButtonStyle(field = textNormal, color = theme.colors[greenColor])
      imageLabelButton(image = images[actionButton.icon],
          text = actionButton.label, alignment = right):
        let
          baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
              playerShip.skyY].baseIndex
          trader: string = (if baseIndex > 0: "base" else: "ship")
        try:
          case dialog
          of buyDialog:
            buyItems(baseItemIndex = manipulateData.itemIndex.abs,
                amount = $manipulateData.amount)
          of sellDialog:
            sellItems(itemIndex = manipulateData.itemIndex,
                amount = $manipulateData.amount)
          of takeDialog:
            if cargoIndex > -1:
              updateCargo(ship = playerShip, cargoIndex = cargoIndex,
                  amount = manipulateData.amount, durability = skyBases[
                  baseIndex].cargo[baseCargoIndex].durability,
                  quality = skyBases[baseIndex].cargo[baseCargoIndex].quality)
            else:
              updateCargo(ship = playerShip, protoIndex = protoIndex,
                  amount = manipulateData.amount, durability = skyBases[
                  baseIndex].cargo[baseCargoIndex].durability,
                  quality = skyBases[baseIndex].cargo[baseCargoIndex].quality)
            try:
              updateBaseCargo(cargoIndex = baseCargoIndex,
                  amount = -manipulateData.amount, durability = skyBases[
                  baseIndex].cargo[baseCargoIndex].durability,
                      quality = skyBases[baseIndex].cargo[
                      baseCargoIndex].quality)
            except:
              dialog = setError(message = "Can't update the base's cargo3.")
              return
            try:
              addMessage(message = "You took " & $manipulateData.amount & " " &
                  itemsList[protoIndex].name & ".", mType = orderMessage)
            except:
              dialog = setError(message = "Can't add message.")
              return
          of dropDialog:
            if baseCargoIndex > -1:
              try:
                updateBaseCargo(cargoIndex = baseCargoIndex,
                    amount = manipulateData.amount,
                    durability = playerShip.cargo[cargoIndex].durability,
                    quality = playerShip.cargo[cargoIndex].quality)
              except:
                dialog = setError(message = "Can't update the base's cargo.")
                return
            else:
              try:
                updateBaseCargo(protoIndex = protoIndex,
                    amount = manipulateData.amount,
                    durability = playerShip.cargo[cargoIndex].durability,
                    quality = playerShip.cargo[cargoIndex].quality)
              except:
                dialog = setError(message = "Can't update the base's cargo2.")
                return
            updateCargo(ship = playerShip, cargoIndex = cargoIndex,
                amount = -manipulateData.amount, durability = playerShip.cargo[
                cargoIndex].durability, quality = playerShip.cargo[
                cargoIndex].quality)
            try:
              addMessage(message = "You drop " & $manipulateData.amount & " " &
                  itemsList[protoIndex].name & ".", mType = orderMessage)
            except:
              dialog = setError(message = "Can't add message.")
              return
          of moveDialog:
            try:
              moveItem(itemIndex = manipulateData.itemIndex,
                  amount = manipulateData.amount,
                  memberIndex = manipulateData.data)
            except NoFreeCargoError:
              dialog = setMessage(message = getCurrentExceptionMsg(),
                  title = "No free space in cargo")
              return
            except CrewNoSpaceError:
              dialog = setError(message = "Can't update the member's inventory.")
              return
            except CrewOrderError:
              dialog = setMessage(message = getCurrentExceptionMsg(),
                  title = "Can't give an order.")
              return
            except:
              dialog = setError(message = "Can't move item to the ship cargo.")
              return
          of giveDialog:
            let item: InventoryData = playerShip.cargo[manipulateData.itemIndex]
            try:
              if freeInventory(memberIndex = manipulateData.data, amount = -(
                  itemsList[item.protoIndex].weight * manipulateData.amount)) < 0:
                dialog = setMessage(message = "No free space in " &
                    playerShip.crew[manipulateData.data].name &
                    "'s inventory for that amount of " & getItemName(
                    item = item), title = "Can't give item")
                return false
            except:
              dialog = setError(message = "Can't get the item.")
              return false
            addMessage(message = "You gave " & $amount & " " & getItemName(
                item = item) & " to " & playerShip.crew[
                manipulateData.data].name & ".", mType = otherMessage)
            try:
              updateInventory(memberIndex = manipulateData.data,
                  amount = manipulateData.amount, protoIndex = item.protoIndex,
                  durability = item.durability, price = item.price,
                  ship = playerShip, quality = item.quality)
            except:
              dialog = setError(message = "Can't update the member's inventory.")
              return
            updateCargo(ship = playerShip, amount = -manipulateData.amount,
                cargoIndex = manipulateData.itemIndex, price = item.price,
                quality = item.quality)
          of dropCargoDialog:
            var dropAmount, dropAmount2: Natural = manipulateData.amount
            let itemIndex: Natural = manipulateData.itemIndex
            try:
              if itemsList[playerShip.cargo[itemIndex].protoIndex].itemType == missionItemsType:
                for j in 1..dropAmount2:
                  for index, mission in acceptedMissions:
                    if mission.mType == deliver and mission.itemIndex ==
                        playerShip.cargo[itemIndex].protoIndex:
                      deleteMission(missionIndex = index)
                      dropAmount.dec
                      break
              elif currentStory.index.len > 0 and storiesList[
                  currentStory.index].startData[0].parseInt == playerShip.cargo[
                      itemIndex].protoIndex:
                clearCurrentStory()
            except:
              dialog = setError(message = "Can't check the drop amount.")
              return false
            if dropAmount > 0:
              addMessage(message = "You dropped " & $dropAmount & " " &
                  getItemName(item = playerShip.cargo[itemIndex]) & ".",
                      mtype = otherMessage)
              updateCargo(ship = playerShip, protoIndex = playerShip.cargo[
                  itemIndex].protoIndex, amount = -dropAmount,
                  durability = playerShip.cargo[itemIndex].durability,
                  price = playerShip.cargo[itemIndex].price,
                  quality = playerShip.cargo[itemIndex].quality)
          else:
            return false
          dialog = none
          result = true
        except CantBuyError:
          dialog = setMessage(message = "You can't buy " &
              getCurrentExceptionMsg() & " in this " & trader & ".",
                  title = "Can't buy items")
        except NoFreeCargoError:
          dialog = setMessage(message = "You don't have enough free space in your ship's cargo.",
              title = "Can't buy items")
        except NoMoneyError:
          dialog = setMessage(message = "You don't have any " & moneyName &
              " to buy " & getCurrentExceptionMsg() & ".",
                  title = "No money to buy items")
        except NotEnoughMoneyError:
          dialog = setMessage(message = "You don't have enough " & moneyName &
              " to buy so many " & getCurrentExceptionMsg() & ".",
              title = "Not enough money to buy items")
        except NoMoneyInBaseError:
          dialog = setMessage(message = "You can't sell so many " &
              getCurrentExceptionMsg() & " because " & trader &
                  " don't have that many " & moneyName &
              " to buy it.", title = "Too much items for sale")
        except NoTraderError:
          dialog = setMessage(message = "You don't have assigned anyone in the crew to the trader's duty.",
              title = "No trader assigned")
        except NoFreeSpaceError:
          dialog = setMessage(message = "The " & trader &
              " doesn't have free space in cargo to buy it.",
              title = "No space in the " &
              trader & "'s cargo")
        except:
          dialog = setError(message = "Can't trade item.")
      restoreButtonStyle()
      # Close button
      addCloseButton(dialog = dialog, icon = cancelIcon, color = redColor,
          label = "Cancel", isPopup = false)
  except:
    dialog = setError(message = "Can't show the info")

  windowSetFocus(name = manipulateData.title)

proc showInventoryItemInfo*(itemIndex: Natural; memberIndex: int;
    button1: ButtonSettings = emptyButtonSettings;
    button2: ButtonSettings = emptyButtonSettings): GameDialog {.raises: [
    KeyError], tags: [WriteIOEffect, TimeEffect, RootEffect], contractual.} =
  ## Show info about selected item in ship cargo or crew member inventory
  ##
  ## * itemIndex   - Index of item (can be inventory or ship cargo)
  ## * memberIndex - If item is in crew member inventory, crew index of member,
  ##                 otherwise 0
  ## * button1     - The settings for the first optional button. If empty, the
  ##                 button will not show. Default value is empty.
  ## * button2     - The setting for the second optional button. If empty,
  ##                 the button will not show. Default value is empty.
  ##
  ## Returns the infoDialog if the info was set, otherwise errorDialog
  var
    protoIndex: Natural = 0
    itemInfo, quality: string = ""
  if memberIndex > -1:
    protoIndex = playerShip.crew[memberIndex].inventory[itemIndex].protoIndex
    quality = $playerShip.crew[memberIndex].inventory[itemIndex].quality
    if playerShip.crew[memberIndex].inventory[itemIndex].durability < defaultItemDurability:
      itemInfo = getItemDamage(itemDurability = playerShip.crew[
          memberIndex].inventory[itemIndex].durability, withColors = true) & '\n'
  else:
    protoIndex = playerShip.cargo[itemIndex].protoIndex
    quality = $playerShip.cargo[itemIndex].quality
    if playerShip.cargo[itemIndex].durability < defaultItemDurability:
      itemInfo = getItemDamage(itemDurability = playerShip.cargo[
          itemIndex].durability, withColors = true) & '\n'
  itemInfo.add(y = "Weight: {gold}" & $itemsList[protoIndex].weight & " kg{/gold}")
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
  let itemTypes: array[6, string] = [weaponType, chestArmor, headArmor,
      armsArmor, legsArmor, shieldType]
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
  if itemsList[protoIndex].description.len > 0:
    itemInfo.add(y = "\n\n" & itemsList[protoIndex].description)
  return setInfo(text = itemInfo, title = (if memberIndex >
      -1: getItemName(item = playerShip.crew[memberIndex].inventory[
      itemIndex], damageInfo = false, toLower = false) else: getItemName(
      item = playerShip.cargo[itemIndex], damageInfo = false,
      toLower = false)), button1 = button1, button2 = button2)

proc updateTimer*(timeDiff: float) {.raises: [], tags: [], contractual.} =
  ## Update the dialogs' timer
  ##
  ## * timeDiff - the amount of time passed
  timer -= timeDiff
  if timer < 0.0:
    timer = 0.0
