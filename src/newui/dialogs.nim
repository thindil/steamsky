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

import std/[colors, os, math, strutils, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, crewinventory, game, game2, maps, messages, shipscargo,
    shipscrew, shipscrew2, types, trades]
import coreui, errordialog, themes

type
  QuestionType* = enum
    ## Types of questions, used to set actions to the player's response
    deleteSave, showDeadStats, quitGame, resignGame, homeBase, finishGame
  QuestionData = object
    question, data: string
    qType: QuestionType
    lines: float
  MessageData = object
    text, title: string
    lines: float
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
    sellAction, buyAction
  ManipulateData = object
    itemIndex: int
    maxAmount: Natural
    cost: Natural
    title: string

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
    var needLines: float = ceil(x = getTextWidth(text = question) / 250)
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
    messageData = MessageData(text: message, title: title, lines: needLines)
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
      width: float = 250
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
        windowNoScrollbar}):
      setLayoutRowDynamic(height = 30 * questionData.lines, cols = 1)
      wrapLabel(str = questionData.question)
      setLayoutRowDynamic(height = 30, cols = 2)
      labelButton(title = "Yes"):
        closePopup()
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
          let moneyIndex2: int = findItem(inventory = playerShip.cargo,
              protoIndex = moneyIndex)
          if moneyIndex2 == -1:
            dialog = setMessage(message = "You don't have any " & moneyName &
                " for change ship home base.", title = "No money")
            return
          let price: Natural = questionData.data.parseInt
          if playerShip.cargo[moneyIndex2].amount < price:
            dialog = setMessage(message = "You don't have enough " & moneyName &
                " for change ship home base.", title = "No money")
            return
          playerShip.homeBase = skyMap[playerShip.skyX][
              playerShip.skyY].baseIndex
          updateCargo(ship = playerShip, cargoIndex = moneyIndex2,
              amount = -price)
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

proc addCloseButton(dialog: var GameDialog) {.raises: [], tags: [],
    contractual.} =
  ## Add the close button to the dialog
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the parameter dialog. It is modified only when the player closed
  ## the dialog.
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(),
        text = "Close the dialog [Escape key]")
  labelButton(title = "Close"):
    closePopup()
    dialog = none

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
  try:
    const
      width: float = 250
      height: float = 150

    updateDialog(width = width, height = height)
    popup(pType = staticPopup, title = messageData.title, x = dialogX,
        y = dialogY, w = width, h = height, flags = {windowBorder, windowTitle,
        windowNoScrollbar}):
      setLayoutRowDynamic(height = 30 * messageData.lines, cols = 1)
      wrapLabel(str = messageData.text)
      setLayoutRowDynamic(height = 30, cols = 1)
      addCloseButton(dialog = dialog)
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
        windowTitle, windowNoScrollbar}):
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

proc setManipulate*(action: ManipulateType; iIndex: int): GameDialog {.raises: [
    ], tags: [RootEffect], contractual.} =
  ## Set the data related to the current in-game manipulate item dialog
  ##
  ## * action - the action used to manipulate items, like selling or buying
  ## * iIndex - the index of the item to manipulate, in the player's ship's
  ##            cargo (if positive) or in a trader's cargo (if negative)
  ##
  ## Returns the type of dialog if the dialog was set, otherwise errorDialog
  setDialog(x = windowWidth / 5.0)
  let (protoIndex, maxSellAmount, maxBuyAmount, price) = try:
      getTradeData(iIndex = iIndex)
    except:
      return setError(message = "Can't get the trade's data.")
  try:
    manipulateData = ManipulateData(itemIndex: iIndex, maxAmount: (if action ==
        buyAction: maxBuyAmount else: maxSellAmount), cost: price, title: (
        if action == buyAction: "Buy " else: "Sell ") & itemsList[
            protoIndex].name)
  except:
    return setError(message = "Can't set the manipulate data.")
  if action == buyAction:
    return buyDialog
  else:
    return sellDialog

proc showManipulateItem*(dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the dialog to manipulate the selected item(s) to the player
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the parameter dialog. It is modified only when the player closed
  ## the dialog.
  if dialog notin {buyDialog, sellDialog}:
    return
  try:
    let
      width = windowWidth / 1.5
      height: float = 150
    updateDialog(width = width, height = height)
    popup(pType = staticPopup, title = manipulateData.title, x = dialogX,
        y = dialogY, w = width, h = height, flags = {windowBorder, windowTitle,
        windowNoScrollbar}):
      setLayoutRowDynamic(height = 30, cols = 1)
      # Draw close button
      addCloseButton(dialog = dialog)
  except:
    dialog = setError(message = "Can't show the info")
