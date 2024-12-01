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

## Provides code related to the selecting the player's goal dialog, like
## showing the dialog, and selecting it.

import std/[strutils, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, game, goals]
import coreui, errordialog

var
  selectedGoal*: string = "Random" ## Currently selected goal
  selected: int = -1
  goalsUiList: Table[string, seq[string]] = initTable[string, seq[string]]()

proc setGoalsUi*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the goals UI, like types of goals, etc.
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog.
  for goal in goalsList.values:
    try:
      let key: string = ($goal.goalType).toUpperAscii
      if not goalsUiList.hasKey(key = key):
        goalsUiList[key] = @[]
      goalsUiList[key].add(y = goalText(index = goal.index.parseInt))
    except:
      dialog = setError(message = "Can't set the list of goals")
      return

proc setSelectedGoal*() {.raises: [], tags: [], contractual.} =
  ## Set the selection in the list of available goals
  selected = -1

proc showGoals*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Show the dialog with the list of available goals for players
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog.
  window(name = "Select a new goal", x = 40, y = 20, w = 540, h = 360, flags = {
      windowBorder, windowMoveable, windowTitle, windowMinimizable,
      windowNoScrollbar}):
    setLayoutRowDynamic(height = 230, cols = 1)
    group(title = "GoalsGroup", flags = {windowNoFlags}):
      setLayoutRowDynamic(height = 25, cols = 1)

      proc addSelectable(label: string; num: Natural) {.raises: [], tags: [],
          contractual.} =
        ## Add a selectable goal to the list
        ##
        ## * label - the goal text to add
        var sel: bool = selected == num
        if selectableLabel(str = label, value = sel):
          if sel:
            selected = num
            selectedGoal = label
          else:
            selected = -1

      var index: Natural = 0
      addSelectable(label = "Random", num = index)
      try:
        const categories: OrderedTable[string, string] = {
          "REPUTATION": "Gain max reputation in bases",
            "DESTROY": "Destroy enemy ships", "DISCOVER": "Discover map",
            "VISIT": "Visit bases", "CRAFT": "Craft items",
            "MISSION": "Finish missions",
            "KILL": "Kill enemies in melee combat"}.toOrderedTable
        var catIndex: Positive = 1
        for catName, category in categories:
          treeNode(title = category, state = minimized, index = catIndex):
            for goal in goalsUiList[catName]:
              index.inc
              addSelectable(label = goal, num = index)
          catIndex.inc
      except:
        dialog = setError(message = "Can't show a goal.")
        return
    setLayoutRowDynamic(height = 35, cols = 1)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Select the goal for your character from the list. If you choose Random option, a random goal will be assigned. You can always change it later during the game, but you will lose all progress then.")
    if selected == -1:
      saveButtonStyle()
      setButtonStyle(normal, 40, 40, 40)
      setButtonStyle(hover, 40, 40, 40)
      setButtonStyle(active, 40, 40, 40)
      setButtonStyle(borderColor, 60, 60, 60)
      setButtonStyle(textBackground, 60, 60, 60)
      setButtonStyle(textNormal, 60, 60, 60)
      setButtonStyle(textHover, 60, 60, 60)
      setButtonStyle(textActive, 60, 60, 60)
      labelButton("Select goal"):
        discard
      restoreButtonStyle()
    else:
      labelButton(title = "Select goal"):
        dialog = none
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Close the goals list without any changes")
    labelButton(title = "Close"):
      dialog = none
