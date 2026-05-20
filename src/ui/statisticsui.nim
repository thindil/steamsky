# Copyright 2026 Bartek thindil Jasicki
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

## Provides code related to the game's statistics ui, like showing them, etc.

import contracts, nuklear/nuklear_sdl_renderer
import ../config
import coreui, goalsui, header, messagesui, setui, themes

proc showStatistics*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the screen with information about the current game's statistics
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  if showHeader(dialog = dialog, close = previous, state = state):
    return
  let height: float = (windowHeight - 35 - gameSettings.messagesPosition.float)
  setLayoutRowDynamic(height = height, cols = 2)

  proc addStatistic(title, value, tooltip: string) {.raises: [], tags: [],
      contractual.} =
    ## Add elements with information about the selected statistic
    ##
    ## * title   - the title of the statistic to show
    ## * value   - the value of the statistic to show
    ## * tooltip - the tooltip for the statistic
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(), text = tooltip)
    label(str = title)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(), text = tooltip)
    colorLabel(str = value, color = theme.colors[goldenColor])

  group(title = "Group1", flags = {}):

    type
      StatsData = object
        title, tooltip: string

    const statsData: array[6, StatsData] = [StatsData(title: "Points:",
        tooltip: "The amount of points gained in this game"), StatsData(
        title: "Time passed:",
        tooltip: "In game time which was passed since it started"), StatsData(
        title: "Bases visited:",
        tooltip: "The amount of sky bases visited and total percentage of all bases"),
        StatsData(title: "Map discovered:",
        tooltip: "The amount of unique map's fields visited"), StatsData(
        title: "Distance traveled:",
        tooltip: "The total amount of map's fields visited"), StatsData(
        title: "Crafting orders finished:",
        tooltip: "The total amount of crafting orders finished in this game")]

    setLayoutRowDynamic(height = 25, cols = 2, ratio = [0.6.cfloat, 0.4])
    for index, stat in statsData:
      addStatistic(title = stat.title, value = statisticsValues[index],
          tooltip = stat.tooltip)
    # Show crafting table
    if finishedCrafts.len > 0:
      setLayoutRowDynamic(height = 25, cols = 2, ratio = [0.7.cfloat, 0.3])
      colorLabel(str = "Name", color = theme.colors[goldenColor])
      colorLabel(str = "Amount", color = theme.colors[goldenColor])
      for craft in finishedCrafts:
        label(str = craft.name)
        label(str = $craft.amount)
    # Show completed missions statistics
    setLayoutRowDynamic(height = 25, cols = 2, ratio = [0.6.cfloat, 0.4])
    addStatistic(title = "Missions completed:", value = statisticsValues[6],
        tooltip = "The total amount of missions finished in this game")
    if finishedMissions.len > 0:
      setLayoutRowDynamic(height = 25, cols = 2, ratio = [0.7.cfloat, 0.3])
      colorLabel(str = "Name", color = theme.colors[goldenColor])
      colorLabel(str = "Amount", color = theme.colors[goldenColor])
      for mission in finishedMissions:
        label(str = mission.name)
        label(str = $mission.amount)
    # Show goals's statistics
    setLayoutRowDynamic(height = 30, cols = 1)
    labelButton(title = selectedGoal):
      dialog = newGoalDialog
      setSelectedGoal()
    setLayoutRowDynamic(height = 25, cols = 2, ratio = [0.6.cfloat, 0.4])
    addStatistic(title = "Finished goals:", value = statisticsValues[7],
        tooltip = "The total amount of goals finished in this game")
    if finishedGoals.len > 0:
      setLayoutRowDynamic(height = 25, cols = 2, ratio = [0.7.cfloat, 0.3])
      colorLabel(str = "Name", color = theme.colors[goldenColor])
      colorLabel(str = "Amount", color = theme.colors[goldenColor])
      for goal in finishedGoals:
        label(str = goal.name)
        label(str = $goal.amount)
  group(title = "Group2", flags = {}):
    # Show destroyed ships statistics
    setLayoutRowDynamic(height = 25, cols = 2, ratio = [0.6.cfloat, 0.4])
    addStatistic(title = "Destroyed ships:", value = statisticsValues[8],
        tooltip = "The total amount of destroyed ships in this game")
    if destroyedShips.len > 0:
      setLayoutRowDynamic(height = 25, cols = 2, ratio = [0.7.cfloat, 0.3])
      colorLabel(str = "Name", color = theme.colors[goldenColor])
      colorLabel(str = "Amount", color = theme.colors[goldenColor])
      for ship in destroyedShips:
        label(str = ship.name)
        label(str = $ship.amount)
    # Show killed mobs statistics
    setLayoutRowDynamic(height = 25, cols = 2, ratio = [0.6.cfloat, 0.4])
    addStatistic(title = "Killed enemies:", value = statisticsValues[9],
        tooltip = "The total amount of enemies killed in melee combat in this game")
    if destroyedShips.len > 0:
      setLayoutRowDynamic(height = 25, cols = 2, ratio = [0.7.cfloat, 0.3])
      colorLabel(str = "Name", color = theme.colors[goldenColor])
      colorLabel(str = "Amount", color = theme.colors[goldenColor])
      for mob in killedMobs:
        label(str = mob.name)
        label(str = $mob.amount)
  showLastMessages(theme = theme, dialog = dialog, height = windowHeight -
      height - 75)
