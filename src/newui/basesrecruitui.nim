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

## Provides code related to recruit new crew members in bases, like show the
## UI, start negotiating, show information about a recruit, etc.

import contracts, nuklear/nuklear_sdl_renderer
import ../config
import coreui, header, messagesui, table

type RecruitsSortOrders = enum
  none, nameAsc, nameDesc, genderAsc, genderDesc, factionAsc, factionDesc,
    priceAsc, priceDesc, attributeAsc, attributeDesc, skillAsc, skillDesc

const
  defaultRecruitsSortOrder: RecruitsSortOrders = none
  headers: array[6, HeaderData[RecruitsSortOrders]] = [
    HeaderData[RecruitsSortOrders](label: "Name", sortAsc: nameAsc,
        sortDesc: nameDesc),
    HeaderData[RecruitsSortOrders](label: "Gender", sortAsc: genderAsc,
        sortDesc: genderDesc),
    HeaderData[RecruitsSortOrders](label: "Faction", sortAsc: factionAsc,
        sortDesc: factionDesc),
    HeaderData[RecruitsSortOrders](label: "Base cost", sortAsc: priceAsc,
        sortDesc: priceDesc),
    HeaderData[RecruitsSortOrders](label: "Highest stat", sortAsc: attributeAsc,
        sortDesc: attributeDesc),
    HeaderData[RecruitsSortOrders](label: "Highest skill", sortAsc: skillAsc,
        sortDesc: skillDesc)]

proc showRecruits*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the school UI
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  if showHeader(dialog = dialog, close = CloseDestination.map, state = state):
    return
  let tableHeight: float = windowHeight - gameSettings.messagesPosition.float - 20
  setLayoutRowDynamic(height = tableHeight, cols = 1)
  group(title = "RecruitsGroup", flags = {windowNoFlags}):
    discard
  showLastMessages(theme = theme, dialog = dialog, height = windowHeight - tableHeight)
  showGameMenu(dialog = dialog)
