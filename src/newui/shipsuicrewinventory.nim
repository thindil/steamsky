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
# along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

## Provides code related to the information about the player's ship's crew
## members's inventory, like listing them, showing information, move items, etc.

import contracts, nuklear/nuklear_sdl_renderer
import ../[config, game, types]
import coreui, dialogs, setui, shipsuicrew, table, themes

type
  InventorySortOrders = enum
    selectedAsc, selectedDesc, nameAsc, nameDesc, durabilityAsc, durabilityDesc,
      usedAsc, usedDesc, amountAsc, amountDesc, weightAsc, weightDesc, none

const defaultInventorySortOrder*: InventorySortOrders = none

var  inventorySortOrder: InventorySortOrders = defaultInventorySortOrder

proc sortInventory(sortAsc, sortDesc: InventorySortOrders;
    dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Sort items on the trades list
  ##
  ## * sortAsc  - the sorting value for ascending sort
  ## * sortDesc - the sorting value for descending sort
  ## * dialog   - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  if inventorySortOrder == sortAsc:
    inventorySortOrder = sortDesc
  else:
    inventorySortOrder = sortAsc

const
  headers: array[6, HeaderData[InventorySortOrders]] = [
    HeaderData[InventorySortOrders](label: "", sortAsc: selectedAsc,
        sortDesc: selectedDesc),
    HeaderData[InventorySortOrders](label: "Name", sortAsc: nameAsc,
        sortDesc: nameDesc),
    HeaderData[InventorySortOrders](label: "Durability", sortAsc: durabilityAsc,
        sortDesc: durabilityDesc),
    HeaderData[InventorySortOrders](label: "Used", sortAsc: usedAsc,
        sortDesc: usedDesc),
    HeaderData[InventorySortOrders](label: "Amount", sortAsc: amountAsc,
        sortDesc: amountDesc),
    HeaderData[InventorySortOrders](label: "Weight", sortAsc: weightAsc,
        sortDesc: weightDesc)]
  ratio: array[6, cfloat] = [40.cfloat, 300, 200, 200, 200, 200]

proc showMemberInventory*(dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the dialog with information about inventory of the selected crew member
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog.
  const
    width: float = 600
    height: float = 500

  let
    member: MemberData = playerShip.crew[crewIndex]
    windowName: string = "Inventory of " & member.name
  updateDialog(width = width, height = height)
  window(name = windowName, x = dialogX, y = dialogY, w = width, h = height,
      flags = {windowBorder, windowTitle, windowNoScrollbar, windowMovable}):
    ## Show information about free inventory space
    setLayoutRowStatic(height = 30, cols = 2, ratio = spaceWidth)
    label(str = spaceText[0])
    colorLabel(str = spaceText[1], color = theme.colors[goldenColor])
    ## Show select/unselect all items buttons
    setLayoutRowStatic(height = 35, cols = 2, width = 35)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(), text = "Select all items")
    imageButton(image = images[selectAllIcon]):
      for data in inventoryDataList.mitems:
        data.checked = true
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(), text = "Unselect all items")
    imageButton(image = images[unselectAllIcon]):
      for data in inventoryDataList.mitems:
        data.checked = false
    # Show the list of items in inventory
    setLayoutRowDynamic(height = height - 200, cols = 1)
    group(title = "InfoGroup", flags = {windowNoFlags}):
      addHeader(headers = headers, ratio = ratio, tooltip = "items",
          code = sortInventory, dialog = dialog)
    setLayoutRowDynamic(height = 30, cols = 1)
    addCloseButton(dialog = dialog, isPopup = false)

  windowSetFocus(name = windowName)
