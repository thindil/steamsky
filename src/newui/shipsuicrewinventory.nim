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

import std/tables
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, crewinventory, game, items, types]
import coreui, dialogs, errordialog, setui, shipsuicrew, table, themes

type
  InventorySortOrders = enum
    selectedAsc, selectedDesc, nameAsc, nameDesc, durabilityAsc, durabilityDesc,
      usedAsc, usedDesc, amountAsc, amountDesc, weightAsc, weightDesc, none
  LocalItemData = object
    selected: bool = false
    name: string = ""
    damage: float = 0.0
    itemType: string = ""
    amount: Positive = 1
    weight: Positive = 1
    used: bool = false
    id: Natural = 0

const defaultInventorySortOrder*: InventorySortOrders = none

var inventorySortOrder: InventorySortOrders = defaultInventorySortOrder

proc sortItems(x, y: LocalItemData): int {.raises: [], tags: [],
    contractual.} =
  ## Compare two items and return which should go first, based on the sort
  ## order of the items
  ##
  ## * x - the first item to compare
  ## * y - the second item to compare
  ##
  ## Returns 1 if the first item should go first, -1 if the second item
  ## should go first.
  case inventorySortOrder
  of selectedAsc:
    if x.selected < y.selected:
      return 1
    return -1
  of selectedDesc:
    if x.selected > y.selected:
      return 1
    return -1
  of nameAsc:
    if x.name < y.name:
      return 1
    return -1
  of nameDesc:
    if x.name > y.name:
      return 1
    return -1
  of durabilityAsc:
    if x.damage < y.damage:
      return 1
    return -1
  of durabilityDesc:
    if x.damage > y.damage:
      return 1
    return -1
  of amountAsc:
    if x.amount < y.amount:
      return 1
    return -1
  of amountDesc:
    if x.amount > y.amount:
      return 1
    return -1
  of weightAsc:
    if x.weight < y.weight:
      return 1
    return -1
  of weightDesc:
    if x.weight > y.weight:
      return 1
    return -1
  of usedAsc:
    if x.used < y.used:
      return 1
    return -1
  of usedDesc:
    if x.used > y.used:
      return 1
    return -1
  of none:
    return -1

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
  var localInventory: seq[LocalItemData] = @[]
  for index, data in inventoryDataList:
    try:
      let item: InventoryData = playerShip.crew[crewIndex].inventory[data.index]
      localInventory.add(y = LocalItemData(selected: data.checked,
          name: getItemName(item = item, damageInfo = false, toLower = false),
          damage: item.durability.float / defaultItemDurability.float,
          itemType: (if itemsList[item.protoIndex].showType.len > 0: itemsList[
          item.protoIndex].showType else: itemsList[item.protoIndex].itemType),
          amount: item.amount, weight: item.amount * itemsList[
          item.protoIndex].weight, used: itemIsUsed(memberIndex = crewIndex,
          itemIndex = data.index), id: data.index))
    except:
      dialog = setError(message = "Can't add item to local inventory.")

proc setItemInfo(data: int; dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Set the dialog with information about the selected item
  ##
  ## * data   - the index of the selected item
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  discard

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
  ratio: array[6, cfloat] = [40.cfloat, 300, 200, 40, 200, 200]

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
    setLayoutRowDynamic(height = height - 170, cols = 1)
    group(title = "InfoGroup", flags = {windowNoFlags}):
      addHeader(headers = headers, ratio = ratio, tooltip = "items",
          code = sortInventory, dialog = dialog)
      var currentRow: Positive = 1
      saveButtonStyle()
      setButtonStyle(field = borderColor, a = 0)
      try:
        setButtonStyle(field = normal, color = theme.colors[tableRowColor])
        setButtonStyle(field = textNormal, color = theme.colors[tableTextColor])
      except:
        dialog = setError(message = "Can't set table color")
        return
      setButtonStyle(field = rounding, value = 0)
      setButtonStyle(field = border, value = 0)
      let startRow: Positive = ((currentPage - 1) * gameSettings.listsLimit) + 1
      var row: Positive = 1
      for index, data in inventoryDataList.mpairs:
        if currentRow < startRow:
          currentRow.inc
          continue
        addCheckButton(tooltip = "Select the item to move or equip it.",
            checked = data.checked)
        addButton(label = getItemName(item = member.inventory[data.index],
            damageInfo = false, toLower = false),
            tooltip = "Show the selected item's info.", data = data.index,
            code = setItemInfo, dialog = dialog)
        addProgressBar(tooltip = "The current durability level of the selected item.",
            value = member.inventory[data.index].durability,
            maxValue = defaultItemDurability, data = data.index,
            code = setItemInfo, dialog = dialog)
        var
          checked: bool = false
          tooltip: string = ""
        if itemIsUsed(memberIndex = crewIndex, itemIndex = data.index):
          checked = true
          tooltip = "The item is used by the crew member"
        else:
          checked = false
          tooltip = "The item isn't used by the crew member"
        addCheckButton(tooltip = tooltip, checked = checked)
        addButton(label = $member.inventory[data.index].amount,
            tooltip = "The amount of the item owned by the crew member.",
            data = data.index, code = setItemInfo, dialog = dialog)
        try:
          addButton(label = $(member.inventory[data.index].amount * itemsList[
              member.inventory[data.index].protoIndex].weight) & " kg",
              tooltip = "The total weight of the items", data = data.index,
              code = setItemInfo, dialog = dialog)
        except:
          dialog = setError(message = "Can't count the total weight of the item.")
          return
        row.inc
        if row == gameSettings.listsLimit + 1:
          break
    restoreButtonStyle()
    setLayoutRowDynamic(height = 30, cols = 1)
    addCloseButton(dialog = dialog, isPopup = false)

  windowSetFocus(name = windowName)
