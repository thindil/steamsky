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

## Provides code related to the information about the player's ship's cargo,
## like shoing its list, moving to crew members' inventory, dropping items
## from it, etc.

import std/[algorithm, strutils, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, game, items, types]
import coreui, dialogs, errordialog, setui, table, themes

type CargoSortOrders = enum
  nameAsc, nameDesc, durabilityAsc, durabilityDesc, qualityAsc, qualityDesc,
    typeAsc, typeDesc, amountAsc, amountDesc, weightAsc, weightDesc, none

const defaultCargoSortOrder: CargoSortOrders = none

var
  showCargoOptions*: bool = false
    ## Show additonal options for managing the player's ship's cargo
  cargoSortOrder: CargoSortOrders = defaultCargoSortOrder
  typeIndex: Natural = 0
  itemIndex: int = -1

proc sortCargo(sortAsc, sortDesc: CargoSortOrders;
    dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Sort the items on the list
  ##
  ## * sortAsc  - the sorting value for ascending sort
  ## * sortDesc - the sorting value for descending sort
  ## * dialog   - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  if cargoSortOrder == sortAsc:
    cargoSortOrder = sortDesc
  else:
    cargoSortOrder = sortAsc
  type LocalCargoData = object
    name: string
    damage: float
    itemType: string
    amount: Positive = 1
    weight: Positive = 1
    quality: ObjectQuality
    id: Natural
  var localCargo: seq[LocalCargoData] = @[]
  for index, item in playerShip.cargo:
    try:
      localCargo.add(y = LocalCargoData(name: getItemName(item = item,
          damageInfo = false, toLower = false), damage: (item.durability.float /
          defaultItemDurability.float), itemType: (if itemsList[
          item.protoIndex].showType.len > 0: itemsList[
          item.protoIndex].showType else: itemsList[item.protoIndex].itemType),
          amount: item.amount, weight: item.amount * itemsList[
          item.protoIndex].weight, quality: item.quality, id: index))
    except:
      dialog = setError(message = "Can't add local item to cargo.")
      return

  proc sortCargo(x, y: LocalCargoData): int {.raises: [], tags: [],
      contractual.} =
    ## Compare two items and return which should go first, based on the sort
    ## order of the items
    ##
    ## * x - the first item to compare
    ## * y - the second item to compare
    ##
    ## Returns 1 if the first item should go first, -1 if the second item
    ## should go first.
    case cargoSortOrder
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
    of qualityAsc:
      if x.quality < y.quality:
        return 1
      return -1
    of qualityDesc:
      if x.quality > y.quality:
        return 1
      return -1
    of typeAsc:
      if x.itemType < y.itemType:
        return 1
      return -1
    of typeDesc:
      if x.itemType > y.itemType:
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
    of none:
      return -1

  localCargo.sort(cmp = sortCargo)
  itemsIndexes = @[]
  for item in localCargo:
    itemsIndexes.add(y = item.id)

proc showGiveDialog*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Show the dialog to give items from the player's ship's cargo to crew
  ## members
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  const
    width: float = 400
    height: float = 400

  let
    item: InventoryData = playerShip.cargo[itemIndex]
    windowName: string = "Give " & getItemName(item = item) & " to a crew member"
  updateDialog(width = width, height = height)
  window(name = windowName, x = dialogX, y = dialogY, w = width, h = height,
      flags = {windowBorder, windowTitle, windowMovable, windowNoScrollbar}):
    setLayoutRowDynamic(height = 35, cols = 2)
    # Close button
    addCloseButton(dialog = dialog, icon = cancelIcon, color = redColor,
        isPopup = false, label = "Cancel")

  windowSetFocus(name = windowName)

proc setGiveDialog(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the dialog for give items from the player's ship's cargo to crew
  ## members
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  closePopup()
  dialog = giveDialog

proc setDropDialog(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the dialog for drop items from the player's ship's cargo
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  closePopup()

proc showItemInfo(data: int; dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the selected item information
  ##
  ## * data   - the index of the selected item
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  itemIndex = itemsIndexes[data]
  try:
    dialog = showInventoryItemInfo(itemIndex = data, memberIndex = -1,
        button1 = ButtonSettings(
        tooltip: "Give item to a crew member",
        code: setGiveDialog, icon: giveIcon.ord, text: "Give", color: ""),
        button2 = ButtonSettings(
        tooltip: "Drop item from the ship cargo",
        code: setDropDialog, icon: dropIcon.ord, text: "Drop", color: ""))
  except:
    dialog = setError(message = "Can't show the item's info.")

const
  headers: array[6, HeaderData[CargoSortOrders]] = [
    HeaderData[CargoSortOrders](label: "Name", sortAsc: nameAsc,
        sortDesc: nameDesc),
    HeaderData[CargoSortOrders](label: "Durability", sortAsc: durabilityAsc,
        sortDesc: durabilityDesc),
    HeaderData[CargoSortOrders](label: "Quality", sortAsc: qualityAsc,
        sortDesc: qualityDesc),
    HeaderData[CargoSortOrders](label: "Type", sortAsc: typeAsc,
        sortDesc: typeDesc),
    HeaderData[CargoSortOrders](label: "Amount", sortAsc: amountAsc,
        sortDesc: amountDesc),
    HeaderData[CargoSortOrders](label: "Weight", sortAsc: weightAsc,
        sortDesc: weightDesc)]
  ratio: array[6, cfloat] = [300.cfloat, 200, 200, 200, 200, 200]

proc showCargoInfo*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Show the list of the player's ship's cargo
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  # Show information about free cargo space in the player's ship
  setLayoutRowStatic(height = 30, cols = 2, ratio = cargoWidth)
  label(str = cargoText[0])
  colorLabel(str = cargoText[1], color = theme.colors[goldenColor])
  if showCargoOptions:
    setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.2.cfloat, 0.6])
    label(str = "Type:")
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Show only items with the selected type")
    let newType: Natural = comboList(items = typesList, selected = typeIndex,
        itemHeight = 25, x = 200, y = 150)
    if newType != typeIndex:
      typeIndex = newType
  # Show the list of crew members
  addHeader(headers = headers, ratio = ratio, tooltip = "cargo",
      code = sortCargo, dialog = dialog)
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
  for index in itemsIndexes:
    if currentRow < startRow:
      currentRow.inc
      continue
    let
      item: InventoryData = playerShip.cargo[index]
      protoItem: ObjectData = try:
          itemsList[item.protoIndex]
        except:
          dialog = setError(message = "Can't get the proto item.")
          return
      itemType: string = (if protoItem.showType.len >
          0: protoItem.showType else: protoItem.itemType)
    if typesList[typeIndex] != "All" and itemType != typesList[typeIndex]:
      continue
    addButton(label = getItemName(item = item),
        tooltip = "Show item's description and actions", data = index,
        code = showItemInfo, dialog = dialog)
    addProgressBar(tooltip = "The current durability of the selected item",
        value = item.durability, maxValue = defaultItemDurability, data = index,
        code = showItemInfo, dialog = dialog)
    addButton(label = ($item.quality).capitalizeAscii,
        tooltip = "The quality of the selected item", data = index,
        code = showItemInfo, dialog = dialog)
    addButton(label = itemType, tooltip = "The type of the selected item",
        data = index, code = showItemInfo, dialog = dialog)
    addButton(label = $item.amount, tooltip = "The amount of the selected item",
        data = index, code = showItemInfo, dialog = dialog)
    addButton(label = $(item.amount * protoItem.weight) & " kg",
        tooltip = "The total weight of the selected item", data = index,
        code = showItemInfo, dialog = dialog)
    row.inc
    if row == gameSettings.listsLimit + 1:
      break
  restoreButtonStyle()
  addPagination(page = currentPage, row = row)
