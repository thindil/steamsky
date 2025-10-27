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

import contracts, nuklear/nuklear_sdl_renderer
import ../config
import coreui, errordialog, setui, table, themes

type CargoSortOrders = enum
  nameAsc, nameDesc, durabilityAsc, durabilityDesc, qualityAsc, qualityDesc,
    typeAsc, typeDesc, amountAsc, amountDesc, weightAsc, weightDesc, none

const defaultCargoSortOrder: CargoSortOrders = none

var
  showCargoOptions*: bool = false
    ## Show additonal options for managing the player's ship's cargo
  cargoSortOrder: CargoSortOrders = defaultCargoSortOrder

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
    setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.2.cfloat, 0.4])
    label(str = "Type:")
    label(str = "temp")
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
  restoreButtonStyle()
  addPagination(page = currentPage, row = row)
