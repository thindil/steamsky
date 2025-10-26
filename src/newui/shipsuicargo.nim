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
import coreui, errordialog, table, themes

type CargoSortOrders = enum
  nameAsc, nameDesc, durabilityAsc, durabilityDesc, qualityAsc, qualityDesc,
    typeAsc, typeDesc, amountAsc, amountDesc, weightAsc, weightDesc, none

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
  discard
