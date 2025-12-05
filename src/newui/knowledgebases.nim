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

## Provides code related to the information about the player's knowledge, like
## lists of known bases, events missions, finished stories, etc.

import contracts, nuklear/nuklear_sdl_renderer
import coreui, setui, table

type BasesSortOrders = enum
  none, nameAsc, nameDesc, distanceAsc, distanceDesc, populationAsc,
    populationDesc, sizeAsc, sizeDesc, ownerAsc, ownerDesc, typeAsc, typeDesc,
    reputationAsc, reputationDesc, coordAsc, coordDesc

const
  defaultBasesSortOrder: BasesSortOrders = none

var
  showBasesOptions*: bool = false
    ## Show additonal options for managing the list of known bases
  basesType, basesStatus, basesOwner: Natural = 0
  basesSortOrder: BasesSortOrders = defaultBasesSortOrder

proc sortBases(sortAsc, sortDesc: BasesSortOrders;
    dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Sort recipes on the list
  ##
  ## * sortAsc  - the sorting value for ascending sort
  ## * sortDesc - the sorting value for descending sort
  ## * dialog   - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  if basesSortOrder == sortAsc:
    basesSortOrder = sortDesc
  else:
    basesSortOrder = sortAsc

proc showBasesInfo*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Show the list of the known bases
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  # Show options related to managing the list
  if showBasesOptions:
    setLayoutRowStatic(height = 25, cols = 6, ratio = [50.cfloat, 150, 75,
        150, 75, 150])
    label(str = "Type:")
    basesType = comboList(items = basesTList, selected = basesType,
        itemHeight = 25, x = 150, y = 150)
    label(str = "Status:")
    basesStatus = comboList(items = basesStatuses, selected = basesStatus,
        itemHeight = 25, x = 150, y = 150)
    label(str = "Owner:")
    basesOwner = comboList(items = basesOwners, selected = basesOwner,
        itemHeight = 25, x = 150, y = 150)
  const
    headers: array[8, HeaderData[BasesSortOrders]] = [
      HeaderData[BasesSortOrders](label: "Name", sortAsc: nameAsc,
          sortDesc: nameDesc),
      HeaderData[BasesSortOrders](label: "Distance",
          sortAsc: distanceAsc, sortDesc: distanceDesc),
      HeaderData[BasesSortOrders](label: "Coordinates", sortAsc: coordAsc,
          sortDesc: coordDesc),
      HeaderData[BasesSortOrders](label: "Population",
          sortAsc: populationAsc, sortDesc: populationDesc),
      HeaderData[BasesSortOrders](label: "Size",
          sortAsc: sizeAsc, sortDesc: sizeDesc),
      HeaderData[BasesSortOrders](label: "Owner",
          sortAsc: ownerAsc, sortDesc: ownerDesc),
      HeaderData[BasesSortOrders](label: "Type",
          sortAsc: typeAsc, sortDesc: typeDesc),
      HeaderData[BasesSortOrders](label: "Reputation",
          sortAsc: reputationAsc, sortDesc: reputationDesc)]
    ratio: array[8, cfloat] = [200.cfloat, 100, 100, 100, 100, 100, 100, 100]


  addHeader(headers = headers, ratio = ratio, tooltip = "bases",
      code = sortBases, dialog = dialog)
