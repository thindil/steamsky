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

## Provides code related to the information about the list of known bases, like
## sorting them, showing information about them, etc.

import contracts
import coreui, table


type EventsSortOrders = enum
  none, typeAsc, typeDesc, distanceAsc, distanceDesc, detailsAsc, detailsDesc,
    coordAsc, coordDesc

const defaultEventsSortOrder: EventsSortOrders = none

var eventsSortOrder: EventsSortOrders = defaultEventsSortOrder

proc sortEvents(sortAsc, sortDesc: EventsSortOrders;
    dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Sort events on the list
  ##
  ## * sortAsc  - the sorting value for ascending sort
  ## * sortDesc - the sorting value for descending sort
  ## * dialog   - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  if eventsSortOrder == sortAsc:
    eventsSortOrder = sortDesc
  else:
    eventsSortOrder = sortAsc

proc showEventsInfo*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Show the list of the known events
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  const
    headers: array[4, HeaderData[EventsSortOrders]] = [
      HeaderData[EventsSortOrders](label: "Name", sortAsc: typeAsc,
          sortDesc: typeDesc),
      HeaderData[EventsSortOrders](label: "Distance",
          sortAsc: distanceAsc, sortDesc: distanceDesc),
      HeaderData[EventsSortOrders](label: "Coordinates", sortAsc: coordAsc,
          sortDesc: coordDesc),
      HeaderData[EventsSortOrders](label: "Details",
          sortAsc: detailsAsc, sortDesc: detailsDesc)]
    ratio: array[4, cfloat] = [200.cfloat, 100, 100, 100]

  addHeader(headers = headers, ratio = ratio, tooltip = "events",
      code = sortEvents, dialog = dialog)
