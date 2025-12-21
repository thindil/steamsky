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

import std/[algorithm, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, game, maps, messages, types]
import coreui, dialogs, errordialog, mapsui, setui, table, themes

type EventsSortOrders = enum
  none, typeAsc, typeDesc, distanceAsc, distanceDesc, detailsAsc, detailsDesc,
    coordAsc, coordDesc

const defaultEventsSortOrder: EventsSortOrders = none

var eventsSortOrder: EventsSortOrders = defaultEventsSortOrder

proc setTargetEvent(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the event as the target for the player's ship
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  closePopup()
  let event = eventsList[eventIndex]
  if event.skyX == playerShip.skyX and event.skyY == playerShip.skyY:
    dialog = setMessage(message = "You are at this location now.",
        title = "Can't set destination")
    return
  playerShip.destinationX = event.skyX
  playerShip.destinationY = event.skyY
  addMessage(message = "You set the travel destination for your ship.",
      mType = orderMessage)

proc showEvent(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Show the event on the map
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  closePopup()
  let event = eventsList[eventIndex]
  centerX = event.skyX
  centerY = event.skyY
  dialog = none
  mapPreview = true

proc showEventInfo(data: int; dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the selected event information
  ##
  ## * data   - the index of the selected item
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  eventIndex = data
  let baseIndex: ExtendedBasesRange = skyMap[eventsList[eventIndex].skyX][eventsList[
        eventIndex].skyY].baseIndex
  var eventInfo: string = "X: {gold}" & $eventsList[eventIndex].skyX &
      "{/gold} Y: {gold}" & $eventsList[eventIndex].skyY & "{/gold}"
  case eventsList[eventIndex].eType
  of enemyShip, enemyPatrol, trader, friendlyShip:
    try:
      eventInfo.add(y = "\nShip type: {gold}" & protoShipsList[eventsList[
          eventIndex].shipIndex].name & "{/gold}")
    except:
      dialog = setError(message = "Can't get the ship info")
      return
  of fullDocks, attackOnBase, disease:
    eventInfo.add(y = "\nBase name: {gold}" & skyBases[baseIndex].name & "{/gold}")
  of doublePrice:
    eventInfo.add(y = "\nBase name: {gold}" & skyBases[baseIndex].name & "{/gold}")
    try:
      eventInfo.add(y = "\nItem: {gold}" & itemsList[eventsList[
          eventIndex].itemIndex].name & "{/gold}")
    except:
      dialog = setError(message = "Can't get the item info")
  of EventsTypes.none, baseRecovery:
    discard
  dialog = setInfo(text = eventInfo, title = "Event information",
      button1 = ButtonSettings(tooltip: "Set the event as the ship destination",
      code: setTargetEvent, icon: destinationIcon.ord, text: "Target",
      color: "green"), button2 = ButtonSettings(
      tooltip: "Show the event on the map", code: showEvent,
      icon: showColoredIcon.ord, text: "Show", color: "green"))

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

  type LocalEventData = object
    name: string
    distance: Natural
    coords: string
    details: string
    id: Natural
  var localEvents: seq[LocalEventData] = @[]
  for event in knownEventsList:
    localEvents.add(y = LocalEventData(name: event.name,
        distance: event.distance, coords: event.coords, details: event.details,
        id: event.index))

  proc sortEvents(x, y: LocalEventData): int {.raises: [], tags: [],
      contractual.} =
    ## Compare two events and return which should go first, based on the sort
    ## order of the events
    ##
    ## * x - the first event to compare
    ## * y - the second event to compare
    ##
    ## Returns 1 if the first event should go first, -1 if the second event
    ## should go first.
    case eventsSortOrder
    of typeAsc:
      if x.name < y.name:
        return 1
      return -1
    of typeDesc:
      if x.name > y.name:
        return 1
      return -1
    of distanceAsc:
      if x.distance < y.distance:
        return 1
      return -1
    of distanceDesc:
      if x.distance > y.distance:
        return 1
      return -1
    of coordAsc:
      if x.coords < y.coords:
        return 1
      return -1
    of coordDesc:
      if x.coords > y.coords:
        return 1
      return -1
    of detailsAsc:
      if x.details < y.details:
        return 1
      return -1
    of detailsDesc:
      if x.details > y.details:
        return 1
      return -1
    of none:
      return -1

  localEvents.sort(cmp = sortEvents)
  knownEventsList = @[]
  for event in localEvents:
    var color: ColorsNames = yellowColor
    let evnt: EventData = eventsList[event.id]
    if evnt.skyX != playerShip.skyX or evnt.skyY != playerShip.skyY:
      case evnt.eType
      of enemyShip, attackOnBase, enemyPatrol:
        color = redColor
      of fullDocks:
        color = cyanColor
      of disease:
        color = goldenColor
      of doublePrice:
        color = limeColor
      of trader, friendlyShip:
        color = greenColor
      of EventsTypes.none, baseRecovery:
        discard
    let details: string = case evnt.eType
      of enemyShip, trader, friendlyShip:
        try:
          protoShipsList[evnt.shipIndex].name
        except KeyError:
          dialog = setError(message = "Can't get ship name")
          return
      of fullDocks, disease, attackOnBase, enemyPatrol:
        skyBases[skyMap[evnt.skyX][evnt.skyY].baseIndex].name
      of doublePrice:
        try:
          itemsList[evnt.itemIndex].name & " in " & skyBases[skyMap[evnt.skyX][
              evnt.skyY].baseIndex].name
        except KeyError:
          dialog = setError(message = "Can't get item name")
          return
      of EventsTypes.none, baseRecovery:
        ""
    knownEventsList.add(y = EventUIData(index: event.id, name: event.name,
        distance: event.distance, coords: event.coords, color: color,
        details: details))

proc showEventsInfo*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Show the list of the known events
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  # No events
  if knownEventsList.len == 0:
    setLayoutRowDynamic(height = 100, cols = 1)
    wrapLabel(str = "You don't know any event yet. You may ask for events in bases. When your ship is docked to base, select Ask for Events from ship orders menu.")
  else:
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
      ratio: array[4, cfloat] = [200.cfloat, 100, 150, 250]

    addHeader(headers = headers, ratio = ratio, tooltip = "events",
        code = sortEvents, dialog = dialog)
    let startRow: Positive = ((currentPage - 1) * gameSettings.listsLimit) + 1
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
    var
      row, currentRow: Positive = 1
    # Show the list of known events
    for event in knownEventsList:
      if currentRow < startRow:
        currentRow.inc
        continue
      setButtonStyle(field = textNormal, color = theme.colors[event.color])
      addButton(label = event.name, tooltip = "Show the event's details",
          data = event.index, code = showEventInfo, dialog = dialog)
      addButton(label = $event.distance, tooltip = "The distance to the event",
          data = event.index, code = showEventInfo, dialog = dialog)
      addButton(label = event.coords, tooltip = "The coordinates of the event",
          data = event.index, code = showEventInfo, dialog = dialog)
      addButton(label = event.details, tooltip = "Show the event's details",
          data = event.index, code = showEventInfo, dialog = dialog)
      setButtonStyle(field = textNormal, color = theme.colors[tableTextColor])
      row.inc
      if row == gameSettings.listsLimit + 1:
        break
    restoreButtonStyle()
    addPagination(page = currentPage, row = row)
