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
## showing the list, information about the selected base, etc.

import std/[algorithm, strutils, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[bases, basestypes, config, game, messages, reputation, types, utils]
import coreui, dialogs, errordialog, mapsui, setui, table, themes

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

proc showBaseInfo*(dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the selectedbase information
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  const width: float = 400

  let
    base: BaseRecord = skyBases[baseIndex]
    windowName: string = base.name
    height: float = (if base.visited.year > 0: 400 else: 170)
  updateDialog(width = width, height = height)
  window(name = windowName, x = dialogX, y = dialogY, w = width, h = height,
      flags = {windowBorder, windowTitle, windowNoScrollbar, windowMovable}):
    setLayoutRowDynamic(height = 30, cols = 4, ratio = [0.4.cfloat, 0.1, 0.1, 0.1])
    label(str = "Coordinates X:")
    colorLabel(str = $base.skyX, color = theme.colors[goldenColor])
    label(str = "Y:")
    colorLabel(str = $base.skyY, color = theme.colors[goldenColor])
    if base.visited.year == 0:
      setLayoutRowDynamic(height = 30, cols = 1)
      colorLabel(str = "Not visited yet.", color = theme.colors[redColor])
    else:
      setLayoutRowDynamic(height = 30, cols = 2)
      label(str = "Last visited:")
      colorLabel(str = formattedTime(time = base.visited),
          color = theme.colors[goldenColor])
      let population: BasePopulation = getBasePopulation(baseIndex = baseIndex)
      var timeDiff: int = 0
      if population > empty and base.reputation.level > -25:
        timeDiff = 30 - daysDifference(dateToCompare = base.recruitDate)
        if timeDiff > 0:
          setLayoutRowDynamic(height = 30, cols = 3,
              ratio = [0.7.cfloat, 0.1, 0.2])
          label(str = "New recruits available in")
          colorLabel(str = $timeDiff, color = theme.colors[goldenColor])
          label(str = "days.")
        else:
          setLayoutRowDynamic(height = 30, cols = 1)
          colorLabel(str = "New recruits available now.",
              color = theme.colors[greenColor])
      else:
        colorLabel(str = "You can't recruit crew members at this base.",
            color = theme.colors[redColor])
      if population > empty and base.reputation.level > -25:
        timeDiff = daysDifference(dateToCompare = base.askedForEvents)
        if timeDiff < 7:
          setLayoutRowDynamic(height = 30, cols = 3,
              ratio = [0.7.cfloat, 0.1, 0.2])
          label(str = "You asked for events")
          colorLabel(str = $timeDiff, color = theme.colors[goldenColor])
          label(str = "days ago.")
        else:
          setLayoutRowDynamic(height = 30, cols = 1)
          colorLabel(str = "You can ask for events again",
              color = theme.colors[greenColor])
      else:
        colorLabel(str = "You can't ask for events at this base.",
            color = theme.colors[redColor])
      if population > empty and base.reputation.level > -1:
        timeDiff = 7 - daysDifference(dateToCompare = base.missionsDate)
        if timeDiff > 0:
          setLayoutRowDynamic(height = 30, cols = 3,
              ratio = [0.7.cfloat, 0.1, 0.2])
          label(str = "New missions available in")
          colorLabel(str = $timeDiff, color = theme.colors[goldenColor])
          label(str = "days.")
        else:
          setLayoutRowDynamic(height = 30, cols = 1)
          colorLabel(str = "New missions available now.",
              color = theme.colors[greenColor])
      else:
        colorLabel(str = "You can't take missions at this base.",
            color = theme.colors[redColor])
      if baseIndex == playerShip.homeBase:
        colorLabel(str = "It is your home base.", color = theme.colors[cyanColor])
      if skyBases[baseIndex].reputation.level == 0:
        setLayoutRowDynamic(height = 30, cols = 2, ratio = [0.3.cfloat, 0.3])
        label(str = "Reputation:")
        colorLabel(str = "Unknown", color = theme.colors[goldenColor])
      else:
        setLayoutRowDynamic(height = 30, cols = 3, ratio = [0.3.cfloat, 0.35, 0.35])
        label(str = "Reputation:")
        let reputationText: string = getReputationText(
            reputationLevel = base.reputation.level)
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(), text = reputationText)
        if base.reputation.level < 0:
          var repLevel: Positive = base.reputation.level.abs
          changeStyle(field = progressbar, color = theme.colors[redColor]):
            progressBar(value = repLevel, maxValue = 100, modifyable = false,
                reversed = true)
        else:
          var repLevel: Natural = 0
          progressBar(value = repLevel, maxValue = 100, modifyable = false)
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(), text = reputationText)
        if base.reputation.level > 0:
          var repLevel: Positive = base.reputation.level
          progressBar(value = repLevel, maxValue = 100, modifyable = false)
        else:
          var repLevel: Natural = 0
          progressBar(value = repLevel, maxValue = 100, modifyable = false)
    setLayoutRowDynamic(height = 30, cols = 3)
    setButtonStyle(field = textNormal, color = theme.colors[greenColor])
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Set the base as the ship destination")
    imageLabelButton(image = images[destinationIcon], text = "Target",
        alignment = right):
      if base.skyX == playerShip.skyX and base.skyY == playerShip.skyY:
        dialog = setMessage(message = "You are at this location now.",
            title = "Can't set destination")
        return
      playerShip.destinationX = base.skyX
      playerShip.destinationY = base.skyY
      addMessage(message = "You set the travel destination for your ship.",
          mType = orderMessage)
      dialog = none
    restoreButtonStyle()
    addCloseButton(dialog = dialog, isPopup = false)
    setButtonStyle(field = textNormal, color = theme.colors[greenColor])
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(), text = "Show the base on the map")
    imageLabelButton(image = images[showColoredIcon], text = "Show",
        alignment = right):
      centerX = base.skyX
      centerY = base.skyY
      dialog = none
      mapPreview = true
    restoreButtonStyle()

  windowSetFocus(name = windowName)

proc setBaseInfo(data: int; dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Set the information about the selected base
  ##
  ## * data   - the index of the selected item
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog.
  baseIndex = data
  dialog = baseDialog

proc sortBases(sortAsc, sortDesc: BasesSortOrders;
    dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Sort bases on the list
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

  type LocalBaseData = object
    name: string
    distance: Natural
    coords: string
    population: int
    size: BasesSize
    owner: string
    baseType: string
    reputation: int
    id: BasesRange = 1
  var localBases: seq[LocalBaseData] = @[]
  for base in knownBasesList:
    localBases.add(y = LocalBaseData(name: base.name, distance: base.distance,
        coords: base.coords, population: (if base.visited: skyBases[
        base.index].population.int else: -1), size: (
        if base.visited: base.size else: unknown), owner: (
        if base.visited: base.owner else: ""), baseType: (
        if base.visited: base.baseType else: ""), reputation: (
        if base.visited: skyBases[base.index].reputation.level.int else: 200),
        id: base.index))

  proc sortBases(x, y: LocalBaseData): int {.raises: [], tags: [],
      contractual.} =
    ## Compare two bases and return which should go first, based on the sort
    ## order of the bases
    ##
    ## * x - the first base to compare
    ## * y - the second base to compare
    ##
    ## Returns 1 if the first base should go first, -1 if the second base
    ## should go first.
    case basesSortOrder
    of nameAsc:
      if x.name < y.name:
        return 1
      return -1
    of nameDesc:
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
    of populationAsc:
      if x.population < y.population:
        return 1
      return -1
    of populationDesc:
      if x.population > y.population:
        return 1
      return -1
    of sizeAsc:
      if x.size < y.size:
        return 1
      return -1
    of sizeDesc:
      if x.size > y.size:
        return 1
      return -1
    of ownerAsc:
      if x.owner < y.owner:
        return 1
      return -1
    of ownerDesc:
      if x.owner > y.owner:
        return 1
      return -1
    of typeAsc:
      if x.baseType < y.baseType:
        return 1
      return -1
    of typeDesc:
      if x.baseType > y.baseType:
        return 1
      return -1
    of reputationAsc:
      if x.reputation < y.reputation:
        return 1
      return -1
    of reputationDesc:
      if x.reputation > y.reputation:
        return 1
      return -1
    of none:
      return -1

  localBases.sort(cmp = sortBases)
  knownBasesList = @[]
  for base in localBases:
    try:
      knownBasesList.add(y = BaseData(index: base.id, name: base.name,
          distance: base.distance, coords: base.coords,
          population: getBasePopulation(baseIndex = base.id), size: skyBases[
          base.id].size, owner: factionsList[skyBases[base.id].owner].name,
          baseType: basesTypesList[skyBases[base.id].baseType].name,
          reputation: getReputationText(reputationLevel = skyBases[
          base.id].reputation.level), visited: base.population > -1))
    except:
      dialog = setError(message = "Can't set the list of known bases")

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
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Show only the selected type bases")
    basesType = comboList(items = basesTList, selected = basesType,
        itemHeight = 25, x = 150, y = 150)
    label(str = "Status:")
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Show only the selected status bases")
    basesStatus = comboList(items = basesStatuses, selected = basesStatus,
        itemHeight = 25, x = 150, y = 150)
    label(str = "Owner:")
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Show only the selected owner bases")
    basesOwner = comboList(items = basesOwners, selected = basesOwner,
        itemHeight = 25, x = 150, y = 150)
    setLayoutRowDynamic(height = 25, cols = 2, ratio = [0.2.cfloat, 0.8])
    label(str = "Name:")
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Search for a base with the selected name")
    editString(text = nameSearch, maxLen = 64)
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
  # Show the list of known bases
  for base in knownBasesList:
    if nameSearch.len > 0 and base.name.toLowerAscii.find(
        sub = nameSearch.toLowerAscii) == -1:
      continue
    if basesStatus > 0 and (basesStatus - 1) != base.visited.ord:
      continue
    if basesOwner > 0 and (not base.visited or basesOwners[basesOwner] != base.owner):
      continue
    if currentRow < startRow:
      currentRow.inc
      continue
    if base.visited:
      setButtonStyle(field = textNormal, color = theme.colors[greenColor])
    elif skyBases[base.index].skyX == playerShip.destinationX and skyBases[
        base.index].skyY == playerShip.destinationY:
      setButtonStyle(field = textNormal, color = theme.colors[yellowColor])
    addButton(label = base.name, tooltip = "Show the base's details",
      data = base.index, code = setBaseInfo, dialog = dialog)
    addButton(label = $base.distance, tooltip = "The distance to the base",
      data = base.index, code = setBaseInfo, dialog = dialog)
    addButton(label = base.coords, tooltip = "The coordinates of the base",
      data = base.index, code = setBaseInfo, dialog = dialog)
    if base.visited:
      addButton(label = $base.population,
        tooltip = "The population size of the base",
        data = base.index, code = setBaseInfo, dialog = dialog)
      addButton(label = $base.size, tooltip = "The size of the base",
        data = base.index, code = setBaseInfo, dialog = dialog)
      addButton(label = base.owner, tooltip = "The faction which own the base",
        data = base.index, code = setBaseInfo, dialog = dialog)
      addButton(label = base.baseType, tooltip = "The type of the base",
        data = base.index, code = setBaseInfo, dialog = dialog)
      addButton(label = base.reputation,
        tooltip = "Your reputation in the base",
        data = base.index, code = setBaseInfo, dialog = dialog)
    else:
      addButton(label = "not", tooltip = "Show the base's details",
          data = base.index, code = setBaseInfo, dialog = dialog)
      addButton(label = "", tooltip = "Show the base's details",
          data = base.index, code = setBaseInfo, dialog = dialog)
      addButton(label = "visited", tooltip = "Show the base's details",
          data = base.index, code = setBaseInfo, dialog = dialog)
      addButton(label = "", tooltip = "Show the base's details",
          data = base.index, code = setBaseInfo, dialog = dialog)
      addButton(label = "yet", tooltip = "Show the base's details",
          data = base.index, code = setBaseInfo, dialog = dialog)
    setButtonStyle(field = textNormal, color = theme.colors[tableTextColor])
    row.inc
    if row == gameSettings.listsLimit + 1:
      break
  restoreButtonStyle()
  addPagination(page = currentPage, row = row)
