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

import std/[algorithm, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[bases, basestypes, config, game, reputation, types]
import coreui, errordialog, setui, table, themes

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

proc showBaseInfo(data: int; dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show information about the selected base
  ##
  ## * data   - the index of the selected item
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  discard

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
    if currentRow < startRow:
      currentRow.inc
      continue
    addButton(label = base.name, tooltip = "Show the base's details",
      data = base.index, code = showBaseInfo, dialog = dialog)
    addButton(label = $base.distance, tooltip = "The distance to the base",
      data = base.index, code = showBaseInfo, dialog = dialog)
    addButton(label = base.coords, tooltip = "The coordinates of the base",
      data = base.index, code = showBaseInfo, dialog = dialog)
    if base.visited:
      addButton(label = $base.population,
        tooltip = "The population size of the base",
        data = base.index, code = showBaseInfo, dialog = dialog)
      addButton(label = $base.size, tooltip = "The size of the base",
        data = base.index, code = showBaseInfo, dialog = dialog)
      addButton(label = base.owner, tooltip = "The faction which own the base",
        data = base.index, code = showBaseInfo, dialog = dialog)
      addButton(label = base.baseType, tooltip = "The type of the base",
        data = base.index, code = showBaseInfo, dialog = dialog)
      addButton(label = base.reputation,
        tooltip = "Your reputation in the base",
        data = base.index, code = showBaseInfo, dialog = dialog)
    else:
      addButton(label = "not", tooltip = "Show the base's details",
          data = base.index, code = showBaseInfo, dialog = dialog)
      addButton(label = "", tooltip = "Show the base's details",
          data = base.index, code = showBaseInfo, dialog = dialog)
      addButton(label = "visited", tooltip = "Show the base's details",
          data = base.index, code = showBaseInfo, dialog = dialog)
      addButton(label = "", tooltip = "Show the base's details",
          data = base.index, code = showBaseInfo, dialog = dialog)
      addButton(label = "yet", tooltip = "Show the base's details",
          data = base.index, code = showBaseInfo, dialog = dialog)
    row.inc
    if row == gameSettings.listsLimit + 1:
      break
  restoreButtonStyle()
  addPagination(page = currentPage, row = row)
