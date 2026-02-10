# Copyright 2022-2026 Bartek thindil Jasicki
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

## Provides code related to the in-game map, like data structure and
## counting distance between the player's ship and destination point.

import std/math
import contracts, nimalyzer
import game, types

type SkyCell* = object
  ## Used to store information about the map's cell
  ##
  ## * baseIndex    - The index of the sky base located in the cell
  ## * visited      - If true, the cell was visited by the player
  ## * eventIndex   - Index of the event which happens in the cell
  ## * missionIndex - Index of the mission which takes place in the cell
  baseIndex: ExtendedBasesRange
  visited: bool
  eventIndex: ExtendedNatural
  missionIndex: ExtendedNatural

proc baseIndex*(skyCell: SkyCell): ExtendedBasesRange {.raises: [], tags: [],
    contractual.} =
  ## The getter of a field of SkyCell type
  ##
  ## * skyCell - the SkyCell object which field will be get
  ##
  ## Returns the value of the selected field
  skyCell.baseIndex

proc `baseIndex=`*(skyCell: var SkyCell, value: ExtendedBasesRange) {.raises: [],
    tags: [], contractual.} =
  ## The setter of a field of SkyCell type
  ##
  ## * skyCell - the SkyCell object which field will be modified
  ## * value   - the new value for the field
  ##
  ## Returns the modified SkyCell object
  skyCell.baseIndex = value

proc visited*(skyCell: SkyCell): bool {.raises: [], tags: [], contractual.} =
  ## The getter of a field of SkyCell type
 ##
  ## * skyCell - the SkyCell object which field will be get
  ##
  ## Returns the value of the selected field
  skyCell.visited

proc `visited=`*(skyCell: var SkyCell, value: bool) {.raises: [], tags: [], contractual.} =
  ## The setter of a field of SkyCell type
  ##
  ## * skyCell - the SkyCell object which field will be modified
  ## * value   - the new value for the field
  ##
  ## Returns the modified SkyCell object
  skyCell.visited = value

proc eventIndex*(skyCell: SkyCell): ExtendedNatural {.raises: [], tags: [],
    contractual.} =
  ## The getter of a field of SkyCell type
  ##
  ## * skyCell - the SkyCell object which field will be get
  ##
  ## Returns the value of the selected field
  skyCell.eventIndex

proc `eventIndex=`*(skyCell: var SkyCell, value: ExtendedNatural) {.raises: [],
    tags: [], contractual.} =
  ## The setter of a field of SkyCell type
  ##
  ## * skyCell - the SkyCell object which field will be modified
  ## * value   - the new value for the field
  ##
  ## Returns the modified SkyCell object
  skyCell.eventIndex = value

proc missionIndex*(skyCell: SkyCell): ExtendedNatural {.raises: [], tags: [],
    contractual.} =
  ## The getter of a field of SkyCell type
  ##
  ## * skyCell - the SkyCell object which field will be get
  ##
  ## Returns the value of the selected field
  skyCell.missionIndex

proc `missionIndex=`*(skyCell: var SkyCell, value: ExtendedNatural) {.raises: [],
    tags: [], contractual.} =
  ## The setter of a field of SkyCell type
  ##
  ## * skyCell - the SkyCell object which field will be modified
  ## * value   - the new value for the field
  ##
  ## Returns the modified SkyCell object
  skyCell.missionIndex = value

proc initSkyCell*(baseIndex: ExtendedBasesRange = 0; visited: bool = false;
    eventIndex: ExtendedNatural = -1;
    missionIndex: ExtendedNatural = -1): SkyCell {.raises: [], tags: [],
    contractual.} =
  ## Create a new data structure for the map cell
  ##
  ## * baseIndex    - the index of the base in that cell
  ## * visited      - if true, the cell was visited by the player
  ## * eventIndex   - the index of the event in that cell
  ## * missionIndex - the index of the mission in that cell
  ##
  ## Returns the new structure with information about the selected map cell
  return SkyCell(baseIndex: baseIndex, visited: visited, eventIndex: eventIndex,
      missionIndex: missionIndex)

{.push ruleOff: "varDeclared".}
var
  skyMap*: array[MapXRange, array[MapYRange, SkyCell]]
    ## The list of all map's cells
{.push ruleOn: "varDeclared".}

proc normalizeCoord*(coord: var int; isXAxis: bool = true) {.raises: [], tags: [
    ], contractual.} =
  ## Normalize (fix to be in range of) the map's coordinates
  ##
  ## * coord   - The coordinate which will be normalized
  ## * isXAxis - If true the coordinate to be normalized is in X axis, otherwise
  ##             it is in Y axis
  ##
  ## Returns the updated coord argument
  ensure:
    if isXAxis:
      coord >= MapXRange.low and coord <= MapXRange.high
    else:
      coord >= MapYRange.low and coord <= MapYRange.high
  body:
    if isXAxis:
      if coord < MapXRange.low:
        coord = MapXRange.low
      elif coord > MapXRange.high:
        coord = MapXRange.high
    else:
      if coord < MapYRange.low:
        coord = MapYRange.low
      elif coord > MapYRange.high:
        coord = MapYRange.high

proc countDistance*(destinationX: MapXRange;
    destinationY: MapYRange): Natural {.raises: [], tags: [],
    contractual.} =
  ## Count the distance between the player's ship and the point on the map
  ##
  ## * destinationX - the X position of the point to which the distance will be count
  ## * destinationY - the Y position of the point to which the distance will be count
  ##
  ## The distance between the player's ship position and the selected point on the
  ## map.
  body:
    var
      diffX: float = ((playerShip.skyX - destinationX).abs).float
      diffY: float = ((playerShip.skyY - destinationY).abs).float
    return (sqrt(x = (diffX^2) + (diffY^2))).floor.Natural
