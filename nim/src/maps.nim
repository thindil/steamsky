# Copyright 2022-2023 Bartek thindil Jasicki
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

import std/math
import game, types

type SkyCell* = object
  ## Used to store information about the map's cell
  ##
  ## * baseIndex    - The index of the sky base located in the cell
  ## * visited      - If true, the cell was visited by the player
  ## * eventIndex   - Index of the event which happens in the cell
  ## * missionIndex - Index of the mission which takes place in the cell
  baseIndex*: ExtendedBasesRange
  visited*: bool
  eventIndex*: int
  missionIndex*: int

var skyMap*: array[MapXRange, array[MapYRange, SkyCell]] ## The list of all map's cells

func normalizeCoord*(coord: var cint; isXAxis: cint = 1) {.gcsafe, raises: [],
    tags: [], exportc.} =
  ## Normalize (fix to be in range of) the map's coordinates
  ##
  ## * coord   - The coordinate which will be normalized
  ## * isXAxis - If 1 the coordinate to be normalized is in X axis, otherwise
  ##             it is in Y axis
  ##
  ## Returns the updated coord argument
  if isXAxis == 1:
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
    destinationY: MapYRange): Natural {.sideEffect, raises: [], tags: [].} =
  ## Count the distance between the player's ship and the point on the map
  ##
  ## * destinationX - the X position of the point to which the distance will be count
  ## * destinationY - the Y position of the point to which the distance will be count
  ##
  ## The distance between the player's ship position and the selected point on the
  ## map.
  var
    diffX: float = ((playerShip.skyX - destinationX).abs).float
    diffY: float = ((playerShip.skyY - destinationY).abs).float
  return (sqrt((diffX^2) + (diffY^2))).floor.Natural


# Temporary code for interfacing with Ada

proc getAdaMapCell(x, y, baseIndex, visited, eventIndex,
    missionIndex: cint) {.raises: [], tags: [], exportc.} =
  skyMap[x][y] = SkyCell(baseIndex: baseIndex, visited: (if visited ==
      1: true else: false), eventIndex: eventIndex - 1,
      missionIndex: missionIndex - 1)

proc setAdaMapCell(x, y: cint; baseIndex, visited, eventIndex,
    missionIndex: var cint) {.raises: [], tags: [], exportc.} =
  baseIndex = skyMap[x][y].baseIndex
  visited = skyMap[x][y].visited.ord.cint
  eventIndex = skyMap[x][y].eventIndex.cint + 1
  missionIndex = skyMap[x][y].missionIndex.cint + 1

proc countAdaDistance(destinationX, destinationY: cint): cint {.raises: [],
    tags: [], exportc.} =
  return countDistance(destinationX, destinationY).cint
