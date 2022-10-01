# Copyright 2022 Bartek thindil Jasicki
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

{.used.}

import game

func normalizeCoord*(coord: var cint; isXAxis: cint = 1) {.gcsafe, raises: [],
    tags: [], exportc.} =
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
