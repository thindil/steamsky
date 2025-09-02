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

## Provides code related to the information about the player's ship's crew
## members, like listing them, showing information, give orders, etc.

import contracts, nuklear/nuklear_sdl_renderer
import setui

var
  showCrewOptions*: bool = false
    ## Show additonal options for managing the player's ship's crew

proc showCrewInfo*() {.raises: [], tags: [], contractual.} =
  ## Show the list of the player's ship's crew members
  if showCrewOptions:
    var
      cols: Positive = 2
      ratio: seq[cfloat] = @[0.4.cfloat, 0.1]
    if needClean:
      cols.inc
      ratio.add(y = 0.1.cfloat)
    if needRepair:
      cols.inc
      ratio.add(y = 0.1.cfloat)
    setLayoutRowDynamic(height = 35, cols = cols, ratio = ratio)
    label(str = "Orders for all:")
