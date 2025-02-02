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
# along with Steam Sky.  if, see <http://www.gnu.org/licenses/>.

## Provides code related to showing in-game messages

import contracts, nuklear/nuklear_sdl_renderer

proc showLastMessages*() {.raises: [], tags: [], contractual.} =
  ## Show the last in-game messages to the player
  group(title = "LastMessagesGroup", flags = {windowBorder}):
    setLayoutRowDynamic(height = 25, cols = 1)
    label(str = "here")
