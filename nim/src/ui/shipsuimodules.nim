# Copyright 2023 Bartek thindil Jasicki
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

import coreui, table

var modulesTable: TableWidget

proc updateModulesInfo*(page: Positive = 1) =
  let
    shipCanvas = mainPaned & ".shipinfoframe.modules.canvas"
    shipInfoFrame = shipCanvas & ".frame"
  if modulesTable.rowHeight == 1:
    modulesTable = createTable(parent = shipInfoFrame, headers = @["Name",
        "Durability", "Additional info"], scrollbar = mainPaned &
        ".shipinfoframe.modules.scrolly", command = "SortShipModules",
        tooltipText = "Press mouse button to sort the modules.")
