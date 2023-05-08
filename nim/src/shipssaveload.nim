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

import std/xmltree
import game

proc savePlayerShip*(saveData: var XmlNode) =
  var shipTree = newXmlTree("playership", [], {"name": playerShip.name,
      "x": $playerShip.skyX, "y": $playerShip.skyY, "speed": $playerShip.speed,
      "upgrademodule": $playerShip.upgradeModule,
      "destinationx": $playerShip.destinationX,
      "destinationy": $playerShip.destinationY,
      "repairpriority": $playerShip.repairModule,
      "homebase": $playerShip.homeBase}.toXmlAttributes)
  saveData.add(shipTree)
