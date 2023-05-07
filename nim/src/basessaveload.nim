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

proc saveBases*(saveData: var XmlNode) =
  for skyBase in skyBases.items:
    var baseTree = newXmlTree("base", [], {
        "name": skyBase.name, "type": $skyBase.baseType,
        "population": $skyBase.population}.toXmlAttributes)
    if skyBase.visited.year > 0:
      var saveDate = newElement("visiteddate")
      saveDate.attrs = {"year": $skyBase.visited.year,
          "month": $skyBase.visited.month, "day": $skyBase.visited.day,
          "hour": $skyBase.visited.hour,
          "minutes": $skyBase.visited.minutes}.toXmlAttributes
      baseTree.add(saveDate)
      saveDate = newElement("recruitdate")
      saveDate.attrs = {"year": $skyBase.recruitDate.year,
          "month": $skyBase.recruitDate.month, "day": $skyBase.recruitDate.day,
          "hour": $skyBase.recruitDate.hour,
          "minutes": $skyBase.recruitDate.minutes}.toXmlAttributes
      baseTree.add(saveDate)
    saveData.add(baseTree)
