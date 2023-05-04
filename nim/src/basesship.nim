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

import bases, config, crewinventory, game, maps, messages, shipscargo, shipscrew, types

proc payForDock*() =
  let baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  if skyBases[baseIndex].population == 0:
    return
  let moneyIndex2 = findItem(inventory = playerShip.cargo,
      protoIndex = moneyIndex)
  if moneyIndex2 == 0:
    gainRep(baseIndex = baseIndex, points = -10)
    addMessage(message = "You don't have " & moneyName &
        " for pay for docking!", mType = otherMessage, color = red)
    return
  var dockingCost: Natural = 0
  for module in playerShip.modules.items:
    if module.mType == ModuleType2.hull:
      dockingCost = module.maxModules
      break
  dockingCost = (dockingCost.float * newGameSettings.pricesBonus).Natural
  if dockingCost == 0:
    dockingCost = 1
  let traderIndex = findMember(order = talk)
  countPrice(price = dockingCost, traderIndex = traderIndex)
  if dockingCost > playerShip.cargo[moneyIndex2].amount:
    dockingCost = playerShip.cargo[moneyIndex].amount
  updateCargo(ship = playerShip, cargoIndex = moneyIndex2, amount = -(dockingCost))
