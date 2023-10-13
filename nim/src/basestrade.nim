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

import bases, game, crewinventory, maps, shipscrew, trades, types

proc checkMoney(price: Positive; message: string = ""): Positive =
  result = findItem(inventory = playerShip.cargo, protoIndex = moneyIndex)
  if result == -1:
    raise newException(exceptn = NoMoneyError, message = message)
  if playerShip.cargo[result].amount < price:
    raise newException(exceptn = NotEnoughMoneyError, message = message)

proc hireRecruit*(recruitIndex: Natural; cost: Positive; dailyPayment, tradePayment: Natural, contractLength: int) =
  let traderIndex = findMember(order = talk)
  if traderIndex == -1:
    raise newException(exceptn = NoTraderError, message = "")
  var price: Natural = cost
  countPrice(price = price, traderIndex = traderIndex)
  let
    baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
    recruit = skyBases[baseIndex].recruits[recruitIndex]
    moneyIndex2 = checkMoney(price = price, message = recruit.name)
