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

import std/tables
import bases, game, game2, crewinventory, maps, messages, shipscargo, shipscrew,
    trades, types

proc checkMoney(price: Positive; message: string = ""): Positive =
  result = findItem(inventory = playerShip.cargo, protoIndex = moneyIndex)
  if result == -1:
    raise newException(exceptn = NoMoneyError, message = message)
  if playerShip.cargo[result].amount < price:
    raise newException(exceptn = NotEnoughMoneyError, message = message)

proc hireRecruit*(recruitIndex: Natural; cost: Positive; dailyPayment,
    tradePayment: Natural; contractLength: int) =
  let traderIndex = findMember(order = talk)
  if traderIndex == -1:
    raise newException(exceptn = NoTraderError, message = "")
  var price: Natural = cost
  countPrice(price = price, traderIndex = traderIndex)
  let
    baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
    recruit = skyBases[baseIndex].recruits[recruitIndex]
  var inventory: seq[InventoryData]
  for item in recruit.inventory:
    inventory.add(InventoryData(protoIndex: item, amount: 1, name: "",
        durability: defaultItemDurability, price: 0))
  var morale: Natural = 0
  if "nomorale" in factionsList[skyBases[baseIndex].owner].flags:
    morale = 50
  else:
    morale = 50 + skyBases[baseIndex].reputation.level
    if morale > 100:
      morale = 100
  playerShip.crew.add(MemberData(name: recruit.name, gender: recruit.gender,
      health: 100, tired: 0, skills: recruit.skills, hunger: 0, thirst: 0,
      order: rest, previousOrder: rest, orderTime: 15,
      attributes: recruit.attributes, inventory: inventory,
      equipment: recruit.equipment, payment: [1: dailyPayment, 2: tradePayment],
      contractLength: contractLength, morale: [1: morale, 2: 0],
      loyalty: morale, homeBase: recruit.homeBase, faction: recruit.faction))
  let moneyIndex2 = checkMoney(price = price, message = recruit.name)
  updateCargo(ship = playerShip, cargoIndex = moneyIndex2, amount = -price)
  gainExp(amount = 1, skillNumber = talkingSkill, crewIndex = traderIndex)
  gainRep(baseIndex = baseIndex, points = 1)
  addMessage(message = "You hired " & recruit.name & " for " & $price & " " &
      moneyName & ".", mType = tradeMessage)
  skyBases[baseIndex].recruits.delete(recruitIndex)
  skyBases[baseIndex].population.dec
  updateGame(minutes = 5)

# Temporary code for interfacing with Ada

proc hireAdaRecruit(recruitIndex, cost, dailyPayment, tradePayment,
    contractLength: cint): cstring {.raises: [], tags: [WriteIOEffect,
    RootEffect], exportc.} =
  try:
    hireRecruit(recruitIndex = recruitIndex, cost = cost,
        dailyPayment = dailyPayment, tradePayment = tradePayment,
        contractLength = contractLength)
    return "".cstring
  except Exception as e:
    return ($e.name & " " & e.msg).cstring
