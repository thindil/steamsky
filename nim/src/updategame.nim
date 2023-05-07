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

import basesship, config, crew, game, shipscrew, types

proc updateGame*(minutes: Positive; inCombat: bool = false) =
  var needCleaning, needSaveGame = false

  proc updateDay() =
    gameDate.day.inc
    for module in playerShip.modules.mitems:
      if module.mType == ModuleType2.cabin and module.cleanliness > 0:
        module.cleanliness.dec
        needCleaning = true
    if needCleaning:
      updateOrders(ship = playerShip)
    if playerShip.speed == docked:
      payForDock()
    dailyPayment()
    if $gameSettings.autoSave == $daily:
      needSaveGame = true

  var tiredPoints = 0
  for i in 1 .. minutes:
    if gameDate.minutes + i mod 15 == 0:
      tiredPoints.inc
  let addedMinutes = minutes mod 60
  gameDate.minutes = gameDate.minutes + addedMinutes
  if gameDate.minutes > 59:
    gameDate.minutes = gameDate.minutes - 60
    gameDate.hour.inc
  var addedHours = (minutes / 60).int
  while addedHours > 23:
    addedHours = addedHours - 24
    updateDay()
  gameDate.hour = gameDate.hour + addedHours
  while gameDate.hour > 23:
    gameDate.hour = gameDate.hour - 24
    updateDay()
