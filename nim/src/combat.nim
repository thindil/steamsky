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

import types

type
  EnemyRecord* = object
    ship*: ShipRecord
    accuracy*: Natural
    distance*: int
    combatAi*: ShipCombatAi
    evasion*: Natural
    loot*: Natural
    perception*: Natural
    harpoonDuration: Natural
    guns: seq[array[3, Natural]]

var
  harpoonDuration*: Natural = 0
  enemy*: EnemyRecord = EnemyRecord(ship: ShipRecord(skyX: 1, skyY: 1));
