# Copyright 2022-2023 Bartek thindil Jasicki
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
  ModuleType* = enum
    any, engine, cabin, cockpit, turret, gun, cargo, hull, armor, batteringRam,
    alchemyLab, furnace, waterCollector, workshop, greenhouse, medicalRoom,
    harpoonGun, trainingRoom
    ## Types of available prototypes of ships modules

  BaseModuleData* = object
    name*: string
    mType*: ModuleType
    weight*: Natural
    value*: int
    maxValue*: int
    durability*: int
    repairMaterial*: string
    repairSkill*: Positive
    price*: Natural
    installTime*: Positive
    unique*: bool
    size*: range[1..10]
    description*: string
    maxOwners*: range[0..10]
    speed*: int
    reputation*: ReputationRange
