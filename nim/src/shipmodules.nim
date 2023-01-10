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
    ## Used to store information about prototypes of ships' modules
    name*: string ## The name of the module
    mType*: ModuleType ## The type of the module
    weight*: Natural ## The weight of the module
    value*: int ## Additional data for the module, for engines it is power
    maxValue*: int ## Additional data for the mode, for guns it is damage
    durability*: int ## The base durability of the module
    repairMaterial*: string ## The index of the material used to repair the module
    repairSkill*: Positive ## The index of the skill used to repair the module
    price*: Natural ## The base price of the module in shipyards
    installTime*: Positive ## The amount of time needed to install the module
    unique*: bool ## If true, only one that module can be installed on the ship
    size*: range[1..10] ## The size of the module
    description*: string ## The description of the module
    maxOwners*: range[0..10] ## The amount of users of the module
    speed*: int ## How fast the gun shoots in the combat
    reputation*: ReputationRange ## The minumum amount of reputation needed for buy the module
