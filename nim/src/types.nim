# Copyright 2022 Bartek thindil Jasicki
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

type
  ShipUpgrade = enum
    ## FUNCTION
    ##
    ## Available types of the player's ship's upgrades
    none, durability, maxValue, value

  ModuleType2 = enum
    ## FUNCTION
    ##
    ## Available types of the ships' modules
    workshop, any, medicalRoom, trainingRoom, engine, cabin, cockpit, turret,
        gun, cargoRoom, hull, armor, batteringRam, harpoonGun

  ShipSpeed* = enum
    ## FUNCTION
    ##
    ## Ships's state of speed, how much engines are used
    docked, full_Stop, quarter_Speed, half_Speed, full_Speed

  CrewOrders = enum
    ## FUNCTION
    ##
    ## Possible orders for ships' crew members
    pilot, engineer, gunner, repair, craft, upgrading, talk, heal, clean, rest,
        defend, boarding, train

  EquipmentLocations* = enum
    ## FUNCTION
    ##
    ## Possible equipment location for mobiles
    weapon, shield, helmet, torso, arms, legs, tool

  NamesTypes* = enum
    ## FUNCTION
    ##
    ## The type of the faction names, normal, based on syllables or robotic,
    ## based on random letters and numbers
    normal, robotic

  MapXRange* = range[1..1_024] ## The size of the game map in X axis
  MapYRange* = range[1..1_024] ## The size of the game map in Y axis
  ItemsDurability* = range[0..101] ## The range of the items durability
  SkillRange* = range[0..100] ## The range of skills levels
  AttributesArray* = array[1 .. 2, Natural] ## 1 - Attribute level, 2 - Attribute experience
  BasesRange* = range[1..1_024] ## The amount of bases in the game
  ReputationRange* = range[-100..100] ## The range of possible reputation levels


  ModuleData* = object
    ## FUNCTION
    ##
    ## Used to store information about ships' modules
    name: string ## The name of the module
    protoIndex: Natural ## The index of the prototype module
    weight: Natural ## The weight of the module
    durability: Natural ## The current durability of the module
    maxDurability: Natural ## The max durability of the module
    owner: seq[Natural] ## The list of owners of the module
    upgradeProgress: int ## The upgrade progess of the module
    upgradeAction: ShipUpgrade ## The current upgrade type for the module
    case mType: ModuleType2
    of engine:
      fuelUsage: Positive ## The fuel usage for engines modules
      power: Positive ## The power of the engines modules
      disabled: bool ## If true, the engine is disabled
    of cabin:
      cleanliness: Natural ## The cleanliness level of the cabin
      quality: Natural ## The quality level of the cabin
    of turret:
      gunIndex: Natural ## The index of the module used as gun in the turret
    of gun:
      damage: Positive ## The damage of the gun
      ammoIndex: Natural ## The index of item from ship's cargo used as ammunition
    of hull:
      installedModules: Natural ## The amount of installed modules in the hull
      maxModules: Positive ## The max amount of modules which the hull can hold
    of workshop:
      craftingIndex: string ## The index of currently crafted recipe
      craftingTime: Natural ## The amount of time needed to finish the order
      craftingAmount: Natural ## How many times repeat the crafting order
    of trainingRoom:
      trainedSkill: Natural ## The index of trained skill
    of batteringRam:
      damage2: Positive ## The damage of the battering ram
      coolingDown: bool ## If true, the battering ram can't attack now
    of harpoonGun:
      duration: Positive ## The duration bonus of the harpoon gun
      harpoonIndex: Natural ## The index of item from ship's cargo used as harpoon
    of any:
      data: array[1..3, int] ## Various data for module, depends on module
    else:
      discard

  InventoryData* = object
    ## FUNCTION
    ##
    ## Used to store information about items in various inventories (cargo, crew
    ## inventory, ect)
    protoIndex*: Natural ## The index of the item's prototype
    amount*: Positive ## The amount of the item in the inventory
    name*: string ## The name of the item, if different than the default one
    durability*: ItemsDurability ## The current durability of the item
    price*: Natural ## The price for which the item was bought

  MobAttributeRecord = object
    ## FUNCTION
    ##
    ## Used to store information about the crew member's attributes
    level: range[1..50] ## The level of the attribute
    experience: Natural ## The amount of experience in the attribute

  SkillInfo = object
    ## FUNCTION
    ##
    ## Used to store information about the crew member's skills
    index: Natural ## The index of the skill
    level: SkillRange ## The level of the skill
    experience: Natural ## The amount of the experience in the skill

  MemberData* = object
    ## FUNCTION
    ##
    ## Used to store information about the crew member
    attributes: seq[MobAttributeRecord] ## The member's attributes
    skills: seq[SkillInfo] ## The member's skills
    name: string ## The member's name
    gender: char ## The member's gender
    health: SkillRange ## The member's health points
    tired: range[0..150] ## The member's tiredness level
    hunger: SkillRange ## The member's hunger level
    thirst: SkillRange ## The member's thirst level
    order: CrewOrders ## The current order of the member
    previousOrder: CrewOrders ## The previous order of the member
    orderTime: int ## The amount of minutes to next check in the order
    orders: array[1..12, Natural] ## The orders priorities for the member
    inventory: seq[InventoryData] ## The inventory o the member
    equipment: array[EquipmentLocations, Natural] ## The equipment of the member
    payment: AttributesArray ## The payment information for the member
    contractLength: int ## The length of the contract with the member
    morale: AttributesArray ## The morale information for the member
    loyalty: SkillRange ## The loyalty level of the member
    homeBase: BasesRange ## The index of the home base
    faction: string ## The faction index to which the member belongs

  ShipRecord* = object
    ## FUNCTION
    ##
    ## Used to store information about ships
    name: string ## The name of the ship
    skyX: MapXRange ## The X position of the ship on the map
    skyY: MapYRange ## The Y position of the ship on the map
    speed: ShipSpeed ## The current setting for the ship's speed
    modules: seq[ModuleData] ## The list of modules installed on the ship
    cargo: seq[InventoryData] ## The list of items in the ship's cargo
    crew: seq[MemberData] ## The list of the crew members of the ship
    upgradeModule: int ## The index of the currently upgraded module
    destinationX: range[0..MapXRange.high] ## The X position to which the ship goes
    destinationY: range[0..MapYRange.high] ## The Y position to which the ship goes
    repairModule: int ## The index of module which will be repaired as first
    description: string ## The description of the ship
    homeBase: Natural ## The index of the home base of the ship

  ReputationRanges = object
    ## FUNCTION
    ##
    ## Used to store reputation ranges for relation with other factions
    min: ReputationRange ## Minimal reputation with the selected faction
    max: ReputationRange ## Maximal reputation with the selected faction

  RelationsData = object
    ## FUNCTION
    ##
    ## Used to store data about relation with other faction
    reputation: ReputationRanges ## Values of min and max reputation with the faction
    friendly: bool ## If true, the selected faction is friendly towards the faction

  CareerData = object
    ## FUNCTION
    ##
    ## Used to store data about careers available for the faction
    shipIndex: Positive ## The index of the starting ship prototype for the career
    playerIndex: string ## The index of the starting mob prototype as the player character
    description: string ## The description of the career
    name: string ## The name of the career

  FactionData* = object
    ## FUNCTION
    ##
    ## Used to store data about the selected faction
    name*: string ## The name of the faction
    memberName: string ## The name of members of the faction
    pluralMemberName: string ## The name for plural amount of members of the faction
    spawnChance: Natural ## The chance of the spawn for a base of the selected faction
    population: AttributesArray ## The min and max population for new bases of the faction
    namesType*: NamesTypes ## The type of names for the mobs, bases and ships of the faction
    relations: Table[string, RelationsData] ## The faction's relations with other factions
    description: string ## The description of the faction
    foodTypes: seq[string] ## The types of items used as food for the faction's members
    drinksTypes: seq[string] ## The types of items used as drinks for the faction's members
    healingTools: string ## The type of items used as healing tools for the faction's members
    healingSkill: Natural ## The skill used as healing skill for the faction's members
    flags: seq[string] ## Various flags set for the faction
    careers: Table[string, CareerData] ## The list of available careers for the faction
    baseIcon: Natural ## The icon used as icon for the faction's bases on the map
    basesTypes: Table[string, Positive] ## The list of available bases types for the faction
    weaponSkill*: Natural ## The skill used as prefered weapon skill for the faction

