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

import std/tables

type

  CrewNoSpaceError* = object of CatchableError
    ## Raised when there is no space for new item in crew member inventory

  CrewOrderError* = object of CatchableError
    ## Used to mark problems during giving orders to the crew members

  ShipUpgrade* = enum
    ## Available types of the player's ship's upgrades
    none, durability, maxValue, value

  ModuleType2* {.pure.} = enum
    ## Available types of the ships' modules
    workshop, any, medicalRoom, trainingRoom, engine, cabin, cockpit, turret,
        gun, cargoRoom, hull, armor, batteringRam, harpoonGun

  ShipSpeed* = enum
    ## Ships's state of speed, how much engines are used
    docked, fullStop = "full_Stop", quarterSpeed = "quarter_Speed",
        halfSpeed = "half_Speed", fullSpeed = "full_Speed"

  CrewOrders* = enum
    ## Possible orders for ships' crew members
    pilot, engineer, gunner, repair, craft, upgrading, talk, heal, clean, rest,
        defend, boarding, train

  EquipmentLocations* = enum
    ## Possible equipment location for mobiles
    weapon, shield, helmet, torso, arms, legs, tool

  NamesTypes* = enum
    ## The type of the faction names, normal, based on syllables or robotic,
    ## based on random letters and numbers
    normal, robotic

  BasesSize* = enum
    ## Size of sky bases
    small, medium, big, unknown

  MissionsTypes* = enum
    ## Types of missions
    deliver, destroy, patrol, explore, passenger

  ModuleType* {.pure.} = enum
    ## Types of available prototypes of ships modules
    any, engine, cabin, cockpit, turret, gun, cargo, hull, armor, batteringRam,
    alchemyLab, furnace, waterCollector, workshop, greenhouse, medicalRoom,
    harpoonGun, trainingRoom

  GoalTypes* {.pure.} = enum
    ## Types of in-game goals
    random, reputation, destroy, discover, visit, craft, mission, kill

  MessageType* = enum
    ## Type of an in-game message
    default, combatMessage, tradeMessage, orderMessage, craftMessage,
        otherMessage, missionMessage

  MessageColor* = enum
    ## The color used to show a message
    white, yellow, green, red, blue, cyan

  EventsTypes* {.pure.} = enum
    ## Possible types of events on map
    none, enemyShip, attackOnBase, disease, doublePrice, baseRecovery,
        fullDocks, enemyPatrol, trader, friendlyShip

  ShipCombatAi* {.pure.} = enum
    ## Possible types of NPC's ships combat behaviour
    none, berserker, attacker, coward, disarmer

  MapXRange* = range[1..1_024] ## The size of the game map in X axis
  MapYRange* = range[1..1_024] ## The size of the game map in Y axis
  ItemsDurability* = range[0..101] ## The range of the items durability
  SkillRange* = range[0..100] ## The range of skills levels
  AttributesArray* = array[1..2, Natural] ## 1 - Attribute level, 2 - Attribute experience
  BasesRange* = range[1..1_024] ## The amount of bases in the game
  ExtendedBasesRange* = range[0..1_024] ## The amount of bases in the game with zero value
  ReputationRange* = range[-100..100] ## The range of possible reputation levels
  RewardMultiplier* = range[0.0..2.0] ## The range of multiplier for missions reward

  ModuleData* = object
    ## Used to store information about ships' modules
    name*: string                ## The name of the module
    protoIndex*: Natural         ## The index of the prototype module
    weight*: Natural             ## The weight of the module
    durability*: Natural         ## The current durability of the module
    maxDurability*: Natural      ## The max durability of the module
    owner*: seq[int]             ## The list of owners of the module
    upgradeProgress*: int        ## The upgrade progess of the module
    upgradeAction*: ShipUpgrade  ## The current upgrade type for the module
    case mType*: ModuleType2
    of ModuleType2.engine:
      fuelUsage*: Positive       ## The fuel usage for engines modules
      power*: Positive           ## The power of the engines modules
      disabled*: bool            ## If true, the engine is disabled
    of ModuleType2.cabin:
      cleanliness*: Natural      ## The cleanliness level of the cabin
      quality*: Natural          ## The quality level of the cabin
    of ModuleType2.turret:
      gunIndex*: int             ## The index of the module used as gun in the turret
    of ModuleType2.gun:
      damage*: Positive          ## The damage of the gun
      ammoIndex*: int            ## The index of item from ship's cargo used as ammunition
    of ModuleType2.hull:
      installedModules*: Natural ## The amount of installed modules in the hull
      maxModules*: Positive      ## The max amount of modules which the hull can hold
    of ModuleType2.workshop:
      craftingIndex*: string     ## The index of currently crafted recipe
      craftingTime*: Natural     ## The amount of time needed to finish the order
      craftingAmount*: Natural   ## How many times repeat the crafting order
    of ModuleType2.trainingRoom:
      trainedSkill*: Natural     ## The index of trained skill
    of ModuleType2.batteringRam:
      damage2*: Positive         ## The damage of the battering ram
      coolingDown*: bool         ## If true, the battering ram can't attack now
    of ModuleType2.harpoonGun:
      duration*: Positive        ## The duration bonus of the harpoon gun
      harpoonIndex*: int         ## The index of item from ship's cargo used as harpoon
    of ModuleType2.any:
      data*: array[1..3, int]    ## Various data for module, depends on module
    else:
      discard

  InventoryData* = object
    ## Used to store information about items in various inventories (cargo, crew
    ## inventory, ect)
    protoIndex*: Natural ## The index of the item's prototype
    amount*: Positive ## The amount of the item in the inventory
    name*: string ## The name of the item, if different than the default one
    durability*: ItemsDurability ## The current durability of the item
    price*: Natural ## The price for which the item was bought

  MobAttributeRecord* = object
    ## Used to store information about the crew member's attributes
    level*: range[1..50] ## The level of the attribute
    experience*: Natural ## The amount of experience in the attribute

  SkillInfo* = object
    ## Used to store information about the crew member's skills
    index*: Natural      ## The index of the skill
    level*: SkillRange   ## The level of the skill
    experience*: Natural ## The amount of the experience in the skill

  MemberData* = object
    ## Used to store information about the crew member
    attributes*: seq[MobAttributeRecord] ## The member's attributes
    skills*: seq[SkillInfo]              ## The member's skills
    name*: string                        ## The member's name
    gender*: char                        ## The member's gender
    health*: SkillRange                  ## The member's health points
    tired*: range[0..150]                ## The member's tiredness level
    hunger*: SkillRange                  ## The member's hunger level
    thirst*: SkillRange                  ## The member's thirst level
    order*: CrewOrders                   ## The current order of the member
    previousOrder*: CrewOrders           ## The previous order of the member
    orderTime*: int                      ## The amount of minutes to next check in the order
    orders*: array[1..12, Natural]       ## The orders priorities for the member
    inventory*: seq[InventoryData]       ## The inventory of the member
    equipment*: array[EquipmentLocations, int] ## The equipment of the member
    payment*: AttributesArray            ## The payment information for the member
    contractLength*: int                 ## The length of the contract with the member
    morale*: AttributesArray             ## The morale information for the member
    loyalty*: SkillRange                 ## The loyalty level of the member
    homeBase*: BasesRange                ## The index of the home base
    faction*: string                     ## The faction index to which the member belongs

  ShipRecord* = object
    ## Used to store information about ships
    name*: string              ## The name of the ship
    skyX*: MapXRange           ## The X position of the ship on the map
    skyY*: MapYRange           ## The Y position of the ship on the map
    speed*: ShipSpeed          ## The current setting for the ship's speed
    modules*: seq[ModuleData]  ## The list of modules installed on the ship
    cargo*: seq[InventoryData] ## The list of items in the ship's cargo
    crew*: seq[MemberData]     ## The list of the crew members of the ship
    upgradeModule*: int        ## The index of the currently upgraded module
    destinationX*: range[0..MapXRange.high] ## The X position to which the ship goes
    destinationY*: range[0..MapYRange.high] ## The Y position to which the ship goes
    repairModule*: int         ## The index of module which will be repaired as first
    description*: string       ## The description of the ship
    homeBase*: Natural         ## The index of the home base of the ship

  ReputationRanges* = object
    ## Used to store reputation ranges for relation with other factions
    min*: ReputationRange ## Minimal reputation with the selected faction
    max*: ReputationRange ## Maximal reputation with the selected faction

  RelationsData* = object
    ## Used to store data about relation with other faction
    reputation*: ReputationRanges ## Values of min and max reputation with the faction
    friendly*: bool ## If true, the selected faction is friendly towards the faction

  CareerData* = object
    ## Used to store data about careers available for the faction
    shipIndex*: Positive ## The index of the starting ship prototype for the career
    playerIndex*: string ## The index of the starting mob prototype as the player character
    description*: string ## The description of the career
    name*: string        ## The name of the career

  FactionData* = object
    ## Used to store data about the selected faction
    name*: string        ## The name of the faction
    memberName*: string  ## The name of members of the faction
    pluralMemberName*: string ## The name for plural amount of members of the faction
    spawnChance*: Natural ## The chance of the spawn for a base of the selected faction
    population*: AttributesArray ## The min and max population for new bases of the faction
    namesType*: NamesTypes ## The type of names for the mobs, bases and ships of the faction
    relations*: Table[string, RelationsData] ## The faction's relations with other factions
    description*: string ## The description of the faction
    foodTypes*: seq[string] ## The types of items used as food for the faction's members
    drinksTypes*: seq[string] ## The types of items used as drinks for the faction's members
    healingTools*: string ## The type of items used as healing tools for the faction's members
    healingSkill*: Natural ## The skill used as healing skill for the faction's members
    flags*: seq[string]  ## Various flags set for the faction
    careers*: Table[string, CareerData] ## The list of available careers for the faction
    baseIcon*: Natural   ## The icon used as icon for the faction's bases on the map
    basesTypes*: Table[string, Positive] ## The list of available bases types for the faction
    weaponSkill*: Natural ## The skill used as prefered weapon skill for the faction

  ObjectData* = object
    ## Used to store information about items
    name*: string        ## The name of the item
    weight*: Positive    ## The weight of the item
    itemType*: string    ## The type of the item
    price*: Natural      ## The base price of the item in bases
    value*: array[1..5, int] ## Various data related to the item (damage for ammo, etc.)
    showType*: string    ## The item's type to show to the player instead of the itemType
    description*: string ## The description of the item
    reputation*: ReputationRange ## The minumal reputation which is needed to buy that item

  RecruitData* = object
    ## Used to store information about the recruit in bases
    attributes*: seq[MobAttributeRecord] ## The recruit's attributes
    skills*: seq[SkillInfo]              ## The recruit's skills
    name*: string                        ## The recruit's name
    gender*: char                        ## The recruit's gender
    price*: Positive                     ## The cost of hire of the recruit
    inventory*: seq[InventoryData]       ## The inventory of the recruit
    equipment*: array[EquipmentLocations, int] ## The equipment of the recruit
    payment*: Positive                   ## The payment information for the recruit
    homeBase*: BasesRange                ## The index of the home base
    faction*: string                     ## The faction index to which the recruit belongs

  BaseCargo* = object
    ## Used to store information about items in bases cargo
    protoIndex*: Natural         ## The index of the item's prototype
    amount*: Positive            ## The amount of the item in the inventory
    durability*: ItemsDurability ## The current durability of the item
    price*: Natural              ## The price for which the item was bought

  DateRecord* = object
    ## Used to store the game's time
    year*: range[0..4_000_000] ## The game's year
    month*: range[0..24]       ## The game's month
    day*: range[0..62]         ## The game's day
    hour*: range[0..48]        ## The game's hour
    minutes*: range[0..120]    ## The game's minutes

  ReputationData* = object
    ## Used to store information about the level of the player's reputation
    level*: ReputationRange ## The level of the reputation
    experience*: Natural    ## The current experience gained in the reputation

  MissionData* = object
    ## Used to store information about missions
    time*: Positive                    ## The amount of minutes to finish the mission
    targetX*: range[0..MapXRange.high] ## The X position of the target on the map
    targetY*: range[0..MapYRange.high] ## The Y position of the target on the map
    reward*: Positive                  ## The amount of money as the reward for the mission
    startBase*: BasesRange             ## The index of the starting base for the mission
    finished*: bool ## If true, the mission is ready to return, otherwise false
    multiplier*: RewardMultiplier ## The multiplier for the mission reward money and reputation
    case mType*: MissionsTypes
    of deliver:
      itemIndex*: Natural              ## The index of the proto item to deliver
    of passenger:
      data*: Natural ## The minumum quality of the cabin (in bases) or passenger index (in accepted)
    of destroy:
      shipIndex*: Natural              ## The index of the prototype ship to destroy
    else:
      target*: Natural                 ## The target for the mission (ship, item)

  BaseRecord* = object
    ## Used to store information about bases
    name*: string               ## The name of the base
    visited*: DateRecord        ## The date when the base was last visited
    skyX*: MapXRange            ## The X position of the base on the map
    skyY*: MapYRange            ## The Y position of the base on the map
    baseType*: string           ## The type of the base
    population*: Natural        ## The amount of people living in the base
    recruitDate*: DateRecord    ## The date when recruits were last checked
    recruits*: seq[RecruitData] ## The list of recruits available in the base
    known*: bool                ## If true, the base is known to the player, otherwise false
    askedForBases*: bool        ## If true, the player asked for other bases in the base
    askedForEvents*: DateRecord ## The date when the player asked for event last time
    reputation*: ReputationData ## The player's reputation in the base
    missionsDate*: DateRecord   ## The date when the player last checked missions in the base
    missions*: seq[MissionData] ## The list of available missions in the base
    owner*: string              ## The index of faction which owe the base
    cargo*: seq[BaseCargo]      ## The base's cargo
    size*: BasesSize            ## The size of the base

  BaseModuleData* = object
    ## Used to store information about prototypes of ships' modules
    name*: string            ## The name of the module
    mType*: ModuleType       ## The type of the module
    weight*: Natural         ## The weight of the module
    value*: int              ## Additional data for the module, for engines it is power
    maxValue*: int           ## Additional data for the mode, for guns it is damage
    durability*: int         ## The base durability of the module
    repairMaterial*: string  ## The index of the material used to repair the module
    repairSkill*: Positive   ## The index of the skill used to repair the module
    price*: Natural          ## The base price of the module in shipyards
    installTime*: Positive   ## The amount of time needed to install the module
    unique*: bool            ## If true, only one that module can be installed on the ship
    size*: range[1..10]      ## The size of the module
    description*: string     ## The description of the module
    maxOwners*: range[0..10] ## The amount of users of the module
    speed*: int              ## How fast the gun shoots in the combat
    reputation*: ReputationRange ## The minumum amount of reputation needed for buy the module

  CraftData* = object
    ## Used to store information about crafting recipes
    materialTypes*: seq[string] ## The list of materials types used in crafting
    materialAmounts*: seq[Positive] ## The list of materials amount used in crafting
    resultIndex*: Natural       ## The index of proto item which is the result of the recipe
    resultAmount*: Natural      ## The amount of items produced by one recipe
    workplace*: ModuleType      ## The type of ship's module used as a workshop for the recipe
    skill*: Natural             ## The index of the skill used in crafting
    time*: Positive             ## The amount of minutes needed to finish the recipe
    difficulty*: Positive       ## The difficulty level of the recipe
    tool*: string               ## The type of item used as a tool in crafting
    reputation*: ReputationRange ## The minimal amount of reputation needed to buy the recipe in bases
    toolQuality*: Positive      ## The minimal quality of tool used in crafting

  GoalData* = object
    ## Used to store information about the in-game goals
    index*: string       ## The index of the goal prototype
    goalType*: GoalTypes ## The type of the goal
    amount*: Natural     ## The amount of targets needed for finishe the goal
    targetIndex*: string ## The index of the target needed for finish the goal. If empty
                         ## means all targets of the selected type (bases, ships, etc.)
    multiplier*: Positive ## The muliplier for points awarded for finishing the goal

  MessageData* = object
    ## Used to store data about the game's messages
    message*: string     ## The message itself
    kind*: MessageType   ## The type of message
    color*: MessageColor ## The color used to show the message

  EventData* = object
    ## Used to store data about an event
    skyX*: MapXRange         ## The X coordinate of the event on the map
    skyY*: MapYRange         ## The Y coordinate of the event on the map
    time*: Positive          ## The time in minutes by how long the event will be available
    case eType*: EventsTypes ## The type of the event
    of doublePrice:
      itemIndex*: int        ## The index of the prototype item used by the event
    of attackOnBase, enemyShip, enemyPatrol, trader, friendlyShip:
      shipIndex*: int        ## The index of the prototype ship used by the event
    else:
      data*: int             ## General data of the event

  MobInventoryRecord* = object
    ## Used to store data about the inventory of the mob's prototype
    protoIndex*: Natural ## The index of the item's prototype
    minAmount*: Natural  ## The minimal amount of the item
    maxAmount*: Natural  ## The maximum amount of the item

  ShipBonusData* = object
    ## Used to store data for proto ships bonuses, like accuracy, evasion, etc
    minValue*: Natural ## The minimum value of the bonus
    maxValue*: Natural ## The maximum value of the bonus

  ProtoMemberData* = object
    ## Used to store data about the crew member in prototypes of ships
    protoIndex*: Positive ## The index of mob's prototype which will be used to create the crew member
    minAmount*: Positive ## The minimum amount of the mob as crew members on the ship
    maxAmount*: Natural ## The maximum amount of the mob as crew members on the ship

  ProtoShipData* = object
    ## Used to store data about the prototype of the ship
    name*: string                   ## The name of the prototype
    modules*: seq[Positive]         ## The list of installed modules
    accuracy*: ShipBonusData        ## The accuracy bonus for the ship
    combatAi*: ShipCombatAi         ## The combat behaviour of the ship
    evasion*: ShipBonusData         ## The evasion bonus for the ship
    loot*: ShipBonusData            ## The amount of money looted from the ship
    perception*: ShipBonusData      ## The perception bonus for the ship
    cargo*: seq[MobInventoryRecord] ## The cargo of the ship
    combatValue*: Positive ## The combat strength of the ship (used to generate enemies)
    crew*: seq[ProtoMemberData]     ## The crew of the ship
    description*: string            ## The ship's description
    owner*: string                  ## The faction to which the ship belongs
    knownRecipes*: seq[string] ## The list of known recipes (used only by the player's ship)

  ProtoMobRecord* = object
    ## Used to store data about mobs prototypes
    attributes*: seq[MobAttributeRecord] ## The mob's attributes
    skills*: seq[SkillInfo]              ## The mob's skills
    order*: CrewOrders                   ## The current order of the mob
    priorities*: array[1..12, Natural]   ## The orders priorities of the mob
    inventory*: seq[MobInventoryRecord]  ## The inventory of the mob
    equipment*: array[EquipmentLocations, int] ## The equipment of the mob

# Temporary code for interfacing with Ada

  AdaInventoryData* = object
    protoIndex*: cint
    amount*: cint
    name*: cstring
    durability*: cint
    price*: cint

  AdaMemberData* = object
    attributes*: array[1..16, array[2, cint]]
    skills*: array[1..64, array[3, cint]]
    name*: cstring
    gender*: char
    health*: cint
    tired*: cint
    hunger*: cint
    thirst*: cint
    order*: cint
    previousOrder*: cint
    orderTime*: cint
    orders*: array[1..12, cint]
    equipment*: array[0..6, cint]
    payment*: array[1..2, cint]
    contractLength*: cint
    morale*: array[1..2, cint]
    loyalty*: cint
    homeBase*: cint
    faction*: cstring

proc inventoryToNim*(inventory: array[128, AdaInventoryData]): seq[
    InventoryData] =
  for item in inventory:
    if item.protoIndex == 0:
      break
    result.add(y = InventoryData(protoIndex: item.protoIndex,
        amount: item.amount, name: $item.name, durability: item.durability,
        price: item.price))

proc inventoryToAda*(inventory: seq[InventoryData]): array[128,
    AdaInventoryData] =
  for i in 0..127:
    if i < inventory.len:
      result[i] = AdaInventoryData(protoIndex: inventory[i].protoIndex.cint,
          amount: inventory[i].amount.cint, name: inventory[i].name.cstring,
          durability: inventory[i].durability.cint, price: inventory[i].price.cint)
    else:
      result[i] = AdaInventoryData(protoIndex: 0)

func adaMemberToNim*(adaMember: AdaMemberData): MemberData {.raises: [], tags: [].} =
  result = MemberData(name: $adaMember.name, gender: adaMember.gender,
      health: adaMember.health, tired: adaMember.tired,
      hunger: adaMember.hunger, thirst: adaMember.thirst,
      order: adaMember.order.CrewOrders,
      previousOrder: adaMember.previousOrder.CrewOrders,
      contractLength: adaMember.contractLength, loyalty: adaMember.loyalty,
      homeBase: adaMember.homeBase, faction: $adaMember.faction)
  for index, order in adaMember.orders.pairs:
    result.orders[index] = order
  for index, item in adaMember.equipment.pairs:
    result.equipment[index.EquipmentLocations] = item - 1
  for attribute in adaMember.attributes:
    if attribute[0] == 0:
      break
    result.attributes.add(y = MobAttributeRecord(level: attribute[0],
        experience: attribute[1]))
  for skill in adaMember.skills:
    if skill[0] == 0:
      break
    result.skills.add(y = SkillInfo(index: skill[0], level: skill[1],
        experience: skill[2]))
  result.payment = [adaMember.payment[1].Natural, adaMember.payment[2].Natural]
  result.morale = [adaMember.morale[1].Natural, adaMember.morale[2].Natural]

func adaMemberFromNim*(member: MemberData): AdaMemberData {.raises: [], tags: [].} =
  result = AdaMemberData()
  for attribute in result.attributes.mitems:
    attribute = [0.cint, 0.cint]
  for index, attribute in member.attributes.pairs:
    result.attributes[index + 1] = [attribute.level.cint,
        attribute.experience.cint]
  for skill in result.skills.mitems:
    skill = [0.cint, 0.cint, 0.cint]
  for index, skill in member.skills.pairs:
    result.skills[index + 1] = [skill.index.cint, skill.level.cint,
        skill.experience.cint]
  result.name = member.name.cstring
  result.gender = member.gender
  result.health = member.health.cint
  result.tired = member.tired.cint
  result.hunger = member.hunger.cint
  result.thirst = member.thirst.cint
  result.order = member.order.ord.cint
  result.previousOrder = member.previousOrder.ord.cint
  result.orderTime = member.orderTime.cint
  for index, order in member.orders.pairs:
    result.orders[index] = order.ord.cint
  for index, item in member.equipment:
    result.equipment[index.ord.cint] = item.cint
  result.payment = [1: member.payment[1].cint, 2: member.payment[2].cint]
  result.contractLength = member.contractLength.cint
  result.morale = [1: member.morale[1].cint, 2: member.morale[2].cint]
  result.loyalty = member.loyalty.cint
  result.homeBase = member.homeBase
  result.faction = member.faction.cstring

