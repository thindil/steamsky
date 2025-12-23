# Copyright 2022-2025 Bartek thindil Jasicki
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

## Provides various types for the game, like ships' data structure, etc.

import std/tables

type

  CrewNoSpaceError* = object of CatchableError
    ## Raised when there is no space for new item in crew member inventory

  CrewOrderError* = object of CatchableError
    ## Used to mark problems during giving orders to the crew members

  NoTraderError* = object of CatchableError
    ## Raised when there is no crew member assigned to talk

  NoFreeCargoError* = object of CatchableError
    ## Raised when there is no free space in the player's ship cargo

  NoMoneyInBaseError* = object of CatchableError
    ## Raised when there is not enough money in the base for trade

  NoMoneyError* = object of CatchableError
    ## Raised when the player doesn't have money to buy an item

  NotEnoughMoneyError* = object of CatchableError
    ## Raised when the player doesn't have enough money to buy an item

  CantBuyError* = object of CatchableError
    ## Raised when the item is not available for sale

  ReputationError* = object of CatchableError
    ## Raised when there is some problems with reputations

  NoFreeSpaceError* = object of CatchableError
    ## Raised when there is no space in a trader's cargo to sell an item

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

  BasePopulation* = enum
    ## The size of a base's population
    empty, small, medium, large

  ObjectQuality* = enum
    ## The quality of an item
    poor, low, normal, good, excellent, any

  CraftBonuses* = enum
    ## Special bonuses set during crafting items
    none = "None",
    lighter = "Lighter",
    moreDurable = "More durable"

  CraftMaluses* = enum
    ## Special maluses set during crafting items
    none = "None",
    heavier = "Heavier",
    lessDurable = "Less durable"

  MapXRange* = range[1..1_024] ## The size of the game map in X axis
  MapYRange* = range[1..1_024] ## The size of the game map in Y axis
  ItemsDurability* = range[0..101] ## The range of the items durability
  SkillRange* = range[0..100] ## The range of skills levels
  BasesRange* = range[1..1_024] ## The amount of bases in the game
  ExtendedBasesRange* = range[0..1_024] ## The amount of bases in the game with zero value
  ReputationRange* = range[-100..100] ## The range of possible reputation levels
  RewardMultiplier* = range[0.0..2.0] ## The range of multiplier for missions reward
  ExtendedNatural* = range[-1..Natural.high] ## Extended Natural range, with -1

  AttributesArray* = array[1..2, Natural] ## 1 - Attribute level, 2 - Attribute experience
  EquipmentArray* = array[EquipmentLocations, int] ## The equipment of mobs

  XmlAttribute* = string ## Used to read the game data from XML files
  FactionIndex* = string ## Used to store in-game factions indexes
  BaseType* = string ## Used to store types of bases
  ObjectName* = string ## Used to store names of objects (items)

  ModuleData* = object
    ## Used to store information about ships' modules
    ##
    ## * mType            - The type of the module
    ## * name             - The name of the module
    ## * protoIndex       - The index of the prototype module
    ## * weight           - The weight of the module
    ## * durability       - The current durability of the module
    ## * maxDurability    - The max durability of the module
    ## * owner            - The list of owners of the module
    ## * upgradeProgress  - The upgrade progess of the module
    ## * upgradeAction    - The current upgrade type for the module
    ## * fuelUsage        - The fuel usage for engines modules
    ## * power            - The power of the engines modules
    ## * disabled         - If true, the engine is disabled
    ## * cleanliness      - The cleanliness level of the cabin
    ## * quality          - The quality level of the cabin
    ## * gunIndex         - The index of the module used as gun in the turret
    ## * damage           - The damage of the gun
    ## * ammoIndex        - The index of item from ship's cargo used as ammunition
    ## * installedModules - The amount of installed modules in the hull
    ## * maxModules       - The max amount of modules which the hull can hold
    ## * craftingIndex    - The index of currently crafted recipe
    ## * craftingTime     - The amount of time needed to finish the order
    ## * craftingAmount   - How many times repeat the crafting order
    ## * craftingQuality  - The desired quality of crafted item
    ## * trainedSkill     - The index of trained skill
    ## * damage2          - The damage of the battering ram
    ## * coolingDown      - If true, the battering ram can't attack now
    ## * duration         - The duration bonus of the harpoon gun
    ## * harpoonIndex     - The index of item from ship's cargo used as harpoon
    ## * data             - Various data for module, depends on module
    name*: string
    protoIndex*: Natural
    weight*: Natural
    durability*: Natural
    maxDurability*: Natural
    owner*: seq[int]
    upgradeProgress*: int
    upgradeAction*: ShipUpgrade
    case mType*: ModuleType2
    of ModuleType2.engine:
      fuelUsage*: Positive
      power*: Positive
      disabled*: bool
    of ModuleType2.cabin:
      cleanliness*: Natural
      quality*: Natural
    of ModuleType2.turret:
      gunIndex*: int
    of ModuleType2.gun:
      damage*: Positive
      ammoIndex*: int
    of ModuleType2.hull:
      installedModules*: Natural
      maxModules*: Positive
    of ModuleType2.workshop:
      craftingIndex*: string
      craftingTime*: Natural
      craftingAmount*: Natural
      craftingQuality*: ObjectQuality
    of ModuleType2.trainingRoom:
      trainedSkill*: Natural
    of ModuleType2.batteringRam:
      damage2*: Positive
      coolingDown*: bool
    of ModuleType2.harpoonGun:
      duration*: Positive
      harpoonIndex*: int
    of ModuleType2.any:
      data*: array[1..3, int]
    else:
      discard

  InventoryData* = object
    ## Used to store information about items in various inventories (cargo, crew
    ## inventory, ect)
    ##
    ## * protoIndex - The index of the item's prototype
    ## * amount     - The amount of the item in the inventory
    ## * name       - The name of the item, if different than the default one
    ## * durability - The current durability of the item
    ## * price      - The price for which the item was bought
    ## * quality    - The quality of the item
    protoIndex*: Natural = 0
    amount*: Positive = 1
    name*: string
    durability*: ItemsDurability = 100
    price*: Natural = 0
    quality*: ObjectQuality = normal

  MobAttributeRecord* = object
    ## Used to store information about the crew member's attributes
    ##
    ## * level      - The level of the attribute
    ## * experience - The amount of experience in the attribute
    level*: range[1..50] = 1
    experience*: Natural = 0

  SkillInfo* = object
    ## Used to store information about the crew member's skills
    ##
    ## * index      - The index of the skill
    ## * level      - The level of the skill
    ## * experience - The amount of the experience in the skill
    index*: Natural = 0
    level*: SkillRange = 0
    experience*: Natural = 0

  MemberData* = object
    ## Used to store information about the crew member
    ##
    ## * attributes     - The member's attributes
    ## * skills         - The member's skills
    ## * name           - The member's name
    ## * gender         - The member's gender
    ## * health         - The member's health points
    ## * tired          - The member's tiredness level
    ## * hunger         - The member's hunger level
    ## * thirst         - The member's thirst level
    ## * order          - The current order of the member
    ## * previousOrder  - The previous order of the member
    ## * orderTime      - The amount of minutes to next check in the order
    ## * orders         - The orders priorities for the member
    ## * inventory      - The inventory of the member
    ## * equipment      - The equipment of the member
    ## * payment        - The payment information for the member
    ## * contractLength - The length of the contract with the member
    ## * morale         - The morale information for the member
    ## * loyalty        - The loyalty level of the member
    ## * homeBase       - The index of the home base
    ## * faction        - The faction index to which the member belongs
    attributes*: seq[MobAttributeRecord] = @[]
    skills*: seq[SkillInfo] = @[]
    name*: string = ""
    gender*: char = 'M'
    health*: SkillRange = 100
    tired*: range[0..150] = 0
    hunger*: SkillRange = 0
    thirst*: SkillRange = 0
    order*: CrewOrders = rest
    previousOrder*: CrewOrders = rest
    orderTime*: int = 15
    orders*: array[1..12, Natural]
    inventory*: seq[InventoryData] = @[]
    equipment*: EquipmentArray
    payment*: AttributesArray
    contractLength*: int = -1
    morale*: AttributesArray
    loyalty*: SkillRange = 100
    homeBase*: BasesRange = 1
    faction*: FactionIndex = ""

  ShipRecord* = object
    ## Used to store information about ships
    ##
    ## * name          - The name of the ship
    ## * skyX          - The X position of the ship on the map
    ## * skyY          - The Y position of the ship on the map
    ## * speed         - The current setting for the ship's speed
    ## * modules       - The list of modules installed on the ship
    ## * cargo         - The list of items in the ship's cargo
    ## * crew          - The list of the crew members of the ship
    ## * upgradeModule - The index of the currently upgraded module
    ## * destinationX  - The X position to which the ship goes
    ## * destinationY  - The Y position to which the ship goes
    ## * repairModule  - The index of module which will be repaired as first
    ## * description   - The description of the ship
    ## * homeBase      - The index of the home base of the ship
    name*: string = ""
    skyX*: MapXRange = 1
    skyY*: MapYRange = 1
    speed*: ShipSpeed = fullSpeed
    modules*: seq[ModuleData] = @[]
    cargo*: seq[InventoryData] = @[]
    crew*: seq[MemberData] = @[]
    upgradeModule*: int = -1
    destinationX*: range[0..MapXRange.high] = 0
    destinationY*: range[0..MapYRange.high] = 0
    repairModule*: int = -1
    description*: string = ""
    homeBase*: Natural = 0

  ReputationRanges* = object
    ## Used to store reputation ranges for relation with other factions
    ##
    ## * min - Minimal reputation with the selected faction
    ## * max - Maximal reputation with the selected faction
    min*: ReputationRange
    max*: ReputationRange

  RelationsData* = object
    ## Used to store data about relation with other faction
    ##
    ## * reputation - Values of min and max reputation with the faction
    ## * friendly   - If true, the selected faction is friendly towards the faction
    reputation*: ReputationRanges
    friendly*: bool

  CareerData* = object
    ## Used to store data about careers available for the faction
    ##
    ## * shipIndex   - The index of the starting ship prototype for the career
    ## * playerIndex - The index of the starting mob prototype as the player character
    ## * description - The description of the career
    ## * name        - The name of the career
    shipIndex*: Positive
    playerIndex*: string
    description*: string
    name*: string

  FactionData* = object
    ## Used to store data about the selected faction
    ##
    ## * name             - The name of the faction
    ## * memberName       - The name of members of the faction
    ## * pluralMemberName - The name for plural amount of members of the faction
    ## * spawnChance      - The chance of the spawn for a base of the selected faction
    ## * population       - The min and max population for new bases of the faction
    ## * namesType        - The type of names for the mobs, bases and ships of the faction
    ## * relations        - The faction's relations with other factions
    ## * description      - The description of the faction
    ## * foodTypes        - The types of items used as food for the faction's members
    ## * drinksTypes      - The types of items used as drinks for the faction's members
    ## * healingTools     - The type of items used as healing tools for the faction's members
    ## * healingSkill     - The skill used as healing skill for the faction's members
    ## * flags            - Various flags set for the faction
    ## * careers          - The list of available careers for the faction
    ## * baseIcon         - The icon used as icon for the faction's bases on the map
    ## * basesTypes       - The list of available bases types for the faction
    ## * weaponSkill      - The skill used as prefered weapon skill for the faction
    name*: string
    memberName*: string
    pluralMemberName*: string
    spawnChance*: Natural
    population*: AttributesArray
    namesType*: NamesTypes
    relations*: Table[string, RelationsData]
    description*: string
    foodTypes*: seq[string]
    drinksTypes*: seq[string]
    healingTools*: string
    healingSkill*: Natural
    flags*: seq[string]
    careers*: Table[string, CareerData]
    baseIcon*: Natural
    basesTypes*: Table[string, Positive]
    weaponSkill*: Natural

  ObjectData* = object
    ## Used to store information about items
    ##
    ## * name        - The name of the item
    ## * weight      - The weight of the item
    ## * itemType    - The type of the item
    ## * price       - The base price of the item in bases
    ## * value       - Various data related to the item (damage for ammo, etc.)
    ## * showType    - The item's type to show to the player instead of the itemType
    ## * description - The description of the item
    ## * reputation  - The minumal reputation which is needed to buy that item
    name*: ObjectName
    weight*: Positive
    itemType*: string
    price*: Natural
    value*: array[1..5, int]
    showType*: string
    description*: string
    reputation*: ReputationRange

  RecruitItem* = object
    ## Used to store information about a recruit's inventory
    ##
    ## * index   - the index of the item's prototype
    ## * quality - the quality of the item
    index*: Positive = 1
    quality*: ObjectQuality = normal

  RecruitData* = object
    ## Used to store information about the recruit in bases
    ##
    ## * attributes - The recruit's attributes
    ## * skills     - The recruit's skills
    ## * name       - The recruit's name
    ## * gender     - The recruit's gender
    ## * price      - The cost of hire of the recruit
    ## * inventory  - The inventory of the recruit
    ## * equipment  - The equipment of the recruit
    ## * payment    - The payment information for the recruit
    ## * homeBase   - The index of the home base
    ## * faction    - The faction index to which the recruit belongs
    attributes*: seq[MobAttributeRecord]
    skills*: seq[SkillInfo]
    name*: string
    gender*: char
    price*: Positive = 1
    inventory*: seq[RecruitItem]
    equipment*: EquipmentArray
    payment*: Positive = 1
    homeBase*: BasesRange = 1
    faction*: string

  BaseCargo* = object
    ## Used to store information about items in bases cargo
    ##
    ## * protoIndex - The index of the item's prototype
    ## * amount     - The amount of the item in the inventory
    ## * durability - The current durability of the item
    ## * price      - The price for which the item was bought
    ## * quality    - The quality of the item
    protoIndex*: Natural
    amount*: Natural
    durability*: ItemsDurability
    price*: Natural
    quality*: ObjectQuality

  DateRecord* = object
    ## Used to store the game's time
    ##
    ## * year    - The game's year
    ## * month   - The game's month
    ## * day     - The game's day
    ## * hour    - The game's hour
    ## * minutes - The game's minutes
    year*: range[0..4_000_000] = 0
    month*: range[0..24] = 0
    day*: range[0..62] = 0
    hour*: range[0..48] = 0
    minutes*: range[0..120] = 0

  ReputationData* = object
    ## Used to store information about the level of the player's reputation
    ##
    ## * level      - The level of the reputation
    ## * experience - The current experience gained in the reputation
    level*: ReputationRange
    experience*: Natural

  MissionData* = object
    ## Used to store information about missions
    ##
    ## * mType      - The type of the mission
    ## * time       - The amount of minutes to finish the mission
    ## * targetX    - The X position of the target on the map
    ## * targetY    - The Y position of the target on the map
    ## * reward     - The amount of money as the reward for the mission
    ## * startBase  - The index of the starting base for the mission
    ## * finished   - If true, the mission is ready to return, otherwise false
    ## * multiplier - The multiplier for the mission reward money and reputation
    ## * itemIndex  - The index of the proto item to deliver
    ## * data       - The minumum quality of the cabin (in bases) or passenger index (in accepted)
    ## * shipIndex  - The index of the prototype ship to destroy
    ## * target     - The target for the mission (ship, item)
    time*: Positive = 1
    targetX*: range[0..MapXRange.high]
    targetY*: range[0..MapYRange.high]
    reward*: Positive = 1
    startBase*: BasesRange = 1
    finished*: bool
    multiplier*: RewardMultiplier
    case mType*: MissionsTypes
    of deliver:
      itemIndex*: Natural
    of passenger:
      data*: Natural
    of destroy:
      shipIndex*: Natural
    else:
      target*: Natural

  BaseRecord* = object
    ## Used to store information about bases
    ##
    ## * name           - The name of the base
    ## * visited        - The date when the base was last visited
    ## * skyX           - The X position of the base on the map
    ## * skyY           - The Y position of the base on the map
    ## * baseType       - The type of the base
    ## * population     - The amount of people living in the base
    ## * recruitDate    - The date when recruits were last checked
    ## * recruits       - The list of recruits available in the base
    ## * known          - If true, the base is known to the player, otherwise false
    ## * askedForBases  - If true, the player asked for other bases in the base
    ## * askedForEvents - The date when the player asked for event last time
    ## * reputation     - The player's reputation in the base
    ## * missionsDate   - The date when the player last checked missions in the base
    ## * missions       - The list of available missions in the base
    ## * owner          - The index of faction which owe the base
    ## * cargo          - The base's cargo
    ## * size           - The size of the base
    name*: string
    visited*: DateRecord
    skyX*: MapXRange
    skyY*: MapYRange
    baseType*: BaseType
    population*: Natural
    recruitDate*: DateRecord
    recruits*: seq[RecruitData]
    known*: bool
    askedForBases*: bool
    askedForEvents*: DateRecord
    reputation*: ReputationData
    missionsDate*: DateRecord
    missions*: seq[MissionData]
    owner*: FactionIndex
    cargo*: seq[BaseCargo]
    size*: BasesSize

  BaseModuleData* = object
    ## Used to store information about prototypes of ships' modules
    ##
    ## * name           - The name of the module
    ## * mType          - The type of the module
    ## * weight         - The weight of the module
    ## * value          - Additional data for the module, for engines it is power
    ## * maxValue       - Additional data for the mode, for guns it is damage
    ## * durability     - The base durability of the module
    ## * repairMaterial - The index of the material used to repair the module
    ## * repairSkill    - The index of the skill used to repair the module
    ## * price          - The base price of the module in shipyards
    ## * installTime    - The amount of time needed to install the module
    ## * unique         - If true, only one that module can be installed on the ship
    ## * size           - The size of the module
    ## * description    - The description of the module
    ## * maxOwners      - The amount of users of the module
    ## * speed          - How fast the gun shoots in the combat
    ## * reputation     - The minumum amount of reputation needed for buy the module
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

  CraftData* = object
    ## Used to store information about crafting recipes
    ##
    ## * materialTypes   - The list of materials types used in crafting
    ## * materialAmounts - The list of materials amount used in crafting
    ## * resultIndex     - The index of proto item which is the result of the recipe
    ## * resultAmount    - The amount of items produced by one recipe
    ## * workplace       - The type of ship's module used as a workshop for the recipe
    ## * skill           - The index of the skill used in crafting
    ## * time            - The amount of minutes needed to finish the recipe
    ## * difficulty      - The difficulty level of the recipe
    ## * tool            - The type of item used as a tool in crafting
    ## * reputation      - The minimal amount of reputation needed to buy the recipe in bases
    ## * toolQuality     - The minimal quality of tool used in crafting
    materialTypes*: seq[string] = @[]
    materialAmounts*: seq[Positive] = @[]
    resultIndex*: Natural = 0
    resultAmount*: Natural = 0
    workplace*: ModuleType = alchemyLab
    skill*: Natural = 0
    time*: Positive = 1
    difficulty*: Positive = 1
    tool*: string = ""
    reputation*: ReputationRange = 0
    toolQuality*: Positive = 1

  GoalData* = object
    ## Used to store information about the in-game goals
    ##
    ## * index       - The index of the goal prototype
    ## * goalType    - The type of the goal
    ## * amount      - The amount of targets needed for finishe the goal
    ## * targetIndex - The index of the target needed for finish the goal. If empty
    ##                 means all targets of the selected type (bases, ships, etc.)
    ## * multiplier  - The muliplier for points awarded for finishing the goal
    index*: string
    goalType*: GoalTypes
    amount*: Natural
    targetIndex*: string
    multiplier*: Positive

  MessageData* = object
    ## Used to store data about the game's messages
    ##
    ## * message - The message itself
    ## * kind    - The type of message
    ## * color   - The color used to show the message
    message*: string
    kind*: MessageType
    color*: MessageColor

  EventData* = object
    ## Used to store data about an event
    ##
    ## * eType     - The type of the event
    ## * skyX      - The X coordinate of the event on the map
    ## * skyY      - The Y coordinate of the event on the map
    ## * time      - The time in minutes by how long the event will be available
    ## * itemIndex - The index of the prototype item used by the event
    ## * shipIndex - The index of the prototype ship used by the event
    ## * data      - General data of the event
    skyX*: MapXRange = 1
    skyY*: MapYRange = 1
    time*: Positive = 1
    case eType*: EventsTypes
    of doublePrice:
      itemIndex*: int
    of attackOnBase, enemyShip, enemyPatrol, trader, friendlyShip:
      shipIndex*: int
    else:
      data*: int

  MobInventoryRecord* = object
    ## Used to store data about the inventory of the mob's prototype
    ##
    ## * protoIndex - The index of the item's prototype
    ## * minAmount  - The minimal amount of the item
    ## * maxAmount  - The maximum amount of the item
    protoIndex*: Natural
    minAmount*: Natural
    maxAmount*: Natural

  ShipBonusData* = object
    ## Used to store data for proto ships bonuses, like accuracy, evasion, etc
    ##
    ## * minValue - The minimum value of the bonus
    ## * maxValue - The maximum value of the bonus
    minValue*: Natural
    maxValue*: Natural

  ProtoMemberData* = object
    ## Used to store data about the crew member in prototypes of ships
    ##
    ## * protoIndex - The index of mob's prototype which will be used to create the crew member
    ## * minAmount  - The minimum amount of the mob as crew members on the ship
    ## * maxAmount  - The maximum amount of the mob as crew members on the ship
    protoIndex*: Positive = 1
    minAmount*: Positive = 1
    maxAmount*: Natural = 0

  ProtoShipData* = object
    ## Used to store data about the prototype of the ship
    ##
    ## * name         - The name of the prototype
    ## * modules      - The list of installed modules
    ## * accuracy     - The accuracy bonus for the ship
    ## * combatAi     - The combat behaviour of the ship
    ## * evasion      - The evasion bonus for the ship
    ## * loot         - The amount of money looted from the ship
    ## * perception   - The perception bonus for the ship
    ## * cargo        - The cargo of the ship
    ## * combatValue  - The combat strength of the ship (used to generate enemies)
    ## * crew         - The crew of the ship
    ## * description  - The ship's description
    ## * owner        - The faction to which the ship belongs
    ## * knownRecipes - The list of known recipes (used only by the player's ship)
    ## * reputation   - The level of reputation at which the ship will appear.
    ##                  an enemy, it is negative value, for friendly positive
    name*: string
    modules*: seq[Positive]
    accuracy*: ShipBonusData
    combatAi*: ShipCombatAi
    evasion*: ShipBonusData
    loot*: ShipBonusData
    perception*: ShipBonusData
    cargo*: seq[MobInventoryRecord]
    combatValue*: Positive
    crew*: seq[ProtoMemberData]
    description*: string
    owner*: FactionIndex
    knownRecipes*: seq[string]
    reputation*: Natural

  ProtoMobRecord* = object
    ## Used to store data about mobs prototypes
    ##
    ## * attributes - The mob's attributes
    ## * skills     - The mob's skills
    ## * order      - The current order of the mob
    ## * priorities - The orders priorities of the mob
    ## * inventory  - The inventory of the mob
    ## * equipment  - The equipment of the mob
    attributes*: seq[MobAttributeRecord]
    skills*: seq[SkillInfo]
    order*: CrewOrders
    priorities*: array[1..12, Natural]
    inventory*: seq[MobInventoryRecord]
    equipment*: EquipmentArray

  EnemyRecord* = object
    ## Used to store information about the enemy
    ##
    ## * ship            - The enemy's ship
    ## * accuracy        - The enemy's bonus to accuracy
    ## * distance        - The distance to the enemy's ship in combat
    ## * combatAi        - The type of enemy's AI
    ## * evasion         - The enemy's bonus to evasion
    ## * loot            - The amount of money looted from the enemy after win
    ##                     the fight with it
    ## * perception      - The enemy's bonus to perception
    ## * harpoonDuration - How long in combat rounds the enemy's ship will be
    ##                     stopped by the player's harpoon
    ## * guns            - The list of guns on the enemy's ship, 0 - gun index
    ##                     in ship modules list, 1 - gunner order, 2 - amount
    ##                     of shoots from the gun, value below zero means that
    ##                     gun shoot once per that amount of rounds
    ship*: ShipRecord
    accuracy*: Natural
    distance*: int
    combatAi*: ShipCombatAi
    evasion*: Natural
    loot*: Natural
    perception*: Natural
    harpoonDuration*: Natural
    guns*: seq[array[1..3, int]]
