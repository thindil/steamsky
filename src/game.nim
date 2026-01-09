# Copyright 2022-2026 Bartek thindil Jasicki
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

## Provides code related to update the global state of the game, like global
## variables, loading the game data, etc.

import std/[os, parsecfg, paths, streams, strutils, tables, xmlparser, xmltree]
import contracts, nimalyzer
import types

type
  DataAction* = enum
    ## Possible actions to do when loading game data
    add, update, remove

  SkillName* = string
    ## The name of a skill
  SkillDescription* = string
    ## The description of a skill
  SkillTool* = string
    ## The type of items used to train a skill
  AttributeName* = string
    ## The name of an attribute
  AttributeDescription* = string
    ## The description of an attribute
  VersionString = string
    ## The version of the game

  ToolQuality = object
    ## Store data related to quality of tools needed for train a skill
    ##
    ## * level   - The minimal level of a skill which need that quality of tool
    ## * quality - The level of quality of tool needed for training
    level: Natural
    quality: Natural

  SkillRecord* = object
    ## Store data releated to the skills
    ##
    ## * name         - The name of the skill
    ## * attribute    - The index of the attribute related to the skill
    ## * description  - The description of the skill
    ## * tool         - The type of items used to train the skill
    ## * toolsQuality - The quality of tool needed for training
    name: SkillName
    attribute: Natural
    description: SkillDescription
    tool: SkillTool
    toolsQuality: seq[ToolQuality]

  AttributeRecord* = object
    ## Store data related to the attributes
    ##
    ## * name        - The name of the attribute
    ## * description - The description of the attribute
    name: AttributeName
    description: AttributeDescription

{.push ruleOff: "objects".}
type
  DataLoadingError* = object of CatchableError
    ## Used to mark problems during loading the game data from files
  TradeNoFreeCargoError* = object of CatchableError
    ## Used to mark problems with free cargo space in the ship
{.push ruleOn: "objects".}

proc level*(tool: ToolQuality): Natural {.raises: [], tags: [], contractual.} =
  ## The getter of a field of ToolQuality type
  ##
  ## * tool - the ToolQuality object which field will be get
  ##
  ## Returns the value of the selected field
  tool.level

proc quality*(tool: ToolQuality): Natural {.raises: [], tags: [],
    contractual.} =
  ## The getter of a field of ToolQuality type
  ##
  ## * tool - the ToolQuality object which field will be get
  ##
  ## Returns the value of the selected field
  tool.quality

proc initToolQuality(level, quality: Natural): ToolQuality {.raises: [], tags: [
    ], contractual.} =
  ## Create a new data structure for the tool quality information
  ##
  ## * level   - the maximum level on which the tool is used
  ## * quality - the minimum quality of the used tool
  ##
  ## Returns the new structure with information about the selected data
  return ToolQuality(level: level, quality: quality)

proc name*(skill: SkillRecord): SkillName {.raises: [], tags: [],
    contractual.} =
  ## The getter of a field of SkillRecord type
  ##
  ## * skill - the SkillRecord object which field will be get
  ##
  ## Returns the value of the selected field
  skill.name

proc attribute*(skill: SkillRecord): Natural {.raises: [], tags: [],
    contractual.} =
  ## The getter of a field of SkillRecord type
  ##
  ## * skill - the SkillRecord object which field will be get
  ##
  ## Returns the value of the selected field
  skill.attribute

proc description*(skill: SkillRecord): SkillDescription {.raises: [], tags: [],
    contractual.} =
  ## The getter of a field of SkillRecord type
  ##
  ## * skill - the SkillRecord object which field will be get
  ##
  ## Returns the value of the selected field
  skill.description

proc tool*(skill: SkillRecord): SkillTool {.raises: [], tags: [],
    contractual.} =
  ## The getter of a field of SkillRecord type
  ##
  ## * skill - the SkillRecord object which field will be get
  ##
  ## Returns the value of the selected field
  skill.tool

proc toolsQuality*(skill: SkillRecord): seq[ToolQuality] {.raises: [], tags: [],
    contractual.} =
  ## The getter of a field of SkillRecord type
  ##
  ## * skill - the SkillRecord object which field will be get
  ##
  ## Returns the value of the selected field
  skill.toolsQuality

proc initSkillRecord(name: SkillName = ""; attribute: Natural = 1;
    description: SkillDescription = ""; tool: SkillTool = ""; toolsQuality: seq[
    ToolQuality] = @[]): SkillRecord {.raises: [], tags: [], contractual.} =
  ## Create a new data structure for the player's skill information
  ##
  ## * name         - the name of the skill
  ## * attribute    - the attribute associated with the skill
  ## * description  - the description of the skill
  ## * tool         - the type of items used as a tool to train the skill
  ## * toolsQuality - the list of tools' qualities needed for training
  ##
  ## Returns the new structure with information about the selected data
  return SkillRecord(name: name, attribute: attribute, description: description,
      tool: tool, toolsQuality: toolsQuality)

proc name*(attribute: AttributeRecord): AttributeName {.raises: [], tags: [],
    contractual.} =
  ## The getter of a field of AttributeRecord type
  ##
  ## * attribute - the AttributeRecord object which field will be get
  ##
  ## Returns the value of the selected field
  attribute.name

proc description*(attribute: AttributeRecord): AttributeName {.raises: [],
    tags: [], contractual.} =
  ## The getter of a field of AttributeRecord type
  ##
  ## * attribute - the AttributeRecord object which field will be get
  ##
  ## Returns the value of the selected field
  attribute.description

proc initAttributeRecord(name: AttributeName;
    description: AttributeDescription): AttributeRecord {.raises: [], tags: [],
    contractual.} =
  ## Create a new data structure for the attribute information
  ##
  ## * name        - the name of the attribute
  ## * description - the description of the attribute
  ##
  ## Returns the new structure with information about the selected data
  return AttributeRecord(name: name, description: description)

const
  defaultItemDurability*: ItemsDurability = 100 ## Default durability for the new items
  startDate*: DateRecord = DateRecord(year: 1600, month: 3, day: 1, hour: 8,
      minutes: 1) ## The start date for a new game
  noDate*: DateRecord = DateRecord(year: 0, month: 0, day: 0, hour: 0,
      minutes: 0) ## The empty, not set game date
  gameVersion*: VersionString = "Version: " & staticRead(
      filename = "../steamsky.nimble").newStringStream.loadConfig.getSectionValue(
      section = "", key = "version")
    ## The current version of the game

{.warning[UnsafeSetLen]: off.}
{.warning[UnsafeDefault]: off.}
var
  saveDirectory*: Path = ("data" & DirSep & "saves" &
      DirSep).Path
    ## The directory where the saved games and logs are stored
  modsDirectory*: Path = ("data" & DirSep & "mods" &
      DirSep).Path ## The directory where the game's modifications are stored
  dataDirectory*: Path = ("data" & DirSep).Path
    ## The directory where the game's data is stored
  docDirectory*: Path = ("doc" & DirSep).Path
    ## The directory where the game's documentation is stored
  themesDirectory*: string = "data" & DirSep & "themes" &
      DirSep
    ## The directory where the game's themes are stored
  moneyIndex*: Positive = 1
    ## The item's index of the item used as money in the game
  moneyName*: string = "Charcollum"
    ## The name of the item used as a money in the game
  skillsList*: Table[Positive, SkillRecord] = initTable[Positive, SkillRecord]()
    ## The list of all skill available in the game
  basesSyllablesPreList*: seq[string] = @[]
    ## The list of pre syllables for bases names
  basesSyllablesStartList*: seq[string] = @[]
    ## The list of start syllables for bases names
  basesSyllablesEndList*: seq[string] = @[]
    ## The list of end syllables for bases names
  basesSyllablesPostList*: seq[string] = @[]
    ## The list of post syllables for bases names
  malesSyllablesStartList*: seq[string] = @[]
    ## The list of start syllables for males names
  malesSyllablesMiddleList*: seq[string] = @[]
    ## The list of middle syllables for males names
  malesSyllablesEndList*: seq[string] = @[]
    ## The list of end syllables for males names
  malesVocalsList*: seq[string] = @[]
    ## The list of vocals for males names
  malesConsonantsList*: seq[string] = @[]
    ## The list of consonants for males names
  femalesSyllablesStartList*: seq[string] = @[]
    ## The list of start syllables for females names
  femalesSyllablesMiddleList*: seq[string] = @[]
    ## The list of middle syllables for females names
  femalesSyllablesEndList*: seq[string] = @[]
    ## The list of end syllables for females names
  femalesVocalsList*: seq[string] = @[]
    ## The list of vocals for female names
  shipsSyllablesStartList*: seq[string] = @[]
    ## The list of start syllables for ships names
  shipsSyllablesMiddleList*: seq[string] = @[]
    ## The list of middle syllables for ships names
  shipsSyllablesEndList*: seq[string] = @[]
    ## The list of end syllables for ships names
  attributesList*: seq[AttributeRecord] = @[]
    ## The list of all attributes available in the game
  itemsTypesList*: seq[string] = @[]
    ## The list of all types of items available in the game
  repairTools*: string = ""
    ## The type of item used to repair ships
  cleaningTools*: string = ""
    ## The type of item used to cleaning ships
  alchemyTools*: string = ""
    ## The type of item used as alchemy tools
  corpseIndex*: Positive = 1
    ## The index of item used as prototype for corpses
  missionItemsType*: string = ""
    ## The type of item used in missions
  fuelType*: string = ""
    ## The type of item used as fuel for ships
  tradersName*: string = ""
    ## The word used to mark traders ships in their names
  conditionIndex*: Natural = 0
    ## The index of condition attribute
  strengthIndex*: Natural = 0
    ## The index of strength attribute
  pilotingSkill*: Positive = 1
    ## The index of piloting skill
  engineeringSkill*: Positive = 1
    ## The index of engineering skill
  gunnerySkill*: Positive = 1
    ## The index of gunnery skill
  talkingSkill*: Positive = 1
    ## The index of talking skill
  perceptionSkill*: Positive = 1
    ## The index of perception skil
  headArmor*: string = ""
    ## The type of items used as head armor
  chestArmor*: string = ""
    ## The type of items used as chest armor
  armsArmor*: string = ""
    ## The type of items used as arms armor
  legsArmor*: string = ""
    ## The type of items used as legs armor
  shieldType*: string = ""
    ## The type of items used as shield
  weaponType*: string = ""
    ## The type of items used as weapon
  dodgeSkill*: Positive = 1
    ## The index of dodge skill
  unarmedSkill*: Positive = 1
    ## The index of unarmed combat skill
  factionsList*: Table[string, FactionData] = initTable[string, FactionData]()
    ## The list of all available factions in the game
  itemsList*: Table[Positive, ObjectData] = initTable[Positive, ObjectData]()
    ## The list of prototypes of all items availabla in the game
  modulesList*: Table[Positive, BaseModuleData] = initTable[Positive,
      BaseModuleData]()
    ## The list of prototypes of all ships' modules available in the game
  recipesList*: Table[string, CraftData] = initTable[string, CraftData]()
    ## The list of all available crafting recipes in the game
  goalsList*: OrderedTable[Positive, GoalData] = initOrderedTable[Positive,
      GoalData]()
    ## The list of available goals in the game
  playerCareer*: string = ""
    ## Index of the career of the player selected when starting a new game
  knownRecipes*: seq[string] = @[]
    ## The list of known recipes by the player
  messagesList*: seq[MessageData] = @[]
    ## The list of in-game messages
  eventsList*: seq[EventData] = @[]
    ## The list of available events in the game
  playerShip*: ShipRecord = ShipRecord(skyX: 1, skyY: 1)
    ## The player's ship's data
  npcShip*: ShipRecord = ShipRecord(skyX: 1, skyY: 1)
    ## The npc ship like enemy, trader, etc
  protoShipsList*: Table[Positive, ProtoShipData] = initTable[Positive,
      ProtoShipData]()
    ## The list of prototypes of ships available in the game
  protoMobsList*: Table[Positive, ProtoMobRecord] = initTable[Positive,
      ProtoMobRecord]()
    ## The list of prototypes of all mobs availabla in the game
  gameDate*: DateRecord = startDate
    ## The current time in the game
  traderCargo*: seq[BaseCargo] = @[]
    ## The current trader's ship's cargo
  harpoonDuration*: Natural = 0
    ## How long in combat rounds the player's ship will be stopped by an enemy's harpoon
  enemy*: EnemyRecord = EnemyRecord(ship: ShipRecord(skyX: 1, skyY: 1))
    ## The enemy information
{.warning[UnsafeDefault]: on.}
{.warning[UnsafeSetLen]: on.}

{.push ruleOff: "varDeclared".}
var skyBases*: array[BasesRange, BaseRecord]
  ## The list of all bases in the game
{.push ruleOn: "varDeclared".}

proc findSkillIndex*(skillName: string): Natural {.raises: [],
    tags: [], contractual.} =
  ## Get the index of the selected skill
  ##
  ## * skillName - the name of the skill which index will be looking for
  ##
  ## Returns the index of the selected skill or 0 if the skill not found
  for key, skill in skillsList:
    if skill.name == skillName:
      return key
  return 0

proc loadData*(fileName: Path) {.raises: [DataLoadingError],
    tags: [WriteIOEffect, ReadIOEffect, RootEffect], contractual.} =
  ## Load the game data
  ##
  ## * fileName - the name of the file with the game data to load
  require:
    ($fileName).len > 0
  body:

    proc findAttributeIndex(attributeName: string): int {.raises: [], tags: [],
        contractual.} =
      ## Find the index of the selected attribute
      ##
      ## * attributeName - the name of the attribute which index will be looking
      ##                   for
      ##
      ## Returns the index of the selected attribute or -1 if the attribute not found
      for key, attribute in attributesList:
        if attribute.name == attributeName:
          return key
      return -1

    let gameXml: XmlNode = try:
        loadXml(path = $fileName)
      except XmlError, ValueError, IOError, OSError, Exception:
        raise newException(exceptn = DataLoadingError,
            message = "Can't load game data file. Reason: " &
            getCurrentExceptionMsg())
    var skillIndex: Positive = 1
    for gameNode in gameXml:
      if gameNode.kind != xnElement:
        continue
      case gameNode.tag
      of "basessyllablepre":
        basesSyllablesPreList.add(y = gameNode.attr(name = "value"))
      of "basessyllablestart":
        basesSyllablesStartList.add(y = gameNode.attr(name = "value"))
      of "basessyllableend":
        basesSyllablesEndList.add(y = gameNode.attr(name = "value"))
      of "basessyllablepost":
        basesSyllablesPostList.add(y = gameNode.attr(name = "value"))
      of "malessyllablestart":
        malesSyllablesStartList.add(y = gameNode.attr(name = "value"))
      of "malessyllablemiddle":
        malesSyllablesMiddleList.add(y = gameNode.attr(name = "value"))
      of "malessyllableend":
        malesSyllablesEndList.add(y = gameNode.attr(name = "value"))
      of "malesvocal":
        malesVocalsList.add(y = gameNode.attr(name = "value"))
      of "malesconsonant":
        malesConsonantsList.add(y = gameNode.attr(name = "value"))
      of "femalessyllablestart":
        femalesSyllablesStartList.add(y = gameNode.attr(name = "value"))
      of "femalessyllablemiddle":
        femalesSyllablesMiddleList.add(y = gameNode.attr(name = "value"))
      of "femalessyllableend":
        femalesSyllablesEndList.add(y = gameNode.attr(name = "value"))
      of "femalesvocal":
        femalesVocalsList.add(y = gameNode.attr(name = "value"))
      of "shipssyllablestart":
        shipsSyllablesStartList.add(y = gameNode.attr(name = "value"))
      of "shipssyllablemiddle":
        shipsSyllablesMiddleList.add(y = gameNode.attr(name = "value"))
      of "shipssyllableend":
        shipsSyllablesEndList.add(y = gameNode.attr(name = "value"))
      of "attribute":
        attributesList.add(y = initAttributeRecord(name = gameNode.attr(
            name = "name"), description = gameNode.innerText()))
      of "skill":
        var newSkill: SkillRecord = initSkillRecord()
        newSkill.name = gameNode.attr(name = "name")
        newSkill.tool = gameNode.attr(name = "tool")
        let attributeName: string = gameNode.attr(name = "attribute")
        for index, attribute in attributesList.pairs():
          if attribute.name == attributeName:
            newSkill.attribute = index
            break
        for childNode in gameNode:
          if childNode.kind != xnElement:
            continue
          case childNode.tag
          of "description":
            newSkill.description = childNode.innerText()
          of "toolquality":
            try:
              newSkill.toolsQuality.add(y = initToolQuality(
                  level = childNode.attr(name = "level").parseInt(),
                  quality = childNode.attr(name = "quality").parseInt()))
            except ValueError:
              raise newException(exceptn = DataLoadingError,
                  message = "Can't add skill '" & newSkill.name & "'. Invalid value for tools quality.")
        skillsList[skillIndex] = newSkill
        skillIndex.inc()
      of "itemtype":
        itemsTypesList.add(y = gameNode.attr(name = "value"))
      of "remove":
        case gameNode.attr(name = "name")
        of "skill":
          {.warning[ProveInit]: off.}
          {.warning[UnsafeDefault]: off.}
          try:
            skillsList.del(key = gameNode.attr(name = "value").parseInt())
          except ValueError:
            raise newException(exceptn = DataLoadingError,
                message = "Can't delete skill '" & gameNode.attr(
                    name = "value") & "'. Invalid index.")
          {.warning[ProveInit]: on.}
          {.warning[UnsafeDefault]: on.}
        of "attribute":
          try:
            attributesList.del(i = gameNode.attr(name = "value").parseInt() - 1)
          except ValueError:
            raise newException(exceptn = DataLoadingError,
                message = "Can't delete attribute '" & gameNode.attr(
                    name = "value") & "'. Invalid index.")
        of "itemtype":
          try:
            itemsTypesList.del(i = gameNode.attr(name = "value").parseInt() - 1)
          except ValueError:
            raise newException(exceptn = DataLoadingError,
                message = "Can't delete item type '" & gameNode.attr(
                    name = "value") & "'. Invalid index.")
        else:
          discard
      of "repairtools":
        repairTools = gameNode.attr(name = "value")
      of "cleaningtools":
        cleaningTools = gameNode.attr(name = "value")
      of "alchemytools":
        alchemyTools = gameNode.attr(name = "value")
      of "corpseindex":
        try:
          corpseIndex = gameNode.attr(name = "value").parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
            message = "Can't set corpse index '" & gameNode.attr(
                name = "value") & "'. Invalid value.")
      of "missionitemstype":
        missionItemsType = gameNode.attr(name = "value")
      of "fueltype":
        fuelType = gameNode.attr(name = "value")
      of "moneyindex":
        try:
          moneyIndex = gameNode.attr(name = "value").parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
            message = "Can't set money index '" & gameNode.attr(
                name = "value") & "'. Invalid value.")
      of "tradersname":
        tradersName = gameNode.attr(name = "value")
      of "conditionname":
        conditionIndex = findAttributeIndex(attributeName = gameNode.attr(
            name = "value"))
      of "strengthname":
        strengthIndex = findAttributeIndex(attributeName = gameNode.attr(
            name = "value"))
      of "pilotingskill":
        pilotingSkill = findSkillIndex(skillName = gameNode.attr(
            name = "value"))
      of "engineeringskill":
        engineeringSkill = findSkillIndex(skillName = gameNode.attr(
            name = "value"))
      of "gunneryskill":
        gunnerySkill = findSkillIndex(skillName = gameNode.attr(name = "value"))
      of "talkingskill":
        talkingSkill = findSkillIndex(skillName = gameNode.attr(name = "value"))
      of "perceptionskill":
        perceptionSkill = findSkillIndex(skillName = gameNode.attr(
            name = "value"))
      of "headarmor":
        headArmor = gameNode.attr(name = "value")
      of "chestarmor":
        chestArmor = gameNode.attr(name = "value")
      of "armsarmor":
        armsArmor = gameNode.attr(name = "value")
      of "legsarmor":
        legsArmor = gameNode.attr(name = "value")
      of "shieldtype":
        shieldType = gameNode.attr(name = "value")
      of "weapontype":
        weaponType = gameNode.attr(name = "value")
      of "dodgeskill":
        dodgeSkill = findSkillIndex(skillName = gameNode.attr(name = "value"))
      of "unarmedskill":
        unarmedSkill = findSkillIndex(skillName = gameNode.attr(name = "value"))
