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

import std/[strutils, tables, xmlparser, xmltree]
import game, items, log, types

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

var modulesList* = initTable[Positive, BaseModuleData]() ## The list of prototypes of all ships' modules available in the game

proc loadModules*(fileName: string) =
  let modulesXml = try:
      loadXml(path = fileName)
    except XmlError, ValueError, IOError, OSError, Exception:
      raise newException(exceptn = DataLoadingError,
          message = "Can't load modules data file. Reason: " &
          getCurrentExceptionMsg())
  for moduleNode in modulesXml:
    if moduleNode.kind != xnElement:
      continue
    let
      moduleIndex: Natural = try:
          moduleNode.attr(name = "index").parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't add module '" & moduleNode.attr(name = "index") & "', invalid index.")
      moduleAction: DataAction = try:
          parseEnum[DataAction](moduleNode.attr(name = "action").toLowerAscii)
        except ValueError:
          DataAction.add
    if moduleAction in [update, remove]:
      if moduleIndex > modulesList.len():
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $moduleAction & " module '" & $moduleIndex & "', there is no module with that index.")
    elif moduleIndex < modulesList.len():
      raise newException(exceptn = DataLoadingError,
          message = "Can't add module '" & $moduleIndex & "', there is an module with that index.")
    if moduleAction == DataAction.remove:
      {.warning[ProveInit]: off.}
      {.warning[UnsafeDefault]: off.}
      modulesList.del(key = moduleIndex)
      {.warning[ProveInit]: on.}
      {.warning[UnsafeDefault]: on.}
      logMessage(message = "module removed: '" & $moduleIndex & "'",
          debugType = everything)
      continue
    var module: BaseModuleData = if moduleAction == DataAction.update:
        try:
          modulesList[moduleIndex]
        except ValueError:
          BaseModuleData(repairSkill: 1, installTime: 1, size: 1)
      else:
        BaseModuleData(repairSkill: 1, installTime: 1, size: 1)
    var attribute = moduleNode.attr(name = "name")
    if attribute.len() > 0:
      module.name = attribute
    attribute = moduleNode.attr(name = "type")
    if attribute.len() > 0:
      module.mType = try:
          parseEnum[ModuleType](attribute.toLowerAscii)
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $moduleAction & " module '" &
                  $moduleIndex & "', invalid type of module.")
    else:
      module.mType = any
    attribute = moduleNode.attr(name = "weight")
    if attribute.len() > 0:
      module.weight = try:
          attribute.parseInt()
      except ValueError:
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $moduleAction & " module '" & $moduleIndex & "', invalid value for module weight.")
    attribute = moduleNode.attr(name = "value")
    if attribute.len() > 0:
      module.value = try:
          attribute.parseInt()
      except ValueError:
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $moduleAction & " module '" & $moduleIndex & "', invalid value for value field.")
    attribute = moduleNode.attr(name = "maxvalue")
    if attribute.len() > 0:
      module.maxvalue = try:
          attribute.parseInt()
      except ValueError:
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $moduleAction & " module '" & $moduleIndex & "', invalid value for maxvalue field.")
    attribute = moduleNode.attr(name = "durability")
    if attribute.len() > 0:
      module.durability = try:
          attribute.parseInt()
      except ValueError:
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $moduleAction & " module '" & $moduleIndex & "', invalid value for module durability.")
    attribute = moduleNode.attr(name = "material")
    if attribute.len() > 0:
      let itemIndex = findProtoItem(itemType = attribute)
      if itemIndex == 0:
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $moduleAction & " module '" & $moduleIndex &
            "', no items with type '" & attribute & "'.")
      module.repairMaterial = attribute
    attribute = moduleNode.attr(name = "skill")
    if attribute.len() > 0:
      let skillIndex = findSkillIndex(skillName = attribute)
      if skillIndex == 0:
        raise newException(exceptn = DataLoadingError,
            message = "Can't " & $moduleAction & " faction '" & $moduleIndex &
            "', no skill named '" & attribute & "'.")
      module.repairSkill = skillIndex
