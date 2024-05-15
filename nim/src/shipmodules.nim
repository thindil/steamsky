# Copyright 2022-2024 Bartek thindil Jasicki
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
import contracts
import game, items, log, types

proc loadModules*(fileName: string) {.sideEffect, raises: [DataLoadingError],
    tags: [WriteIOEffect, ReadIOEffect, RootEffect], contractual.} =
  ## Load available prototypes of ship's modules from the data file
  ##
  ## * fileName - the path to the file with ship's modules data which will be loaded
  require:
    fileName.len > 0
  body:
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
                message = "Can't add module '" & moduleNode.attr(
                    name = "index") & "', invalid index.")
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
        module.mType = ModuleType.any
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
        module.maxValue = try:
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
              message = "Can't " & $moduleAction & " module '" & $moduleIndex &
              "', no skill named '" & attribute & "'.")
        module.repairSkill = skillIndex
      else:
        module.repairSkill = 2
      attribute = moduleNode.attr(name = "price")
      if attribute.len() > 0:
        module.price = try:
            attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $moduleAction & " module '" & $moduleIndex & "', invalid value for module price.")
      attribute = moduleNode.attr(name = "installtime")
      if attribute.len() > 0:
        module.installTime = try:
            attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $moduleAction & " module '" & $moduleIndex & "', invalid value for module installation time.")
      else:
        module.installTime = 60
      attribute = moduleNode.attr(name = "unique")
      if attribute.len() > 0:
        module.unique = true
      else:
        module.unique = false
      attribute = moduleNode.attr(name = "size")
      if attribute.len() > 0:
        module.size = try:
            attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $moduleAction & " module '" & $moduleIndex & "', invalid value for module size.")
      attribute = moduleNode.attr(name = "maxowners")
      if attribute.len() > 0:
        module.maxOwners = try:
            attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $moduleAction & " module '" & $moduleIndex & "', invalid value for module maxium owners amount.")
      else:
        module.maxOwners = 1
      attribute = moduleNode.attr(name = "speed")
      if attribute.len() > 0:
        module.speed = try:
            attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $moduleAction & " module '" & $moduleIndex & "', invalid value for module speed.")
      else:
        module.speed = 4
      attribute = moduleNode.attr(name = "reputation")
      if attribute.len() > 0:
        module.reputation = try:
            attribute.parseInt()
        except ValueError:
          raise newException(exceptn = DataLoadingError,
              message = "Can't " & $moduleAction & " module '" & $moduleIndex & "', invalid value for module required reputation.")
      else:
        module.reputation = -100
      attribute = moduleNode.innerText()
      if attribute.len() > 0:
        module.description = attribute
      if moduleAction == DataAction.add:
        logMessage(message = "Module added: '" & $moduleIndex & "'",
            debugType = everything)
      else:
        logMessage(message = "Module updated: '" & $moduleIndex & "'",
            debugType = everything)
      modulesList[moduleIndex] = module

proc getModuleType*(moduleIndex: Positive): string {.sideEffect, raises: [
    KeyError], tags: [], contractual.} =
  ## Get the type of the selected module from its name as enumeration
  ##
  ## * moduleIndex - the prototype index of the module
  ##
  ## Returns the string with the human readable name of the type of the
  ## selected module
  require:
    moduleIndex in modulesList
  body:
    result = $modulesList[moduleIndex].mType
    let index = result.find({'A'..'Z'})
    if index > 0:
      result = result[0 .. index - 1] & " " & result[index].toLowerAscii &
          result[index + 1 .. ^1]
    result = result.capitalizeAscii

# Temporary code for interfacing with Ada

type
  AdaBaseModuleData = object
    name: cstring
    mType: cint
    weight: cint
    value: cint
    maxValue: cint
    durability: cint
    repairMaterial: cstring
    repairSkill: cint
    price: cint
    installTime: cint
    unique: cint
    size: cint
    description: cstring
    maxOwners: cint
    speed: cint
    reputation: cint

proc getAdaModule(index: cint; adaModule: var AdaBaseModuleData) {.raises: [],
    tags: [], exportc, contractual.} =
  adaModule = AdaBaseModuleData(name: "".cstring, mType: 0, weight: 0, value: 0,
      maxValue: 0, durability: 0, repairMaterial: "".cstring, repairSkill: 0,
      price: 0, installTime: 0, unique: 0, size: 0, description: "".cstring,
      maxOwners: 0, speed: 0, reputation: -100)
  if not modulesList.hasKey(key = index):
    return
  let module = try:
      modulesList[index]
    except KeyError:
      return
  adaModule.name = module.name.cstring
  adaModule.mType = module.mType.ord.cint
  adaModule.weight = module.weight.cint
  adaModule.value = module.value.cint
  adaModule.maxValue = module.maxValue.cint
  adaModule.durability = module.durability.cint
  adaModule.repairMaterial = module.repairMaterial.cstring
  adaModule.repairSkill = module.repairSkill.cint
  adaModule.price = module.price.cint
  adaModule.installTime = module.installTime.cint
  adaModule.unique = (if module.unique: 1 else: 0)
  adaModule.size = module.size.cint
  adaModule.description = module.description.cstring
  adaModule.maxOwners = module.maxOwners.cint
  adaModule.speed = module.speed.cint
  adaModule.reputation = module.reputation.cint

proc getAdaModulesAmount(): cint {.raises: [], tags: [], exportc,
    contractual.} =
  return modulesList.len.cint

proc getAdaModuleType(moduleIndex: cint): cstring {.raises: [], tags: [],
    exportc, contractual.} =
  try:
    return getModuleType(moduleIndex = moduleIndex.Positive).cstring
  except KeyError:
    return ""
