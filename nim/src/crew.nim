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

{.used.}

import std/[tables]
import factions, game, items, utils


type
  EquipmentLocations* = enum
    ## FUNCTION
    ##
    ## Possible equipment location for mobiles
    weapon, shield, helmet, torso, arms, legs, tool

  CrewOrders = enum
    pilot, engineer, gunner, repair, craft, upgrading, talk, heal, clean, rest,
        defend, boarding, train

  MobAttributeRecord = object
    level: range[1..50]
    experience: Natural

  SkillInfo = object
    index: Natural
    level: SkillRange
    experience: Natural

  MemberData* = object
    attributes: seq[MobAttributeRecord]
    skills: seq[SkillInfo]
    name: string
    gender: char
    health: SkillRange
    tired: range[0..150]
    hunger: SkillRange
    thirst: SkillRange
    order: CrewOrders
    previousOrder: CrewOrders
    orderTime: int
    orders: array[1..12, Natural]
    inventory: seq[InventoryData]
    equipment: array[EquipmentLocations, Natural]
    payment: AttributesArray
    contractLength: int
    morale: AttributesArray
    loyalty: SkillRange

proc generateMemberName*(gender: char; factionIndex: string): string {.sideEffect,
    raises: [], tags: [].} =
  ## FUNCTION
  ##
  ## Generate the name for the mob, based on his/her faction. Based on
  ## libtcod names generator
  ##
  ## PARAMETERS
  ##
  ## * gender       - The gender of the mob, M - male, F - female
  ## * factionIndex - The index of the faction to which the mob belongs
  ##
  ## RETURNS
  ##
  ## The randomly generated name of the mob
  try:
    if factionsList[factionIndex].namesType == robotic:
      return $generateRoboticName();
  except KeyError:
    discard
  if gender == 'M':
    result = malesSyllablesStartList[getRandom(min = 0, max = (
        malesSyllablesStartList.len - 1))]
    result = result & malesVocalsList[getRandom(min = 0, max = (
        malesVocalsList.len - 1))]
    if getRandom(min = 1, max = 100) < 36:
      result = result & malesSyllablesMiddleList[getRandom(min = 0, max = (
          malesSyllablesMiddleList.len - 1))]
    if getRandom(min = 1, max = 100) < 11:
      result = result & malesConsonantsList[getRandom(min = 0, max = (
          malesConsonantsList.len - 1))]
    result = result & malesSyllablesEndList[getRandom(min = 0, max = (
        malesSyllablesEndList.len - 1))]
    return
  result = femalesSyllablesStartList[getRandom(min = 0, max = (
      femalesSyllablesStartList.len - 1))]
  result = result & femalesVocalsList[getRandom(min = 0, max = (
      femalesVocalsList.len - 1))]
  if getRandom(min = 1, max = 100) < 36:
    result = result & femalesSyllablesMiddleList[getRandom(min = 0, max = (
        femalesSyllablesMiddleList.len - 1))]
  if getRandom(min = 1, max = 100) < 11:
    result = result & femalesSyllablesMiddleList[getRandom(min = 0, max = (
        femalesSyllablesMiddleList.len - 1))]
  result = result & femalesSyllablesEndList[getRandom(min = 0, max = (
      femalesSyllablesEndList.len - 1))]

# Temporary code for interfacing with Ada

proc generateAdaMemberName(gender: char;
    factionIndex: cstring): cstring {.exportc.} =
  return generateMemberName(gender = gender,
      factionIndex = $factionIndex).cstring
