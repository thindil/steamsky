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
    ## FUNCTION
    ##
    ## Possible orders for ships' crew members
    pilot, engineer, gunner, repair, craft, upgrading, talk, heal, clean, rest,
        defend, boarding, train

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
