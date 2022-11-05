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

import std/tables
import factions, game, utils

proc generateBaseName*(factionIndex: string): string {.sideEffect, raises: [],
    tags: [].} =
  ## FUNCTION
  ##
  ## Generate the name for the sky base, based on its owner's faction. Based
  ## on libtcod names generator
  ##
  ## PARAMETERS
  ##
  ## * factionIndex - the index of the faction which owns the base
  ##
  ## RETURNS
  ##
  ## The randomly generated name of the base
  try:
    if factionsList[factionIndex].namesType == robotic:
      return $generateRoboticName();
  except KeyError:
    discard
  if getRandom(min = 1, max = 100) < 16:
    result = basesSyllablesPreList[getRandom(min = 0, max = (
        basesSyllablesPreList.len() - 1))] & " "
  result = result & basesSyllablesStartList[getRandom(min = 0, max = (
      basesSyllablesStartList.len - 1))]
  result = result & basesSyllablesEndList[getRandom(min = 0, max = (
      basesSyllablesEndList.len - 1))]
  if getRandom(min = 1, max = 100) < 16:
    result = result & " " & basesSyllablesPostList[getRandom(min = 0, max = (
        basesSyllablesPostList.len - 1))]

# Temporary code for interfacing with Ada

proc generateAdaBaseName(factionIndex: cstring): cstring {.exportc, raises: [],
    tags: [].} =
  return generateBaseName(factionIndex = $factionIndex).cstring
