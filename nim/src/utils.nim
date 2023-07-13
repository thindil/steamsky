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

import std/random
import types, game

proc generateRoboticName*(): cstring {.exportc, gcsafe, sideEffect, raises: [],
    tags: [].} =
  ## Generate robotic type name for bases, mobs, ships, etc
  ##
  ## Returns random robotic name
  randomize()
  let
    lettersAmount: Positive = rand(x = 2..5)
    numbersAmount: Positive = rand(x = 2..4)
    letters: set[char] = {'A'..'Z'}
  var name: string = ""
  # Get random letters for the name
  for i in 1..lettersAmount:
    name.add(y = sample(s = letters))
  name.add('-')
  # Get random digits for the name
  for i in 1..numbersAmount:
    name.add(y = $rand(max = 9))
  return name.cstring

proc getRandom(min, max: cint): cint {.exportc, gcsafe, sideEffect, raises: [],
    tags: [].} =
  ## Get the random value from the selected range
  ##
  ## * min - The minimal value from which the value will be taken
  ## * max - The maximal value from which the value will be taken
  ##
  ## Returns the random value from min and max range
  randomize()
  return rand(min .. max)

proc getRandom*(min,max: int): int {.gcsafe, sideEffect, raises: [],
    tags: [].} =
  ## Get the random value from the selected range
  ##
  ## * min - The minimal value from which the value will be taken
  ## * max - The maximal value from which the value will be taken
  ##
  ## Returns the random value from min and max range
  randomize()
  return rand(min .. max)

proc daysDifference*(dateToCompare, currentDate: DateRecord): cint {.exportc,
    gcsafe, sideEffect, raises: [], tags: [].} =
  ## Get the difference in days between two dates, mostly with the current
  ## date in the game
  ##
  ## * dateToCompare - the game date to compare
  ## * currentDate   - the current game date to which the date will be compared
  ##
  ## Returns the difference in days between the two dates
  let
    curDate = (if currentDate.year > gameDate.year: gameDate else: currentDate)
    compDate = (if dateToCompare.year > 4_000_000: gameDate else: dateToCompare)
  return (curDate.day.cint + (30 * curDate.month.cint) + (
      curDate.year.cint * 360)) - (compDate.day.cint + (30 *
      compDate.month.cint) + (compDate.year.cint * 360))
