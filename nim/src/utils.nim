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

## Provides various procedures like generating names for robotic factions and
## random numbers generation.

import std/random
import contracts
import game, types

proc generateRoboticName*(): string {.sideEffect, raises: [],
    tags: [], contractual.} =
  ## Generate robotic type name for bases, mobs, ships, etc
  ##
  ## Returns random robotic name
  ensure:
    result.len > 0
  body:
    randomize()
    let
      lettersAmount: Positive = rand(x = 2..5)
      numbersAmount: Positive = rand(x = 2..4)
      letters: set[char] = {'A'..'Z'}
    result = ""
    # Get random letters for the name
    for i in 1..lettersAmount:
      result.add(y = sample(s = letters))
    result.add(y = '-')
    # Get random digits for the name
    for i in 1..numbersAmount:
      result.add(y = $rand(max = 9))

proc getRandom(min, max: cint): cint {.exportc, gcsafe, sideEffect, raises: [],
    tags: [], contractual.} =
  ## Get the random value from the selected range
  ##
  ## * min - The minimal value from which the value will be taken
  ## * max - The maximal value from which the value will be taken
  ##
  ## Returns the random value from min and max range
  require:
    min <= max
  ensure:
    result in min .. max
  body:
    randomize()
    return rand(x = min .. max)

proc getRandom*(min, max: int): int {.gcsafe, sideEffect, raises: [],
    tags: [], contractual.} =
  ## Get the random value from the selected range
  ##
  ## * min - The minimal value from which the value will be taken
  ## * max - The maximal value from which the value will be taken
  ##
  ## Returns the random value from min and max range
  require:
    min <= max
  ensure:
    result in min .. max
  body:
    randomize()
    return rand(x = min .. max)

proc daysDifference*(dateToCompare: DateRecord;
    currentDate: DateRecord = gameDate): cint {.gcsafe, sideEffect, raises: [],
    tags: [], contractual.} =
  ## Get the difference in days between two dates, mostly with the current
  ## date in the game
  ##
  ## * dateToCompare - the game date to compare
  ## * currentDate   - the current game date to which the date will be compared.
  ##                   Default value the current in-game date.
  ##
  ## Returns the difference in days between the two dates
  return (currentDate.day.cint + (30 * currentDate.month.cint) + (
      currentDate.year.cint * 360)) - (dateToCompare.day.cint + (30 *
      dateToCompare.month.cint) + (dateToCompare.year.cint * 360))

# Temporary code for interfacing with Ada

proc daysAdaDifference(y, m, d, h, mi, yc, mc, dc, hc,
    mic: cint): cint {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  return daysDifference(dateToCompare = DateRecord(year: y, month: m, day: d,
      hour: h, minutes: m), currentDate = DateRecord(year: yc, month: mc,
      day: dc, hour: hc, minutes: mic))
