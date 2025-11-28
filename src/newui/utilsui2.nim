# Copyright 2025 Bartek thindil Jasicki
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

## Provides various utility procedures for the game's UI, like counting the
## travel information, converting minutes to in-game date, etc.

import contracts
import ../[crew, game, shipscrew, shipsmovement, types]

type
  TravelArray* = array[1..2, Natural]
    ## Used to store data about travel to some point on the map

proc travelInfo*(distance: Natural): TravelArray {.raises: [ValueError],
    tags: [WriteIOEffect, TimeEffect, RootEffect], contractual.} =
  ## Count the ETA and the fuel usage for the selected distance
  ##
  ## * distance - Distance in map fields to destination point
  ##
  ## The result is the array with two values, the first is estimated time to
  ## travel the distance, the second is the amount of fuel needed to travel
  ## the distance. Additionally, it returns the modified parameter dialog it
  ## is modified if any error happened.
  result = [0, 0]
  if distance == 0:
    return
  let speed: float = realSpeed(ship = playerShip, infoOnly = true) / 1_000
  if speed == 0.0:
    return
  var minutesDiff: int = (100.0 / speed).int
  case playerShip.speed
  of quarterSpeed:
    if minutesDiff < 60:
      minutesDiff = 60
  of halfSpeed:
    if minutesDiff < 30:
      minutesDiff = 30
  of fullSpeed:
    if minutesDiff < 15:
      minutesDiff = 15
  else:
    discard
  minutesDiff *= distance
  var rests, restTime: int = 0
  for index, member in playerShip.crew:
    if member.order notin {pilot, engineer}:
      continue
    let tired: int = (minutesDiff / 15).int + member.tired
    if (tired / (80 + member.attributes[conditionIndex].level)).int > rests:
      rests = (tired / (80 + member.attributes[conditionIndex].level)).int
    if rests > 0:
      let cabinIndex: int = findCabin(memberIndex = index)
      var tempTime: int = 0
      if cabinIndex > -1:
        let
          damage: float = 1.0 - (playerShip.modules[cabinIndex].durability.float /
              playerShip.modules[cabinIndex].maxDurability.float)
        var cabinBonus: int = playerShip.modules[cabinIndex].cleanliness - (
            playerShip.modules[cabinIndex].cleanliness.float * damage).int
        if cabinBonus == 0:
          cabinBonus = 1
        tempTime = ((80.0 + member.attributes[conditionIndex].level.float) /
            cabinBonus.float).int * 15
        if tempTime == 0:
          tempTime = 15
      else:
        tempTime = (80 + member.attributes[conditionIndex].level) * 15
      tempTime += 15
      if tempTime > restTime:
        restTime = tempTime
  result[1] = minutesDiff + (rests * restTime)
  result[2] = abs(x = distance * countFuelNeeded()) + (rests * (restTime / 10).int)

proc minutesToDate*(minutes: int; infoText: var string) {.raises: [
    ], tags: [], contractual.} =
  ## Convert the game minutes to the game time in days, hours, etc
  ##
  ## * minutes  - the amount of minutes to convert
  ## * infoText - the string to which the converted time will be added
  ##
  ## Returns the updated infoText paramater with converted minutes to the game
  ## time
  var
    travelTime: DateRecord = DateRecord()
    minutesDiff: int = minutes
  while minutesDiff > 0:
    case minutesDiff
    of 518_401..int.high:
      minutesDiff -= 518_400
      travelTime.year.inc()
    of 43_201..518_400:
      minutesDiff -= 43_200
      travelTime.month.inc()
      if travelTime.month > 12:
        travelTime.year.inc()
        travelTime.month = 1
    of 1_441..43_200:
      minutesDiff -= 1_440
      travelTime.day.inc()
      if travelTime.day > 31:
        travelTime.month.inc()
        if travelTime.month > 12:
          travelTime.year.inc()
          travelTime.month = 1
    of 61..1_440:
      minutesDiff -= 60
      travelTime.hour.inc()
      if travelTime.hour > 23:
        travelTime.hour = 0
        travelTime.day.inc()
        if travelTime.day > 31:
          travelTime.day = 1
          travelTime.month.inc()
          if travelTime.month > 12:
            travelTime.month = 1
            travelTime.year.inc()
    else:
      travelTime.minutes = minutesDiff
      minutesDiff = 0
    if travelTime.year == 4_000_000:
      break
  if travelTime.year > 0:
    infoText = infoText & " " & $travelTime.year & "y"
  if travelTime.month > 0:
    infoText = infoText & " " & $travelTime.month & "m"
  if travelTime.day > 0:
    infoText = infoText & " " & $travelTime.day & "d"
  if travelTime.hour > 0:
    infoText = infoText & " " & $travelTime.hour & "h"
  if travelTime.minutes > 0:
    infoText = infoText & " " & $travelTime.minutes & "mins"

proc getSkillMarks*(skillIndex: Positive; memberIndex: Natural): string
  {.raises: [], tags: [WriteIOEffect, TimeEffect, RootEffect], contractual.} =
  ## Get the marks with information about the skill level for the selected
  ## skill for the selected crew member
  ##
  ## * skillIndex  - the index of the skill to check
  ## * memberIndex - the index of the player's ship's crew member to check
  ##
  ## The string with one "+" sign if the crew member known the skill, the
  ## string with twi "+" sings if the crew member has the highest level in
  ## the skill of the all crew members. Otherwise return an empty string.
  var
    skillValue: int = 0
    crewIndex: int = -1
  try:
    for index, member in playerShip.crew:
      if getSkillLevel(member = member, skillIndex = skillIndex) > skillValue:
        skillValue = getSkillLevel(member = member, skillIndex = skillIndex)
        crewIndex = index
    if getSkillLevel(member = playerShip.crew[memberIndex],
        skillIndex = skillIndex) > 0:
      result = " +"
  except:
    return ""
  if memberIndex == crewIndex:
    result &= "+"
