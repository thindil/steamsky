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

import random

proc generateRoboticName*(): cstring {.exportc, gcsafe, raises: [], tags: [].} =
  ## FUNCTION
  ##
  ## Generate robotic type name for bases, mobs, ships, etc
  ##
  ## RESULT
  ##
  ## Random robotic name
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

proc getRandom*(min, max: cint): cint {.exportc.} =
  randomize()
  return rand(min .. max)
