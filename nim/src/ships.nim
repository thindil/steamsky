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

type
  ShipSpeed* = enum
    ## FUNCTION
    ##
    ## Ships's state of speed, how much engines are used
    docked, full_Stop, quarter_Speed, half_Speed, full_Speed

proc getCabinQuality*(quality: cint): cstring {.exportc.} =
  case quality
  of 0..10:
    return "Empty room"
  of 11..20:
    return "Minimal quality"
  of 21..30:
    return "Basic quality"
  of 31..40:
    return "Second class"
  of 41..50:
    return "Medium quality"
  of 51..60:
    return "First class"
  of 61..70:
    return "Extended quality"
  of 71..80:
    return "Encrusted room"
  of 81..90:
    return "Luxury quality"
  else:
    return "Palace room"
