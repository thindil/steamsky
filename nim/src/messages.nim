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

func formattedTime*(year: cint, month: cint, day: cint, hour: cint,
    minutes: cint): cstring {.gcsafe, raises: [], tags: [], exportc.} =
  var formattedTime: string = $year & "-"
  if month < 10:
    formattedTime.add("0")
  formattedTime.add($month & "-")
  if day < 10:
    formattedTime.add("0")
  formattedTime.add($day & " ")
  if hour < 10:
    formattedTime.add("0")
  formattedTime.add($hour & ":")
  if minutes < 10:
    formattedTime.add("0")
  formattedTime.add($minutes)
  return formattedTime.cstring
