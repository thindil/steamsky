# Copyright 2023 Bartek thindil Jasicki
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

import std/[strutils, xmlparser, xmltree]
import game

type
  HallOfFameData = object
    name: string
    points: Natural
    deathReason: string

var hallOfFameArray: array[1..10, HallOfFameData]

proc loadHallOfFame*() {.sideEffect, raises: [DataLoadingError], tags: [
    WriteIOEffect, ReadIOEffect, RootEffect].} =
  if hallOfFameArray[1].name.len > 0:
    return
  for entry in hallOfFameArray.mitems:
    entry = HallOfFameData(name: "", points: 0, deathReason: "")
  let hofXml = try:
      loadXml(path = saveDirectory & "halloffame.dat")
    except XmlError, ValueError, IOError, OSError, Exception:
      return
  var index = 1
  for hofNode in hofXml:
    if hofNode.kind != xnElement:
      continue
    hallOfFameArray[index].name = hofNode.attr(name = "name")
    hallOfFameArray[index].points = try:
        hofNode.attr(name = "points").parseInt()
      except ValueError:
        raise newException(exceptn = DataLoadingError,
            message = "Invalid value for points in hall of fame entry.")
    hallOfFameArray[index].deathReason = hofNode.attr(name = "Death_Reason")
    index.inc

# Temporary code for interfacing with Ada

type
  AdaHallOfFameData = object
    name: cstring
    points: cint
    deathReason: cstring

proc loadAdaHallOfFame(): cstring {.sideEffect, raises: [], tags: [
    WriteIOEffect, ReadIOEffect, RootEffect], exportc.} =
  try:
    loadHallOfFame()
    return "".cstring
  except DataLoadingError:
    return getCurrentExceptionMsg().cstring

proc getAdaHofEntry(index: cint; entry: var AdaHallOfFameData) {.sideEffect,
    raises: [], tags: [], exportc.} =
  entry = AdaHallOfFameData(name: hallOfFameArray[index].name.cstring,
      points: hallOfFameArray[index].points.cint, deathReason: hallOfFameArray[
      index].deathReason.cstring)
