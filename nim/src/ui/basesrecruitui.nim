# Copyright 2024 Bartek thindil Jasicki
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

import std/tables
import ../[game, tk, types]
import coreui, table

proc getHighestAttribute(baseIndex: BasesRange;
    memberIndex: Natural): string {.sideEffect, raises: [], tags: [].} =
  ## Get the highest attribute's name of the selected recruit
  ##
  ## * baseIndex   - The index of the base in which the recruit's attributes
  ##                 will be check
  ## * memberIndex - The index of the recruit which attributes will be check
  ##
  ## Returns the name of the attribute with the highest level of the selected
  ## recruit
  var
    highestLevel = 1
    highestIndex = 0
  for index, attrib in skyBases[baseIndex].recruits[memberIndex].attributes:
    if attrib.level > highestLevel:
      highestLevel = attrib.level
      highestIndex = index
  return attributesList[highestIndex].name

proc getHighestSkill(baseIndex: BasesRange;
    memberIndex: Natural): string {.sideEffect, raises: [], tags: [].} =
  ## Get the highest skill's name of the selected recruit
  ##
  ## * baseIndex   - The index of the base in which the recruit's skill will
  ##                 be check
  ## * memberIndex - The index of the recruit which skills will be check
  ##
  ## Returns the name of the skill with the highest level of the selected
  ## recruit
  var
    highestLevel = 1
    highestIndex = 0
  for skill in skyBases[baseIndex].recruits[memberIndex].skills:
    if skill.level > highestLevel:
      highestLevel = skill.level
      highestIndex = skill.index
  try:
    return skillsList[highestIndex].name
  except:
    return ""

var recruitTable: TableWidget

proc showRecruitCommand(clientData: cint; interp: PInterp;
    argc: cint; argv: cstringArray): TclResults {.exportc.} =
  var recruitFrame = mainPaned & ".recruitframe"
  if tclEval2(script = "winfo exists " & recruitFrame) == "0":
    tclEval(script = "ttk::frame " & recruitFrame)
    recruitTable = createTable(parent = recruitFrame, headers = @[
        "Name", "Gender", "Faction", "Base cost", "Highest stat",
        "Highest skill"], command = "SortRecruits",
        tooltipText = "Press mouse button to sort the recruits.")
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the trades UI
  try:
    discard
#    addCommand("ShowRecruit", showRecruitCommand)
  except:
    showError(message = "Can't add a Tcl command.")

# Temporary code for interfacing with Ada

proc getAdaHighestAttribute(baseIndex, memberIndex: cint): cstring {.exportc.} =
  return getHighestAttribute(baseIndex = baseIndex, memberIndex = memberIndex - 1).cstring

proc getAdaHighestRecSkill(baseIndex, memberIndex: cint): cstring {.exportc.} =
  return getHighestSkill(baseIndex = baseIndex, memberIndex = memberIndex - 1).cstring
