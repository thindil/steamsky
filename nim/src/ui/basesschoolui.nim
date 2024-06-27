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

import std/[strutils, tables]
import ../[game, tk]
import coreui

proc setSchoolSkillsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.exportc.} =
  let frameName = mainPaned & ".schoolframe.canvas.school"
  var comboBox = frameName & ".setting.crew"
  let memberIndex = tclEval2(script = comboBox & " current").parseInt
  for index, skill in skillsList:
    var skillLevel = 0
    for skill2 in playerShip.crew[memberIndex].skills:
      if skill2.index == index:
        skillLevel = skill2.level
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the trades UI
  try:
    discard
#    addCommand("SetSchoolSkills", setSchoolSkillsCommand)
  except:
    showError(message = "Can't add a Tcl command.")
