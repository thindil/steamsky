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

import std/[os, osproc]
import ../tk
import dialogs

proc openLinkCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [
        ReadIOEffect, ExecIOEffect, RootEffect].} =
  let command = try:
        findExe(exe = (if hostOs == "windows": "start" elif hostOs ==
          "macosx": "open" else: "xdg-open"))
      except:
        tclEval(script = "bgerror {Can't find the program to open the link. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
  if command.len == 0:
    showMessage(text = "Can't open the link. Reason: no program to open it.",
        parentFrame = ".", title = "Can't open the link.")
    return tclOk
  discard execCmd(command = command & " " & $argv[1])
  return tclOk

proc addCommands*() =
  addCommand("OpenLink", openLinkCommand)
