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

import ../[tk]
import dialogs

proc showWaitCommand*(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
  var waitDialog = ".gameframe.wait"
  if tclEval2(script = "winfo exists " & waitDialog) == "1":
    let button = waitDialog & ".frame.close"
    tclEval(script = button & " invoke")
    return tclOk
  waitDialog = createDialog(name = ".gameframe.wait", title = "Wait in place", columns = 3)

  proc addButton(time: Positive) =
    let button = waitDialog & ".wait" & $time
    tclEval(script = "ttk::button " & button & " -text {Wait " & $time &
        " minute" & (if time > 1: "s" else: "") & "} -command {Wait " & $time & "}")
    tclEval(script = "grid " & button & " -sticky we -columnspan 3 -padx 5" & (
        if time == 1: " -pady {5 0}" else: ""))
    tclEval(script = "bind " & button & " <Escape> {CloseDialog " & waitDialog & ";break}")
    tclEval(script = "tooltip::tooltip " & button & " \"Wait in place for " &
        $time & " minute" & (if time > 1: "s" else: "") & "\"")

  addButton(time = 1)
  return tclOk
