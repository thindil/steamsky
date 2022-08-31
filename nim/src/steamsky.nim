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

when defined(windows):
  const
    tclDllName = "tcl86.dll"
    tkDllName = "tk86.dll"
elif defined(macosx):
  const
    tclDllName = "libtcl8.6.dylib"
    tkDllName = "libtk8.6.dylib"
else:
  const
    tclDllName = "libtcl8.6.so(|.1|.0)"
    tkDllName = "libtk8.6.so(|.1|.0)"

type
  TFreeProc* = proc (theBlock: pointer){.cdecl.}
  TclInterp* = object
    result*: cstring
    freeProc*: TFreeProc
    errorLine*: int
  PInterp* = ptr TclInterp

proc steamsky(): PInterp {.exportc.} =
  proc tclCreateInterp(): PInterp {.cdecl, dynlib: tclDllName, importc: "Tcl_CreateInterp".}
  proc tclInit(interp: PInterp): cint {.cdecl, dynlib: tclDllName, importc: "Tcl_Init".}
  proc tkInit(interp: PInterp): cint {.cdecl, dynlib: tkDllName, importc: "Tk_Init".}

  result = tclCreateInterp()
  discard tclInit(result)
  discard tkInit(result)
