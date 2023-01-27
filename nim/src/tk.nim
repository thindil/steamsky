# Copyright 2022-2023 Bartek thindil Jasicki
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

# Set names of Tcl/Tk libraries
# On Windows
when defined(windows):
  const
    tclDllName = "tcl86.dll"
    tkDllName = "tk86.dll"
# On MacOSX
elif defined(macosx):
  const
    tclDllName = "libtcl8.6.dylib"
    tkDllName = "libtk8.6.dylib"
# Any other *nix system
else:
  const
    tclDllName = "libtcl8.6.so(|.1|.0)"
    tkDllName = "libtk8.6.so(|.1|.0)"

type
  TFreeProc* = proc (theBlock: pointer){.cdecl.}
    ## Procedure which will be run during freeing the result value
    ##
    ## * theBlock - the pointer to the value to free

  TclInterp* = object
    ## Represents Tcl interpreter
    result*: cstring ## the string with result's value returned by the last Tcl command
    freeProc*: TFreeProc ## the procedure which will be run during freeing the result value
    errorLine*: int ## the number of the line where error occured. Set only when error happened

  PInterp* = ptr TclInterp
    ## Pointer to the Tcl interpreter

  TclResults* = enum
    tclOk, tclError, tclReturn, tclBreak, tclContinue
    ## Types of result used by Tcl

  TclError* = object of CatchableError
    ## Used to raise exceptions related to the Tcl/Tk, like failed
    ## initialization, etc.

var currentTclInterp: PInterp = nil
  ## Stores the current Tcl interpreter

proc setInterp*(interp: PInterp) {.gcsafe, sideEffect, raises: [], tags: [].} =
  ## Set the current Tcl interpreter.
  ##
  ## * interp - The Tcl interpreter which will be set as the current
  currentTclInterp = interp

proc getInterp*(): PInterp {.gcsafe, sideEffect, raises: [], tags: [].} =
  ## Get the current Tcl interpreter
  ##
  ## Returns the Tcl interpreter set as the current
  result = currentTclInterp

proc tclCreateInterp*(): PInterp {.cdecl, dynlib: tclDllName,
    importc: "Tcl_CreateInterp".}
  ## Create Tcl interpreter. Imported from C
  ##
  ## Returns pointer to the newly created Tcl interpreter or nil if creation failed.

proc tclInit*(interp: PInterp): TclResults {.cdecl, dynlib: tclDllName,
    importc: "Tcl_Init".}
  ## Initialize Tcl with the selected interpreter. Load libraries, etc.
  ##
  ## * interp - A Tcl interpreter which will be initialized
  ##
  ## Returns tclOk if Tcl initialized correctly, otherwise tclError

proc tkInit*(interp: PInterp): TclResults {.cdecl, dynlib: tkDllName,
    importc: "Tk_Init".}
  ## Initialize Tk on the selected Tcl interpreter
  ##
  ## * interp - A Tcl interpreter on which Tk will be initialized
  ##
  ## Returns tclOk if Tk initialized correctly, otherwise tclError

proc tclEval*(interp: PInterp; script: cstring): TclResults {.cdecl,
    dynlib: tclDllName, importc: "Tcl_Eval".}
  ## Evaluate the Tcl code on the selected Tcl interpreter and get the result
  ## of the evaluation
  ##
  ## * interp - The Tcl interpreter on which the code will be evaluated
  ## * script - The Tcl code which will be evaluated
  ##
  ## Returns tclOk if the code evaluated correctly, otherwise tclError

proc tclGetResult*(interp: PInterp): cstring {.cdecl, dynlib: tclDllName,
    importc: "Tcl_GetStringResult".}
  ## Get the string with the result of the last evaluated Tcl command
  ##
  ## * interp - The Tcl interpreter from which the result will be taken
  ##
  ## Returns the string with the result of the last evaluated Tcl command
