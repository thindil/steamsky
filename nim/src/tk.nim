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
  TFreeProc* = proc (theBlock: pointer) {.cdecl.}
    ## Procedure which will be run during freeing the result value
    ##
    ## * theBlock - the pointer to the value to free

  TclInterp* = object
    ## Represents Tcl interpreter
    result*: cstring ## the string with result's value returned by the last Tcl command
    freeProc*: TFreeProc ## the procedure which will be run during freeing the result value
    errorLine*: cint ## the number of the line where error occured. Set only when error happened

  PInterp* = ptr TclInterp
    ## Pointer to the Tcl interpreter

  TclResults* = enum
    tclOk, tclError, tclReturn, tclBreak, tclContinue
    ## Types of result used by Tcl

  TclError* = object of CatchableError
    ## Used to raise exceptions related to the Tcl/Tk, like failed
    ## initialization, etc.

  TclCmdProc* = proc (clientData: cint; interp: TclInterp; argc: cint;
      argv: openArray[cstring]): TclResults
    ## Procedure which will be executed as Tcl command
    ##
    ## * clientData - the additional data passed to the procedure
    ## * interp     - the Tcl interpreter on which the command will be executed
    ## * argc       - the amount of arguments which the command takes
    ## * argv       - the values of the command's arguments
    ##
    ## Returns tclOk if the command was executed without problems, otherwise
    ## usually tclError

  TclCmdDeleteProc* = proc (clientData: cint) {.cdecl.}
    ## Procedure used to remove the selected Tcl command
    ##
    ## * clientData - the additional data passed to the procedure

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

proc tclEval(interp: PInterp; script: cstring): TclResults {.cdecl,
    dynlib: tclDllName, importc: "Tcl_Eval".}
  ## Evaluate the Tcl code on the selected Tcl interpreter and get the result
  ## of the evaluation
  ##
  ## * interp - The Tcl interpreter on which the code will be evaluated
  ## * script - The Tcl code which will be evaluated
  ##
  ## Returns tclOk if the code evaluated correctly, otherwise tclError

proc tclEval*(interp: PInterp = getInterp();
    script: string): TclResults {.discardable.} =
  ## Evaluate the Tcl code on the selected Tcl interpreter and get the result
  ## of the evaluation. Accepts Tcl code as Nim string
  ##
  ## * interp - The Tcl interpreter on which the code will be evaluated
  ## * script - The Tcl code which will be evaluated
  ##
  ## Returns tclOk if the code evaluated correctly, otherwise tclError
  return interp.tclEval(script = script.cstring)

proc tclGetResult*(interp: PInterp): cstring {.cdecl, dynlib: tclDllName,
    importc: "Tcl_GetStringResult".}
  ## Get the string with the result of the last evaluated Tcl command
  ##
  ## * interp - The Tcl interpreter from which the result will be taken
  ##
  ## Returns the string with the result of the last evaluated Tcl command

proc tclEval2*(interp: PInterp = getInterp(); script: string): string =
  ## Evaluate the Tcl code on the selected Tcl interpreter and get the result
  ## of the evaluation. Accepts Tcl code as Nim string
  ##
  ## * interp - The Tcl interpreter on which the code will be evaluated
  ## * script - The Tcl code which will be evaluated
  ##
  ## Returns the result of the evaluation of the code as Nim string
  interp.tclEval(script = script)
  return $(interp.tclGetResult)

proc tclCreateCommand*(interp: PInterp; cmdName: cstring; cproc: TclCmdProc;
    clientData: cint; deleteProc: TclCmdDeleteProc): pointer {.cdecl,
    dynlib: tclDllName, importc: "Tcl_CreateCommand", discardable.}
  ## Add a new Tcl command, defined in Nim on the selected Tcl interpreter.
  ## If there is a command with the same name, it will be replaced.
  ##
  ## * interp     - the Tcl interpreter on which the command will be added
  ## * cmdName    - the name of the command to add
  ## * cproc      - the Nim procedure which will be executed as the command
  ## * clientData - the additional data passed to cproc and deleteProc
  ## * deleteProc - the Nim procedure which will be executed during removing
  ##                the command, can be nil
  ##
  ## Returns pointer for the newly created command

proc tclGetVar(interp: PInterp; varName: cstring;
    flags: cint): cstring {.cdecl, dynlib: tclDllName, importc: "Tcl_GetVar".}
  ## Get the value of the selected Tcl variable.
  ##
  ## * interp  - the Tcl interpreter from which the variable will be get
  ## * varName - the name of the Tcl variable to get
  ## * flags   - the flag related to what kind of variable to search
  ##
  ## Returns the value of the selected Tcl variable

proc tclGetVar*(varName: string): string =
  ## Get the value of the selected Tcl variable.
  ##
  ## * varName - the name of the Tcl variable to get
  ##
  ## Returns the value of the selected Tcl variable as a Nim string
  return $tclGetVar(getInterp(), varName.cstring, 1)

proc tclSetVar(interp: PInterp; varName, newValue: cstring;
    flags: cint) {.cdecl, dynlib: tclDllName, importc: "Tcl_SetVar".}
  ## Set the new value for the selected Tcl variable. If variable doesn't
  ## exist, it will be created.
  ##
  ## * interp   - the Tcl interpreter on which the variable will be set
  ## * varName  - the name of the Tcl variable to set
  ## * newValue - the value of the Tcl variable
  ## * flags    - the flag related to what kind of variable it will be

proc tclSetVar*(varName, newValue: string) =
  ## Set the new value for the selected Tcl variable. If variable doesn't
  ## exist, it will be created.
  ##
  ## * varName  - the name of the Tcl variable to set
  ## * newValue - the value of the Tcl variable
  tclSetVar(getInterp(), varName.cstring, newValue.cstring, 1)
