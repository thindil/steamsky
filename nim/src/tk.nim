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
    ## FUNCTION
    ##
    ## Procedure which will be run during freeing the result value
    ##
    ## PARAMETERS
    ##
    ## * theBlock - the pointer to the value to free
  TclInterp* = object
    ## FUNCTION
    ##
    ## Represents Tcl interpreter
    ##
    ## FIELDS
    ##
    ## * result    - the string with result's value returned by the last Tcl
    ##               command
    ## * freeProc  - the procedure which will be run during freeing the result
    ##               value
    ## * errorLine - the number of the line where error occured. Set only when
    ##               error happened
    result*: cstring
    freeProc*: TFreeProc
    errorLine*: int
  PInterp* = ptr TclInterp
    ## FUNCTION
    ##
    ## Pointer to the Tcl interpreter
  TclResults* = enum
    tclOk, tclError, tclReturn, tclBreak, tclContinue
    ## FUNCTION
    ##
    ## Types of result used by Tcl
  TclError* = object of CatchableError
    ## FUNCTION
    ##
    ## Used to raise exceptions related to the Tcl/Tk, like failed
    ## initialization, etc.


proc tclCreateInterp*(): PInterp {.cdecl, dynlib: tclDllName,
    importc: "Tcl_CreateInterp".}
  ## FUNCTION
  ##
  ## Create Tcl interpreter. Imported from C
  ##
  ## RETURNS
  ##
  ## Pointer to the newly created Tcl interpreter or nil if creation failed.

proc tclInit*(interp: PInterp): TclResults {.cdecl, dynlib: tclDllName,
    importc: "Tcl_Init".}
  ## FUNCTION
  ##
  ## Initialize Tcl with the selected interpreter. Load libraries, etc.
  ##
  ## PARAMETERS
  ##
  ## * interp - A Tcl interpreter which will be initialized
  ##
  ## RETURNS
  ##
  ## tclOk if Tcl initialized correctly, otherwise tclError

proc tkInit*(interp: PInterp): TclResults {.cdecl, dynlib: tkDllName,
    importc: "Tk_Init".}
  ## FUNCTION
  ##
  ## Initialize Tk on the selected Tcl interpreter
  ##
  ## PARAMETERS
  ##
  ## * interp - A Tcl interpreter on which Tk will be initialized
  ##
  ## RETURNS
  ##
  ## tclOk if Tk initialized correctly, otherwise tclError

proc tclEval*(interp: PInterp; script: cstring): TclResults {.cdecl,
    dynlib: tclDllName, importc: "Tcl_Eval".}
  ## FUNCTION
  ##
  ## Evaluate the Tcl code on the selected Tcl interpreter and get the result
  ## of the evaluation
  ##
  ## PARAMETERS
  ##
  ## * interp - The Tcl interpreter on which the code will be evaluated
  ## * script - The Tcl code which will be evaluated
  ##
  ## RETURNS
  ##
  ## tclOk if the code evaluated correctly, otherwise tclError
