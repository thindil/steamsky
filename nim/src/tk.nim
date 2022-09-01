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

proc tclCreateInterp*(): PInterp {.cdecl, dynlib: tclDllName,
    importc: "Tcl_CreateInterp".}

proc tclInit*(interp: PInterp): cint {.cdecl, dynlib: tclDllName,
    importc: "Tcl_Init".}

proc tkInit*(interp: PInterp): cint {.cdecl, dynlib: tkDllName,
    importc: "Tk_Init".}
