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
