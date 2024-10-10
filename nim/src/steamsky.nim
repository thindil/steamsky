# Copyright 2022-2024 Bartek thindil Jasicki
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

## The main game module. Read the game configuration, command line arguments,
## initialize graphic and start the game.

import std/[os, parseopt, strutils]
import contracts
import config, halloffame, game, log, tk
import ui/[mainmenu, themes]

proc steamsky(params: cstring) {.exportc, raises: [TclError, IOError,
    OSError, ValueError, DataLoadingError], tags: [ReadIOEffect, RootEffect],
        contractual.} =
  ## The main procedure of the game.

  # Get the command line params if any
  if params.len() > 0:
    var gameParams: OptParser = initOptParser(cmdLine = $params)
    for kind, key, val in gameParams.getopt():
      case key
      of "savedir":
        saveDirectory = val & DirSep
        normalizePath(path = saveDirectory)
      of "modsdir":
        modsDirectory = val & DirSep
        normalizePath(path = modsDirectory)
      of "datadir":
        dataDirectory = val & DirSep
        normalizePath(path = dataDirectory)
      of "debug":
        debugMode = parseEnum[DebugTypes](s = val)

  # Create the game directories
  createDir(dir = saveDirectory)
  createDir(dir = modsDirectory)
  createDir(dir = themesDirectory)

  # Start logging
  startLogging()

  # Load the game configuration.
  loadConfig()

  # Load the hall of fame
  loadHallOfFame()

  # Load the game's themes
  loadThemes()

  # Create Tcl interpreter
  let res: PInterp = tclCreateInterp()
  # If creation failed, report error and quit
  if res == nil:
    raise newException(exceptn = TclError,
        message = "Can't create Tcl interpreter.")
  # Initialize Tcl. Quit if failed
  if tclInit(interp = res) == tclError:
    raise newException(exceptn = TclError,
        message = "Can't initialize Tcl interpreter.")
  # Initialize Tk. Quit if failed
  if tkInit(interp = res) == tclError:
    raise newException(exceptn = TclError, message = "Can't initialize Tk.")
  setInterp(interp = res)

  # Initialize needed packages
  for package in ["tooltip", "tksvg", "autoscroll"]:
    if res.tclEval(script = "package require " & package) == tclError:
      raise newException(exceptn = TclError, message = "Can't initialize " &
          package & " package.")

  # Create and show the main game menu
  createMainMenu()

  # Loop inside Tk, waiting for commands to execute. When there are no windows
  # left, mainLoop returns and we exit.
  mainLoop()
