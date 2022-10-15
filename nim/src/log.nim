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

{.used.}

import std/logging
import game

type DebugTypes* = enum
  ## FUNCTION
  ##
  ## What kind of debug messages log. None, everything, only combat or start
  ## the game with debug menu
  none, everything, combat, menu

var debugMode*: DebugTypes = none ## The debug mode of the game.

proc logMessage(message: cstring; debugType: cint) {.exportc, sideEffect,
    raises: [], tags: [RootEffect].} =
  ## FUNCTION
  ##
  ## Write the selected message to the log file, C version
  ##
  ## PARAMETERS
  ##
  ## * message   - The message which will be written to the file
  ## * debugType - The type of message which will be written. If different
  ##               than the game debug mode (except everything), don't write it
  if debugType != debugMode.cint and debugMode != everything:
    return
  try:
    log(level = lvlAll, $message)
  except Exception:
    echo ("Can't write log message, reason: " & getCurrentExceptionMsg())

proc logMessage*(message: string; debugType: DebugTypes) {.sideEffect, raises: [],
    tags: [RootEffect].} =
  ## FUNCTION
  ##
  ## Write the selected message to the log file, Nim version
  ##
  ## PARAMETERS
  ##
  ## * message   - The message which will be written to the file
  ## * debugType - The type of message which will be written. If different
  ##               than the game debug mode (except everything), don't write it
  logMessage(message = message.cstring, debugType = ord(debugType).cint)

proc startLogging*() {.sideEffect, raises: [], tags: [RootEffect].} =
  ## FUNCTION
  ##
  ## Start logging the game. Set the logger.
  if debugMode == none:
    return
  try:
    let logger: FileLogger = newFileLogger(filename = saveDirectory &
        "debug.log", fmtStr = "[$datetime] - $levelname: ")
    addHandler(handler = logger)
    log(level = lvlAll, "Starting game in debug mode.")
  except IOError, Exception:
    echo ("Can't start log for the game, reason: " & getCurrentExceptionMsg())
