# Copyright 2022-2026 Bartek thindil Jasicki
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

## Provides code related to logging various messages to the log file.
## What exactly is logged, depends on the logging's level.

import std/logging
import contracts
import game

type DebugLevels* = enum
  ## What level of debug messages to log. The firsts levels are the same as
  ## Level from logging, the last is like lvlNone with showing the debug
  ## menu.
  all, debug, info, notice, warn, error, fatal, none, menu

var debugMode*: DebugLevels = none ## The debug mode of the game.

proc logMessage*(message: string; messageLevel: Level = lvlDebug) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Write the selected message to the log file, Nim version
  ##
  ## * message      - The message which will be written to the file
  ## * messageLevel - The logging level of the message. Default value is lvlDebug
  try:
    log(level = messageLevel, args = $message)
  except Exception:
    echo ("Can't write log message, reason: " & getCurrentExceptionMsg())

proc startLogging*() {.raises: [], tags: [RootEffect], contractual.} =
  ## Start logging the game. Set the logger.
  # We want only the menu, don't set the loggers.
  if debugMode in {menu, none}:
    return
  try:
    let
      fileLogger: FileLogger = newFileLogger(filename = saveDirectory.string &
          "debug.log", levelThreshold = debugMode.ord.Level,
          fmtStr = "[$datetime] - $levelname: ", flushThreshold = lvlAll)
      consoleLogger: ConsoleLogger = newConsoleLogger(
          levelThreshold = debugMode.ord.Level,
          fmtStr = "[$datetime] - $levelname: ", flushThreshold = lvlAll)
    addHandler(handler = fileLogger)
    addHandler(handler = consoleLogger)
    log(level = lvlAll, args = "Starting game in debug mode.")
  except IOError, Exception:
    echo ("Can't start log for the game, reason: " & getCurrentExceptionMsg())
