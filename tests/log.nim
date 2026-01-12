import std/paths
import unittest2
include ../src/log

suite "Unit tests for log module":

  test "Logging a message.":
    let oldSaveDirectory = saveDirectory
    saveDirectory = "tests/".Path
    debugMode = all
    startLogging()
    logMessage(message = "Test message")
    saveDirectory = oldSaveDirectory
