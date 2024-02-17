import unittest2
include ../src/log

suite "Unit tests for log module":

  test "Testing logMessage.":
    let oldSaveDirectory = saveDirectory
    saveDirectory = "."
    debugMode = everything
    startLogging()
    logMessage(message = "Test message", debugType = everything)
    saveDirectory = oldSaveDirectory
