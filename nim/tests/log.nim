import unittest2
include ../src/log

suite "Unit tests for log module":

  test "Logging a message.":
    let oldSaveDirectory = saveDirectory
    saveDirectory = "."
    debugMode = everything
    startLogging()
    logMessage(message = "Test message", debugType = everything)
    saveDirectory = oldSaveDirectory
