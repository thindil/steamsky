discard """
  exitcode: 0
"""

import ../../src/[game, log]

let oldSaveDirectory = saveDirectory
saveDirectory = "."
debugMode = everything
startLogging()
logMessage(message = "Test message", debugType = everything)
saveDirectory = oldSaveDirectory
