discard """
  exitcode: 0
  output: '''Testing logMessage.'''
"""

import ../../src/[game, log]

echo "Testing logMessage."
let oldSaveDirectory = saveDirectory
saveDirectory = "."
debugMode = everything
startLogging()
logMessage(message = "Test message", debugType = everything)
saveDirectory = oldSaveDirectory
