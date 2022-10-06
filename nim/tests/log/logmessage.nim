discard """
  exitcode: 0
"""

import ../../src/log

debugMode = everything
startLogging()
logMessage(message = "Test message", debugType = ord(everything))
