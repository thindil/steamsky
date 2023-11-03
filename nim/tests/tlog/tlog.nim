discard """
  exitcode: 0
  output: '''Testing logMessage.'''
"""

import ../../src/[game, log]

## Temporary code
import std/tables
import ../../src/[items, careers, factions]
if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
# end of temporary code

echo "Testing logMessage."
let oldSaveDirectory = saveDirectory
saveDirectory = "."
debugMode = everything
startLogging()
logMessage(message = "Test message", debugType = everything)
saveDirectory = oldSaveDirectory
