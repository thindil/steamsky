discard """
  exitcode: 0
  output: '''Testing updateHallOfFame.'''
"""

import ../../src/[game, halloffame, statistics]

for entry in hallOfFameArray.mitems:
  entry = HallOfFameData(name: "", points: 0, deathReason: "")
gameStats.points = 100
saveDirectory = "."

echo "Testing updateHallOfFame."
updateHallOfFame("TestPlayer", "TestDeath")
assert hallOfFameArray[1].name == "TestPlayer", "Failed to add entry to Hall of Fame."
