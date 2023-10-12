discard """
  exitcode: 0
  output: '''Testing normalizeCoord.
Testing countDistance.'''
"""

import ../../src/[game, maps]

echo "Testing normalizeCoord."
var coord: cint = 0
normalizeCoord(coord)
try:
  assert coord == 1
except AssertionDefect:
  writeLine(stderr, "Failed to normalize an invalid coordinate.")
normalizeCoord(coord)
try:
  assert coord == 1
except AssertionDefect:
  writeLine(stderr, "Failed to not normalize a valid coordinate.")

playerShip.skyX = 1
playerShip.skyY = 1

echo "Testing countDistance."
try:
  assert countDistance(2, 2) == 1
except AssertionDefect:
  writeLine(stderr, "Failed to count distance between the player' ship and the selected map's field.")
