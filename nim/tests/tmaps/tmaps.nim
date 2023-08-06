discard """
  exitcode: 0
  output: '''Testing normalizeCoord.'''
"""

import ../../src/maps

echo "Testing normalizeCoord."
var coord: cint = 0
normalizeCoord(coord)
try:
  assert coord == 1
except AssertionDefect:
  echo "Failed to normalize an invalid coordinate."
normalizeCoord(coord)
try:
  assert coord == 1
except AssertionDefect:
  echo "Failed to not normalize a valid coordinate."
