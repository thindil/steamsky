discard """
  exitcode: 0
  output: '''Testing normalizeCoord.'''
"""

import ../../src/maps

echo "Testing normalizeCoord."
var coord: cint = 0
normalizeCoord(coord)
assert coord == 1, "Failed to normalize an invalid coordinate."
normalizeCoord(coord)
assert coord == 1, "Failed to not normalize a valid coordinate."
