discard """
  exitcode: 0
"""

import ../../src/maps

var coord: cint = 0
normalizeCoord(coord)
assert coord == 1
normalizeCoord(coord)
assert coord == 1
