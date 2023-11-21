import ../src/[game, maps]
import unittest2

suite "Unit tests for maps module":

  playerShip.skyX = 1
  playerShip.skyY = 1

  test "Testing normalizeCoord.":
    var coord: cint = 0
    normalizeCoord(coord)
    checkpoint "Normalize an invalid coordinate."
    check:
      coord == 1
    checkpoint "Don't normalize a valid coordinate."
    normalizeCoord(coord)
    check:
      coord == 1

  test "Testing countDistance.":
    check:
      countDistance(2, 2) == 1