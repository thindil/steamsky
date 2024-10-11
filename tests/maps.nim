import unittest2
include ../src/maps

suite "Unit tests for maps module":

  playerShip.skyX = 1
  playerShip.skyY = 1
  var coord: int = 0

  test "Normalize an invalid coordinate.":
    normalizeCoord(coord)
    check:
      coord == 1

  test "Don't normalize a valid coordinate.":
    normalizeCoord(coord)
    check:
      coord == 1

  test "Counting a distance on the map.":
    check:
      countDistance(2, 2) == 1
