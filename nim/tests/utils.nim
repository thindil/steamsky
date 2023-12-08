import ../src/[types, utils]
import unittest2

suite "Unit tests for utils module":

  test "Testing daysDifference.":
    check:
      daysDifference(DateRecord(year: 1, month: 1, day: 1, hour: 0,
          minutes: 0), DateRecord(year: 1, month: 1, day: 2, hour: 0, minutes: 0)) ==
          1

  test "Testing generateRoboticName.":
    check:
      generateRoboticName().len() > 0

  test "Testing getRandom.":
    checkpoint "Get a random value between 1 and 5"
    for i in 1..5:
      check:
        getrandom(1, 5) in 1..5
    checkpoint "Get a random value between 5 and 5"
    check:
      getrandom(5, 5) == 5
