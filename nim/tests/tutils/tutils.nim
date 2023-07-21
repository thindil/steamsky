discard """
  exitcode: 0
  output: '''Testing daysDifference.
Testing generateRoboticName.
Testing getRandom.'''
"""

import ../../src/[types, utils]

echo "Testing daysDifference."
assert daysDifference(DateRecord(year: 1, month: 1, day: 1, hour: 0,
    minutes: 0), DateRecord(year: 1, month: 1, day: 2, hour: 0, minutes: 0)) ==
    1, "Failed to count days difference between two dates."

echo "Testing generateRoboticName."
assert generateRoboticName().len() > 0, "Failed to generate a robotic name."

echo "Testing getRandom."
for i in 1..5:
  assert getrandom(1, 5) in 1..5, "Failed to generate a random number between 1 and 5"
assert getrandom(5, 5) == 5, "Failed to generate a random number between 5 and 5"
