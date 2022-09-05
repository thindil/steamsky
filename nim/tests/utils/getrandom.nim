discard """
  exitcode: 0
"""

import ../../src/utils

for i in 1..5:
  assert getrandom(1, 5) in 1..5
assert getrandom(5, 5) == 5
