discard """
  exitcode: 0
"""

import ../../src/[game, utils]

assert daysDifference(DateRecord(year: 1, month: 1, day: 1, hour: 0,
    minutes: 0), DateRecord(year: 1, month: 1, day: 2, hour: 0, minutes: 0)) == 1
