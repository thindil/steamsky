discard """
  exitcode: 0
"""

import ../../src/messages

assert formattedTime(1600, 1, 1, 10, 1) == "1600-01-01 10:01"
