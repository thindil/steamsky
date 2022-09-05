discard """
  exitcode: 0
"""

import ../../src/utils

assert generateRoboticName().len() > 0
