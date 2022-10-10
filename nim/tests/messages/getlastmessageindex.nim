discard """
  exitcode: 0
"""

import ../../src/messages

assert getLastMessageIndex() == -1
addMessage("my message", ord(MessageType.default), ord(white))
assert getLastMessageIndex() == 0
