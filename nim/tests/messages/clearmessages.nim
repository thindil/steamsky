discard """
  exitcode: 0
"""

import ../../src/messages

addMessage("my message", ord(MessageType.default), ord(white))
assert getLastMessageIndex() == 0
clearMessages()
assert getLastMessageIndex() == -1
