discard """
  exitcode: 0
"""

import ../../src/messages

let messageIndex = getLastMessageIndex()
addMessage("my message", ord(MessageType.default), ord(white))
assert getLastMessageIndex() == messageIndex + 1
