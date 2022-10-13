discard """
  exitcode: 0
"""

import ../../src/messages

let messageIndex = getLastMessageIndex()
restoreMessage("my message".cstring, ord(MessageType.default), ord(white))
assert getLastMessageIndex() == messageIndex + 1
