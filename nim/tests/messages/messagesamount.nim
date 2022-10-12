discard """
  exitcode: 0
"""

import ../../src/messages

let messagesAmount = messagesAmount(ord(MessageType.default))
addMessage("my message", ord(MessageType.default), ord(white))
assert messagesAmount(ord(MessageType.default)) == messagesAmount + 1
