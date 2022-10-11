discard """
  exitcode: 0
"""

import ../../src/messages

addMessage("my message", ord(MessageType.othermessage), ord(green))
assert getMessage(1, 0).message == "my message"
assert getMessage(10_000, 0).message.len() == 0
