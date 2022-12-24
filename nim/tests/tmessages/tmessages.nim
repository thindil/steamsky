discard """
  exitcode: 0
"""

import ../../src/messages

let messageIndex = getLastMessageIndex()
addMessage("my message", ord(MessageType.default), ord(white))
assert getLastMessageIndex() == messageIndex + 1

clearMessages()
assert getLastMessageIndex() == -1

assert formattedTime(1600, 1, 1, 10, 1) == "1600-01-01 10:01"

addMessage("my message", ord(MessageType.othermessage), ord(green))
assert getMessage(1, 0).message == "my message"
assert getMessage(10_000, 0).message.len() == 0

assert messagesAmount(ord(MessageType.default)) == 1

restoreMessage("my message".cstring, ord(MessageType.default), ord(white))
assert messagesAmount(ord(MessageType.default)) == 2
