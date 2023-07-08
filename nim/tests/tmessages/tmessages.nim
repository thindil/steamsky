discard """
  exitcode: 0
  output: '''Testing getLastMessageIndex.
Testing clearMessages.
Testing formattedTime.
Testing getMessage.
Testing messagesAmount.
Testing restoreMessage.'''
"""

import ../../src/[types, messages]

echo "Testing getLastMessageIndex."
let messageIndex = getLastMessageIndex()
addMessage("my message", ord(MessageType.default), ord(white))
assert getLastMessageIndex() == messageIndex + 1, "Failed to get the index of the last message."

echo "Testing clearMessages."
clearMessages()
assert getLastMessageIndex() == -1, "Failed to get index of the last message from empty list."

echo "Testing formattedTime."
assert formattedTime(1600, 1, 1, 10, 1) == "1600-01-01 10:01", "Failed to format the game's time."

echo "Testing getMessage."
addMessage("my message", ord(MessageType.othermessage), ord(green))
assert getMessage(1, 0).message == "my message", "Failed to get an existing message."
assert getMessage(10_000, 0).message.len() == 0, "Failed to not get a non-existing message."

echo "Testing messagesAmount."
assert messagesAmount(ord(MessageType.default)) == 1, "Failed to get the amount of messages."

echo "Testing restoreMessage."
restoreMessage("my message".cstring, ord(MessageType.default), ord(white))
assert messagesAmount(ord(MessageType.default)) == 2, "Failed to restore the message in the list."
