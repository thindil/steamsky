discard """
  exitcode: 0
  output: '''Testing getLastMessageIndex.
Testing clearMessages.
Testing formattedTime.
Testing getMessage.
Testing messagesAmount.
Testing restoreMessage.'''
"""

import ../../src/[game, types, messages]

echo "Testing getLastMessageIndex."
let messageIndex = getLastMessageIndex()
addMessage("my message", ord(MessageType.default), ord(white))
try:
  assert getLastMessageIndex() == messageIndex + 1
except AssertionDefect:
  writeLine(stderr, "Failed to get the index of the last message.")

echo "Testing clearMessages."
clearMessages()
try:
  assert getLastMessageIndex() == -1
except AssertionDefect:
  writeLine(stderr, "Failed to get index of the last message from empty list.")

echo "Testing formattedTime."
try:
  assert formattedTime(1600, 1, 1, 10, 1) == "1600-01-01 10:01"
except AssertionDefect:
  writeLine(stderr, "Failed to format the game's time.")

echo "Testing getMessage."
gameDate = DateRecord(year: 1600, month: 1, day: 1, hour: 8, minutes: 1)
addMessage("my message", ord(MessageType.othermessage), ord(green))
try:
  assert getMessage(1, 0).message == "[1600-01-01 08:01] my message"
except AssertionDefect:
  writeLine(stderr, "Failed to get an existing message.")
try:
  assert getMessage(10_000, 0).message.len() == 0
except AssertionDefect:
  writeLine(stderr, "Failed to not get a non-existing message.")

echo "Testing messagesAmount."
try:
  assert messagesAmount(ord(MessageType.default)) == 1
except AssertionDefect:
  writeLine(stderr, "Failed to get the amount of messages.")

echo "Testing restoreMessage."
restoreMessage("my message".cstring, ord(MessageType.default), ord(white))
try:
  assert messagesAmount(ord(MessageType.default)) == 2
except AssertionDefect:
  writeLine(stderr, "Failed to restore the message in the list.")
