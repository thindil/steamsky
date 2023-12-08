import ../src/[game, types, messages]
import unittest2

suite "Unit tests for messages module":

  test "Testing getLastMessageIndex.":
    let messageIndex = getLastMessageIndex()
    addMessage("my message", MessageType.default, white)
    check:
      getLastMessageIndex() == messageIndex + 1

  test "Testing clearMessages.":
    clearMessages()
    check:
      getLastMessageIndex() == -1

  test "Testing formattedTime.":
    check:
      formattedTime(1600, 1, 1, 10, 1) == "1600-01-01 10:01"

  test "Testing getMessage.":
    gameDate = DateRecord(year: 1600, month: 1, day: 1, hour: 8, minutes: 1)
    addMessage("my message", MessageType.otherMessage, green)
    checkpoint "Getting an existent message."
    check:
      getMessage(1, 0).message == "[1600-01-01 08:01] my message"
    checkpoint "Getting a non-existent message."
    check:
      getMessage(10_000, 0).message.len() == 0

  test "Testing messagesAmount.":
    check:
      messagesAmount(ord(MessageType.default)) == 1

  test "Testing restoreMessage.":
    restoreMessage("my message", MessageType.default, white)
    check:
      messagesAmount(ord(MessageType.default)) == 2
