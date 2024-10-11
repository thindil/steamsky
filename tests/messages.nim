import unittest2
include ../src/messages

suite "Unit tests for messages module":

  test "Getting the last message index.":
    let messageIndex = getLastMessageIndex()
    addMessage("my message", default, white)
    check:
      getLastMessageIndex() == messageIndex + 1

  test "Clearing messages.":
    clearMessages()
    check:
      getLastMessageIndex() == -1

  test "Getting formatted game time.":
    check:
      formattedTime(1600, 1, 1, 10, 1) == "1600-01-01 10:01"

  test "Getting an existent message.":
    gameDate = DateRecord(year: 1600, month: 1, day: 1, hour: 8, minutes: 1)
    addMessage("my message", otherMessage, green)
    check:
      getMessage(1).message == "[1600-01-01 08:01] my message"

  test "Getting a non-existent message.":
    check:
      getMessage(10_000).message.len == 0

  test "Getting messages amount.":
    check:
      messagesAmount(default) == 1

  test "Restoring a message.":
    restoreMessage("my message", default, white)
    check:
      messagesAmount(default) == 2
