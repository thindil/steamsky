discard """
  exitcode: 0
  output: '''Loading the game data.
Testing getStepData.'''
"""

import std/tables
import ../../src/[stories]

echo "Loading the game data."
if storiesList.len == 0:
  loadStories("../bin/data/stories.dat")

echo "Testing getStepData."
try:
  assert getStepData(storiesList["1"].steps[0].finishData, "condition") == "Rhetoric"
except AssertionDefect:
  echo "Failed to get finish data of selected step."
try:
  assert getStepData(storiesList["1"].steps[0].finishData, "sdfdsf").len == 0
except AssertionDefect:
  echo "Failed to not get non existing finish data of selected step."
