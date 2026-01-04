import ../src/[careers, factions, items]
import unittest2
include ../src/stories

suite "Unit tests for stories module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat".Path)
  loadItems("bin/data/items.dat".Path)
  loadCareers("bin/data/careers.dat".Path)
  loadFactions("bin/data/factions.dat".Path)
  loadStories("bin/data/stories.dat".Path)

  test "Get finish data of the selected step.":
    check:
      getStepData(storiesList["1"].steps[0].finishData, "condition") == "Rhetoric"

  test "Get finish data of the non-existing step.":
    check:
      getStepData(storiesList["1"].steps[0].finishData, "sdfdsf").len == 0

  test "Starting a story.":
    playerShip.crew = @[]
    playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
        homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1,
        1, 1, 0, 0], order: talk, loyalty: 100, skills: @[SkillInfo(index: 4,
        level: 4, experience: 0)], attributes: @[MobAttributeRecord(level: 3,
        experience: 0), MobAttributeRecord(level: 3, experience: 0),
        MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(
        level: 3, experience: 0)]))
    for i in 1 .. 1_000_000:
      startStory("Undead", dropItem)
      if currentStory.index.len > 0:
        break
    check:
      currentStory.index.len > 0

  test "Getting the current story text.":
    currentStory.finishedStep = any
    check:
      getCurrentStoryText().len > 0

  test "Clearing the current story.":
    let oldStory = currentStory
    clearCurrentStory()
    check:
      currentStory.index.len == 0
    currentStory = oldStory

  test "Getting a story location.":
    let (x, y) = getStoryLocation()
    check:
      x > 0 and y > 0
