discard """
  exitcode: 0
  output: '''Loading the game data.
Testing progressStory.'''
"""

import std/tables
import ../../src/[factions, careers, game, items, stories, stories2, types]

echo "Loading the game data."
if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
if storiesList.len == 0:
  loadStories("../bin/data/stories.dat")

playerShip.crew = @[]
playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
    homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1, 1,
    1, 0, 0], order: talk, loyalty: 100, skills: @[SkillInfo(index: 4, level: 4,
    experience: 0)], attributes: @[MobAttributeRecord(level: 3, experience: 0),
    MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
    experience: 0), MobAttributeRecord(level: 3, experience: 0)]))
for i in 1 .. 1_000_000:
  startStory("Undead", dropItem)
  if currentStory.index.len > 0:
    break

echo "Testing progressStory."
discard progressStory()