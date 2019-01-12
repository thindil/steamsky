# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

### Added
- Option to enable or disable tooltips
- New ships: tiny pirates trader, small pirates trader, pirates trader, large
  pirates trader, huge pirates trader, advanced huge pirates trader, tiny
  poleis trader mk II, tiny independent trader mk II, tiny pirates trader mk
  II, tiny poleis patrol ship mk II, tiny independent patrol ship mk II, small
  poleis trader mk II and small independent trader mk II
- Missing message when enemy ship intercept player
- Lack of food or drinks reduce morale of crew members
- Separated icon on map for bases for each faction

### Changed
- Updated cargo bays for some ships
- Updated interface
- Trader talking skill and reputation affects award for completed missions
- Updated help
- Don't add enemy traders to attack on base and enemy patrol events
- Updated README.md
- Factions data with new bases icons
- Updated MODDING.md
- Each custom theme must be in separated directory inside themes directory

### Fixed
- Hide close button when showing stories
- Color of message when player can't dock to base because its docks are full
- Missing menu entries after selecting destination for ship
- Crash on failing mission
- Missing info about lack of tools when giving orders to whole crew
- Setting fonts size

## [3.2] - 2018-12-30

### Added
- New ship modules: medium steel cargo bay, medium titanium cargo bay, medium
  orichalcum cargo bay, medium adamantium cargo bay, large bronze cargo bay,
  large iron cargo bay, large steel cargo bay, large titanium cargo bay, large
  orichalcum cargo bay, large adamantium cargo bay, huge bronze cargo bay, huge
  iron cargo bay, huge steel cargo bay, huge titanium cargo bay, huge
  orichalcum cargo bay and huge adamantium cargo bay
- Option to center map on home base
- Keyboard shortcut (and option to set it) for center map on home base
- Maximum allowed size of modules to ship hulls
- Missing close button to goals list
- Missing tooltips for directories info in options
- Last bought price to items to show profit in trade
- Option to enable or disable last message info

### Changed
- Updated README.md
- Updated interface
- Updated ships hulls data with maximum allowed size of installed modules
- Updated help
- Updated MODDING.md
- Updated new game message
- Updated cargo bays for some ships
- Reduced chance to meet strongest enemies on weaker ships
- More dynamic changes of morale

### Fixed
- Info about item damage
- Unnecessary changes of name of save game file
- Crash on item damage when there is more than one that same item
- Saving default settings for player faction and career for new game
- Size of website button
- Showing info about available ammo for guns in combat
- Showing only visited bases of selected faction on bases list
- Crash after recruit new crew member in bases
- Auto back to map when all recruits are hired
- Resetting hire negotiations
- Info about available missions after accepting mission
- Player ship for general career in pirates faction
- Losing morale when crew member can't go sleep
- Warning in console when giving items to crew members
- Crash when player don't have enough money for paying crew members
- Color of message when player don't have moneys for pay for docking
- Show orders menu outside map

## [3.1] - 2018-12-02

### Added
- New ship modules: advanced huge adamantium hull, huge adamantium engine,
  advanced huge adamantium engine, adamantium harpoon gun, adamantium 80mm gun,
  adamantium 100mm gun, adamantium 120mm gun, small adamantium workshop, medium
  bronze cargo bay and medium iron cargo bay
- Options to update game data (ships, mobs, factions, etc) by modification files
- New ships: huge pirate ship mk VI, advanced huge pirate ship mk VI, huge
  undead ship mk VI, advanced huge undead ship mk VI, huge attacking drone mk
  VI, advanced huge attacking drone mk VI, huge inquisition ship mk VI,
  advanced huge inquisition ship mk VI
- Few new mobs for ships crews
- Option to set starting base for new game

### Changed
- Raised maximum amount of modules for huge adamantium hull
- Raised default durability of huge orichalcum engine and orichalcum harpoon
  gun
- Updated README.md
- Removing game data
- Updated MODDING.md
- Final enemy for story with undead
- Updated interface
- Crews for advanced huge inquistition ship mk V, huge inquisition ship mk V,
  advanced huge undead ship mk V, huge undead ship mk V, advanced huge pirate
  ship mk V and huge pirate ship mk V

### Fixed
- Some typos in old changelog
- Removing mobs from game data
- Chance for damage for items during usage
- Going on break after finishing crafting
- Log entry about removed mob
- Scrolling messages lists to last messsage
- Even more autoresize GUI elements on mouse hover (issue #31)
- Dying in combat from lack of fuel
- Index for small steel training room
- Crash on showing know events list
- Crash on asking friendly ships for bases and events
- Saving crew members faction and homebase
- Crash in combat on destroying module in player ship
- Repair material for titanium armor
- Setting player ship speed after combat
- Entering text cause activation of menu
- Entering text cause activation of menu in numeric fields
- Close windows when entering texts
- Entering text cause activation of shortcuts in options
- Setting key for move ship left
- Showing info about player reputation in bases
- Close wait orders and move map windows when entering text
- Crash during combat on showing messages
