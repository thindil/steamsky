# Changelog
All notable changes to this project will be documented in this file.

## [3.0.6] - 2019-03-03

### Fixed
- Possible crash when showing the game map
- Crash when undocking speed is set to full stop
- Setting custom location for themes directory

## [3.0.5] - 2019-02-10

### Changed
- Made starting ships for Undead player a bit stronger (better battering ram
  and armor)

### Fixed
- Name of faction in destroy ship goals
- Deleting mob by modification files
- Typo in character creation screen - "Carrer" instead of "Career" (issue #40)
- RFE: allow to scroll box info (during character creation) using keyboard
  (issue #41)
- Checking did player ship have enabled engines during changing ship speed
- Ship overloading
- Assigning crew member to harpoon gun
- Generating ship names

## [3.0.4] - 2019-01-20

### Fixed
- Missing info about lack of tools when giving orders to whole crew
- Setting fonts size
- UI/UX: Selecting character goal behaves different than other menuses
  (issue #35)
- Auto-resizing window makes it hard to exit character creation screen
  in specific curcumstances (issue #36)

## [3.0.3] - 2019-01-06

### Added
- Missing message when enemy ship intercept player

### Changed
- Don't add enemy traders to attack on base and enemy patrol events

### Fixed
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
- Hide close button when showing stories
- Color of message when player can't dock to base because its docks are full
- Missing menu entries after selecting destination for ship
- Crash on failing mission

## [3.0.2] - 2018-12-16

### Added
- Missing close button to goals list
- Missing tooltips for directories info in options

### Changed
- Updated README.md
- Updated new game message

### Fixed
- Entering text cause activation of menu in numeric fields
- Close windows when entering text
- Entering text cause activation of shortcuts in options
- Showing info about player reputation in bases
- Close wait orders and move map windows when entering text
- Crash during combat on showing messages
- Info about item damage
- Unnecessary changes of name of save game file
- Crash on item damage when there is more than one that same item
- Saving default settings for player faction and career for new game
- Size of website button
- Showing info about available ammo for guns in combat
- Showing only visited bases of selected faction on bases list

## [3.0.1] - 2018-11-25

### Changed
- Raised maximum amount of modules for huge adamantium hull
- Raised default durability of huge orichalcum engine and orichalcum harpoon
  gun
- Updated README.md

### Fixed
- Some typos in changelog
- Removing mobs from game data
- Chance for damage for items during usage
- Going on break after finishing crafting
- Log entry about removed mob
- Scrolling messages list to last message
- Even more autoresize GUI elements on mouse hover (issue #31)
- Dying in combat from lack of fuel
- Index for small steel training room
- Crash on showing know events list
- Crash on asking friendly ships for bases and events
- Saving crew members home base and faction
- Crash in combat on destroying module in player ship
- Repair material for titanium armor
- Setting player ship speed after combat
- Entering text cause activation of menu
- Setting key for move ship left

## [3.0] - 2018-11-04

### Added
- New crafting recipes: small book of alchemy, small book of cooking, small
  book of gunsmithing, small book of metalsmithing, small book of medicine,
  small book of farming, small book of woodcutting, small book of brewing,
  small book of blacksmithing, small book of woodworking, small book of
  leatherworking, wooden training dummy, small wooden traps, small book of
  printing and wooden printing set
- Welcoming message to each new game
- Option to set order of displayed messages
- Option to zoom in and zoom out map with mouse wheel and keyboard shortcuts
- Variables for keyboard shortcuts for zooming map
- Option to auto ask for bases and events when ship dock to base
- New ship modules: small orichalcum workshop, small adamantium training room,
  adamantium cockpit, small adamantium alchemy lab, small adamantium cargo bay,
  small adamantium furnace, heavy adamantium armor, small adamantium
  greenhouse, small adamantium water collector, small adamantium medical room,
  small extended adamantium cabin, small luxury adamantium cabin, heavy
  adamantium turret, heavy adamantium battering ram and huge adamantium hull
- Option to show or hide buttons for map movement

### Changed
- Updated help
- Updated interface
- Redesigned combat UI (issue #30)
- Updated README.md
- Updated MODDING.md
- Don't allow to disable last engine even when player ship is docked to base
- Updated CONTRIBUTING.md

### Fixed
- Showing information about reputation in base in bases list
- Reading default player gender from new game configuration
- Autoresize GUI elements on mouse hover (issue #31)
- Typo in finished crafting message
- Return to old speed after finished combat
- Assigning guns to turrets during creating ships
- Starting morale and loyalty for recruits
- Generating enemy ships for missions
- Don't show new game and load game buttons when there is error in loading game
  data
- Removing available careers
- Index for bronze sickle recipe
- Updating crew inventory
- Possibility of disease event in bases
- Reading bases population from saves

## [2.9] - 2018-10-07

### Added
- New crafting recipes: adamantium short sword, adamantium sword, adamantium
  shield, adamantium chestplate, adamantium helmet, adamantium armsguard,
  adamantium legsguard, wooden cockpit simulator, sheet of paper, empty book,
  small book of engineering, wooden gun simulator, small book of rethoric and
  wooden small toys
- New items: adamantium sword, adamantium shield, adamantium chestplate,
  wooden cockpit simulator, adamantium helmet, small book of engineering,
  wooden gun simulator, adamantium armsguard, small book of rhetoric, wooden
  small toys, adamantium legsguard, small book of alchemy, small book of
  cooking, small book of gunsmithing, small book of metalsmithing, small book
  of medicine, small book of farming, small books of woodcutting, small book
  of brewing, small book of blacksmithing, small book of woodworking, small
  book of leatherworking, wooden training dummy, small wooden traps, small
  book of printing, wooden printing set, sheet of paper and empty book
- Ability to show current game directories in options
- Optional names for careers in factions
- New items types: cockpitsimulator, engineersbook, gunsimulator,
  rhetoricbook, smalltoys, alchemybook, cookingbook, gunsmithbook,
  metalsmithbook, medicinebook, farmingbook, woodcuttingbook, brewingbook,
  blacksmithbook, woodworkingbook, leatherworkingbook, trainingdummy,
  smalltraps, printingbook, printingset, paper and emptybook
- Option to set fonts size in game
- User Interface themes
- New skill: printing
- Recruits skills and attributes depends on player reputation in base
- Size to bases
- Ability to sort bases in bases list by population
- Waiting outside skybases uses fuel too

### Changed
- Help factions flags variables now return list of factions which have that
  flag enabled
- Update help
- Updated MODDING.md
- Updated interface
- Moved player's careers to separated data file
- Training skills on ship require tools
- How chance for base owner is calculated
- Updated README.md

### Fixed
- Reclaiming turrets and workshops by crew members after back to work after
  rest
- Info about returning mission in bases info
- Armor stat of orichalcum shield
- Typos in careers descriptions
- Possible memory leaks during loading game data
- Keyboard shortcuts for moving map

## [2.8] - 2018-09-09

### Added
- New crafting recipes: adamantium mold, adamantium gunsmith set, adamantium
  cooking set, adamantium sickle, adamantium saw, adamantium bucket, adamantium
  blacksmith set, adamantium woodworker set, adamantium repair tools,
  adamantium sewing kit, adamantium 80mm ammo, adamantium 100mm ammo,
  adamantium 120mm ammo and adamantium harpoon
- New items: adamantium gunsmith set, adamantium cooking set, adamantium
  sickle, adamantium saw, adamantium bucket, adamantium blacksmith set,
  adamantium woodworker set, adamantium repair tools, adamantium sewing kit,
  adamantium 80mm ammo, adamantium 100mm ammo, adamantium 120mm ammo,
  adamantium harpoon and adamantium short sword
- Separated healing tools for each faction
- Variable with healing tool name to help
- Separated healing skill for each faction
- Ability to disable genders for selected factions
- Ability to made selected factions immune to diseases
- Ability to disable tiredness for selected factions
- Ability to disable morale for selected factions
- Separated directory for game modifications and ability to set it by starting
  arguments
- Option to show help depending on player faction
- Option to read game main data from different files than game.dat
- High morale adds small bonus to skills
- Option to remove existing recipes, factions, goals, help entries, items,
  ship modules, ships and stories from modifications files
- Ability to select player career which affect player starting ship and
  equipment
- New ships: player starting ships for trader, hunter, explorer and crafter
  careers
- Bonus to gained experience based on career selected by player
- Each crew members, recruit, passenger have home skybase
- Hide from player factions without careers
- Separated faction for each mobile

### Changed
- Updated interface
- Updated MODDING.md
- Updated help
- Drones faction don't have morale, tiredness, genders and is immune to
  diseases
- Undead faction don't have morale, tiredness and is immune to diseases
- Moved all data files to one directory
- Raised chance for sky bases from factions other than Poleis
- Updated README.md
- Reduced weight of blacksmith sets
- How morale is gained and lost
- Blocked gaining experience by pilot, engineer and gunner when ship is docked
  to base
- Only resting crew members can be wounded in bar brawl event

### Fixed
- Recipe for adamantium plates
- Crash when opening game stats with finished goal
- Searching for food and drinks in crew members inventory too
- Showing info about thirst/hunger/tiredness of crew members
- Showing info about lack of workshop in crafting menu
- Finding fonts on Windows
- Don't show Menu button in game statistics after player death
- Waiting 1 minute button
- Possible crash when reporting error
- Lack of keyboard shortcut for back button in license window

### Removed
- HealingSkill variable from help

## [2.7] - 2018-08-12

### Added
- New ship modules: huge orichalcum hull, advanced huge orichalcum hull, huge
  orichalcum engine, advanced huge orichalcum engine, orichalcum harpoon gun,
  orichalcum 60mm gun, orichalcum 80mm gun, orichalcum 100mm gun and orichalcum
  120mm gun
- Finishing missions raise or lower (if failed) player character morale
- New enemy ships: large clockwork drone mk V, large pirate ship mk V, large
  undead ship mk V, large inquisition ship mk V, large attacking drone mk V,
  huge pirate ship mk V, advanced huge pirate ship mk V, huge undead ship mk V,
  advanced huge undead ship mk V, huge attacking drone mk V, advanced huge
  attacking drone mk V, huge inquisition ship mk V and advanced huge
  inquisition ship mk V
- More text formatting options to help
- Option to stories to set which factions are not allowed to start selected
  story
- Option to enable or disable ship engines
- Showing current unarmed skill name in help
- Separated player starting character for each faction
- Starting player character data for undead and drones factions
- Starting crew data for undead, drones and inquisition factions
- Separated player starting ship for each faction
- New ship: player starting ship for pirates, undead, drones and inquisition
  factions
- Description to factions
- New item types: adamantiumore and adamantium
- Option to select player faction at starting new game
- New items: adamantium ore, adamantium plates and adamantium mold
- Separated food and drinks types for each faction
- New crafting recipe: adamantium plates

### Changed
- Updated interface
- Updated README.md
- Base name of savegame files
- Updated help
- Each faction have separated reputation with each other
- Updated MODDING.md
- Separated friendly/enemy relations between factions
- Updated game data
- Final enemy for story with undead
- Raised chance for sky bases from factions other than Poleis

### Fixed
- Keyboard shortcuts for some orders
- Updating crew orders after combat
- Info about module damage on ship info screen
- Refreshing combat UI after change crew orders on crew screen
- Showing info about events in bases
- Crash on start game
- Renaming ship in game not change save game file name
- Showing upgrade button when upgrade is unavailable for that ship module
- Showing operate harpoon gun in crew members orders list
- Running game on Linux

### Removed
- HealingTool variable from help

## [2.6] - 2018-07-15

### Added
- New goals: kill 800 inquisitors in melee combat, kill 1000 inquisitors in
  melee combat, kill 2000 inquisitors in melee combat, kill 4000 inquisitors
  in melee combat, kill 6000 inquisitors in melee combat, kill 8000 inquisitors
  in melee combat and kill 10000 inquisitors in melee combat
- Keyboard shortcut (and option to set it) to center map on player ship
- Morale to crew members
- Loyalty to crew members
- New type of ship modules: training room
- New ship modules: small bronze training room, small iron training room, small
  steel training room, small titanium training room, small orichalcum training
  room, orichalcum cockpit, small orichalcum alchemy lab, small orichalcum
  cargo bay, large orichalcum engine, large advanced orichalcum engine, small
  orichalcum furnace, large orichalcum hull, advanced large orichalcum hull,
  heavy orichalcum armor, small orichalcum greenhouse, small orichalcum water
  collector, small orichalcum medical room, small advanced orichalcum cabin,
  small extended orichalcum cabin, small luxury orichalcum cabin, heavy
  orichalcum turret and heavy orichalcum battering ram
- Ability to train skills on ship
- Keyboard shortcuts (and option to set them) to move map
- New item type: storyitem
- New items: strange medallion and strange metal scrap
- Keyboard shortcuts (and option to set them) to move cursor around map
- Keyboard shortcut (and option to set it) for selecting map field
- Randomly generated stories
- Option to set GTK share directory via console parameter (Linux only)
- Ability to have few saved games at once

### Changed
- Updated help
- Updated interface
- Updated MODDING.md
- Updated README.md

### Fixed
- Resting during automovement
- Adding keyboards shortcuts with special keys
- Loading missions data from saved game
- Mouse click on map field with player ship should bring orders menu
- Docking to bases where player have very low reputation
- Saving reputation in bases
- Crash on giving invalid order to crew member
- Showing buy option when trader don't have that item
- Moving map near borders
- Crash during selecting event as destination for player ship
- Showing complete mission button on wrong bases
- Keyboard shortcuts for some orders
- Don't show item amount in base/trader ship if player can't buy them
- Possible crash on deleting messages

## [2.5] - 2018-06-17

### Added
- New items: orichalcum chestplate, orichalcum helmet, orichalcum armsguard and
  orichalcum legsguard
- New crafting recipes: orichalcum chestplate, orichalcum helmet, orichalcum
  armsguard and orichalcum legsguard
- Option to set GTK directories via console parameters (Linux only)
- New goals: kill 1000 enemies in melee combat, kill 2000 enemies in melee
  combat, kill 4000 enemies in melee combat, kill 6000 enemies in melee combat,
  kill 8000 enemies in melee combat, kill 10000 enemies in melee combat, kill
  800 pirates in melee combat, kill 1000 pirates in melee combat, kill 2000
  pirates in melee combat, kill 4000 pirates in melee combat, kill 6000 pirates
  in melee combat, kill 8000 pirates in melee combat, kill 10000 pirates in
  melee combat, kill 800 undead in melee combat, kill 1000 undead in melee
  combat, kill 2000 undead in melee combat, kill 4000 undead in melee combat,
  kill 6000 undead in melee combat, kill 8000 undead in melee combat and kill
  10000 undead in melee combat
- Option to set player faction
- Random equipment to recruits in bases
- Payment for crew members
- Negotiating hiring of recruits in bases
- Time-based contracts with crew members

### Changed
- Raised gained experience in combat
- Updated interface
- Updated README.md
- New format of game data file which made old incompatible
- Updated MODDING.md
- New format of hall of fame data file which made old incompatible
- New format of items data file which made old incompatible
- New format of ships modules data file which made old incompatible
- New format of recipes data file which made old incompatible
- New format of mobiles data file which made old incompatible
- New format of ships data file which made old incompatible
- New format of goals data file which made old incompatible
- New format of help data file which made old incompatible
- Moved NPC factions data to data file
- Updated help

### Fixed
- Crash on showing map cell info
- Some typos in changelog
- Loading player ship cargo from savegame file
- Default keyboard shortcuts for ship movement
- Crash on moving map
- Crash on player death
- Hide map after player death
- Showing death screen after lost ship fight
- Deleting savegame when it is saved in different directory than default
- Possible crash on saving hall of fame data
- Updating crew orders in combat
- Showing buying option for unavailable items
- Loading saved game statistics
- Loading player ship data
- Setting set mission as destination for ship button
- Crash when trying to remove destroyed ship module
- Keyboard shortcut for shipyard in orders menu
- Again, lots of grammar/spelling errors by LJNIC (pull request #25)
- Ability to dock when base docks are full
- Showing info about unknown bases on map
- Showing help button in crew member inventory
- Counting hire price for recruits
- Lots of grammar/spelling errors in README.md by MagiOC (pull request #26)
- Crash on repair whole ship in bases
- Showing Wait order when there are full docks in base
- Refreshing map after stop automove due to lack of crew members on position

### Removed
- Abandoned faction

## [2.4] - 2018-05-20

### Added
- New items: titanium woodworker set, orichalcum woodworker set, titanium
  repair tools, orichalcum repair tools, titanium sewing kit, orichalcum
  sewing kit, orichalcum 60mm ammo, orichalcum 80mm ammo, orichalcum 100mm
  ammo, orichalcum 120mm ammo, orichalcum harpoon, orichalcum short sword,
  orichalcum sword and orichalcum shield
- New crafting recipes: titanium woodworker set, orichalcum woodworker set,
  titanium repair tools, orichalcum repair tools, titanium sewing kit,
  orichalcum sewing kit, orichalcum 60mm ammo, orichalcum 80mm ammo,
  orichalcum 100mm ammo, orichalcum 120mm ammo, orichalcum harpoon, orichalcum
  short sword, orichalcum sword and orichalcum shield
- Showing license full text in info about game
- New sky base type: military
- Damage type for personal weapons
- Option to search through messages
- More info about player death in melee combat
- Option to set max amount of stored messages in game
- Option to set max amount of saved messages
- Statistics for killed enemies in melee combat
- New type of goal: kill X enemies in melee combat
- New goal: kill 800 enemies in melee combat

### Changed
- Updated interface
- Updated README.md
- Updated items data
- Updated MODDING.md
- Updated recipes data
- Updated help
- New format of save games which made saves from previous versions incompatible

### Fixed
- Double attack in melee combat after kill
- Generating cargo in first time visited bases
- Crash in repairing ship in bases
- Crash in buying items in bases
- Crash in combat on death of crew member
- Assigning gunners to harpoon guns in enemies ships
- Typo in old changelog file
- Shooting by enemy when no gunner at gun
- Showing game statistics after player death
- Showing last message after buying recipes/ship repairs/crew healing in bases
- Don't show empty know events list
- Auto back to sky map after buying all recipes in base
- Refreshing combat view after player back on ship
- Melee combat stops when no harpoons attached to ships
- Showing empty goals category in goals select menu
- Showing sky map after starting/loading game
- Block ability to back on ship during combat if no harpoon active
- Checking for needed materials before start crafting
- Don't allow to go on rests crew members in boarding party when ships are not
  connected by harpoons
- Issue #17 - Crash when boarding party member don't have weapon
- Showing information about crew member order when he/she boarding enemy ship
- Probably fixed issue #18 - Crash when crew member have more than maximum health
- Gaining experience by crew members during boarding enemy ship
- Issue #20 - Crash when attacking with ram
- Issue #19 - Crash when crew member have non-existent item equipped

## [2.3] - 2018-04-22

### Added
- New items: titanium mold, orichalcum mold, titanium gunsmith set, orichalcum
  gunsmith set, titanium cooking set, orichalcum cooking set, titanium sickle,
  orichalcum sickle, titanium saw, orichalcum saw, titanium bucket, orichalcum
  bucket, titanium blacksmith set and orichalcum blacksmith set
- New crafting recipes: titanium mold, orichalcum mold, titanium gunsmith set,
  orichalcum gunsmith set, titanium cooking set, orichalcum cooking set,
  titanium sickle, orichalcum sickle, titanium saw, orichalcum saw, titanium
  bucket, orichalcum bucket, titanium blacksmith set and orichalcum blacksmith
  set
- Ability to enable/disable interface animations
- Ability to set type of interface animations
- Descriptions for skills and stats
- Option to set how much hands weapons needs to use (one-handed, two-handed)
- New orders priorities: for defending ship and boarding enemy ship which made
  saves from previous version incompatible
- Ability to board player ship by enemies
- Option to give orders to boarding party (if player is in this party)

### Changed
- Updated interface
- Updated game data
- Updated MODDING.md
- Changed item type MeleeWeapon to Weapon
- Updated data for weapons
- Updated data for mobs
- Updated help

### Fixed
- Lots of grammar/spelling errors by LJNIC (pull request #16)
- Clearing combat orders after combat

## [2.2] - 2018-03-25

### Added
- New enemy ships: armored pirate ship mk IV, attacking clockwork drone mk IV,
  armored attacking drone mk IV, inquisition ship mk IV, armored inquisition
  ship mk IV, large clockwork drone mk IV, large pirate ship mk IV, undead ship
  mk IV, large undead ship mk IV, large inquisition ship mk IV, large attacking
  drone mk IV, advanced attacking drone mk IV, advanced pirate ship mk IV,
  advanced undead ship mk IV, advanced inquisition ship mk IV, huge pirate ship
  mk IV, advanced huge pirate ship mk IV, huge undead ship mk IV, huge
  attacking drone mk IV, advanced huge undead ship mk IV, advanced huge
  attacking drone mk IV, huge inquisition ship mk IV and advanced huge
  inquisition ship mk IV
- Keyboard shortcuts for ship movement and menu
- Option to set keyboard shortcuts for ship movement and menu
- New item types: orichalcumore and orichalcum
- New items: orichalcum ore and orichalcum plates
- New crafting recipe: orichalcum plates

### Changed
- Updated MODDING.md
- Updated help
- Updated interface
- Updated README.md

### Fixed
- Movement buttons after set destination for ship
- Moving ship to destination point
- Refreshing move buttons after finish mission
- Crash on showing available missions
- Crash when generate bases cargo
- Showing repair ship option in small and medium bases
- Showing combat after show info windows
- Crash on setting module upgrade when no upgrading materials are available

## [2.1] - 2018-02-25

### Added
- New ship modules: titanium armor, heavy titanium armor, titanium turret,
  small titanium greenhouse, small titanium water collector, small titanium
  medical room, advanced titanium cabin, extended titanium cabin, luxury
  titanium cabin, heavy titanium turret, heavy titanium battering ram, small
  titanium workshop, huge titanium hull, advanced huge titanium hull, huge
  titanium engine, advanced huge titanium engine, titanium harpoon gun,
  titanium 40mm gun, titanium 60mm gun, titanium 80mm gun, titanium 100mm gun
  and titanium 120mm gun
- New items: titanium 40mm ammo, titanium 60mm ammo, titanium 80mm ammo,
  titanium 100mm ammo, titanium 120mm ammo, titanium harpoon, titanium short
  sword, titanium sword, titanium shield, titanium chestplate, titanium helmet,
  titanium armsguard and titanium legsguard
- New crafting recipes: titanium 40mm ammo, titanium 60mm ammo, titanium 80mm
  ammo, titanium 100mm ammo, titanium 120mm ammo, titanium harpoon, titanium
  short sword, titanium sword, titanium shield, titanium chestplate, titanium
  helmet, titanium armsguard and titanium legsguard
- New enemy ship: pirate ship mk IV

### Changed
- Updated README.md
- Renamed all cabins to small cabins
- Updated help
- User Interface from console to graphical

### Fixed
- Typos in old changelog files
- Generating cargo in abandoned bases
- Crew member equipment after moving item to cargo
