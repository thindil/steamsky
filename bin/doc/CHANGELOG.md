# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

### Fixed
- Crash when Drone member is trying to eat
- Move map buttons default look
- Crash on upgrading when setting upgrading cost is very low
- Crash on drawing map with not existing event
- Probably fixed possible crash in selling items in bases
- Crash when player don't have enough money for pay crew members

## [4.0.12] - 2020-02-16

### Fixed
- Crash on missing tools in crafting (issue #51)
- Grammar and sentence structure by ChrisEdBurt (pull request #54)
- Possible crash when save directory is not writeable

## [4.0.11] - 2020-02-02

### Fixed
- Showing info about finished Deconstructing order
- Showing info about lack of materials for crafting orders

## [4.0.10] - 2020-01-05

### Fixed
- Removed outdated tooltip for recruits list in bases
- Orders priority for one mob
- Possible generate that same name for savegame as old name
- Deleting events in debug menu

## [4.0.9] - 2019-12-15

### Fixed
- Info about setting difficulty level of upgrades costs
- Adding non existing ships to the game statistics
- Crash on finishing missions in bases
- Hang on starting a new game

## [4.0.8] - 2019-11-24

### Fixed
- Not working "Auto center map after set destination" configuration option

## [4.0.7] - 2019-11-10

### Changed
- Updated README.md

### Fixed
- Crash during hiring new crew members in bases (issue #49)
- Crash when trying to sell medicines to diseased base with full cargo (issue
  #49)
- Hide orders menu when moving ship (issue #49)
- Not working Close button in combat when you enter any other info screen
  (issue #49)
- Crash on map after selling last engine

## [4.0.6] - 2019-10-20

### Fixed
- Crash on showing battering ram info in Shipyard (issue #47)
- Inventory window doesn't show properly, most of the interface is not drawn,
  flickering on and off (issue #48)

## [4.0.5] - 2019-10-13

### Changed
- Updated README.md to avoid confusion with `run.sh` script (issue #45)
- Updated help with info that deconstructing doesn't give materials (issue #46)

### Fixed
- Crash when viewing available recruits in bases
- Possible crash during generating new missions in bases
- Crash during crafting (issue #46)

## [4.0.4] - 2019-09-29

### Changed
- Update contributing guide

### Fixed
- Crash on editing not visited base in debug menu
- Parsing factions names for selected flag in help

## [4.0.3] - 2019-09-15

### Changed
- Updated README.md

### Fixed
- Moved cheapest harpoons buying option to Military bases instead of Industrial

## [4.0.2] - 2019-09-01

### Changed
- Updated README.md
- Added cargo to traders ships so they will be slower in combat

### Fixed
- Possible crash on generating missions

## [4.0.1] - 2019-08-18

### Changed
- Updated README.md

### Fixed
- Crash on returning mission with max reputation as reward
- Patrol missions should be only on visited map cells
- Lack of fonts on Windows

## [4.0] - 2019-08-11

### Added
- New ship modules: medium double extended adamantium cabin, medium double
  luxury adamantium cabin, large double basic bronze cabin, large double
  advanced bronze cabin, large double extended bronze cabin, large double
  luxury bronze cabin, large double basic bronze cabin, large double advanced
  iron cabin, large double extended iron cabin, large double luxury iron
  cabin, large double basic steel cabin, large double advanced steel cabin,
  large double extended steel cabin, large double luxury steel cabin, large
  double advanced titanium cabin, large double extended titanium cabin, large
  double luxury titanium cabin, large double advanced orichalcum cabin, large
  double extended orichalcum cabin, large double luxury orichalcum cabin,
  large double extended adamanitum cabin, large double luxury adamantium
  cabin, huge double basic bronze cabin, huge double advanced bronze cabin,
  huge double extended bronze cabin, huge double luxury bronze cabin, huge
  double basic iron cabin, huge double advanced iron cabin, huge double
  extended iron cabin, huge double luxury iron cabin, huge double basic steel
  cabin, huge double advanced steel cabin, huge double extended steel cabin,
  huge double luxury steel cabin, huge double advanced titanium cabin, huge
  double extended titanium cabin, huge double luxury titanium cabin, huge
  double advanced orichalcum cabin, huge double extended orichalcum cabin,
  huge double luxury orichalcum cabin, huge double extended adamantium cabin,
  huge double luxury adamanitum cabin, medium bronze alchemy lab, medium iron
  alchemy lab, medium steel alchemy lab, medium titanium alchemy lab, medium
  orichalcum alchemy lab, medium adamantium alchemy lab, large bronze alchemy
  lab, large iron alchemy lab, large steel alchemy lab, large titanium alchemy
  lab, large orichalcum alchemy lab, large adamantium alchemy lab, huge bronze
  alchemy lab and huge iron alchemy lab
- Info about chance to damage for items
- Check if the game have permission to write to save directory
- Info about strength of ship's weapons, guns ammunitions, harpoons, melee
  weapons and crew members armors
- Option to show numeric values of crew members attributes, skills, etc

### Changed
- Updated help
- Starting ships for player from Undead and Drones factions
- Updated UI
- Updated README.md
- Updated contributing guide
- Default installation directory on Windows
- Updated crafter career with bonus to Printing skill

### Fixed
- Crash after showing info dialog in main menu
- Fonts size after first run of the game
- Buying items when they price is lower than weight
- Not adding training tools to tools category
- Showing ship info icons on Windows
- Crash after asking for the events in bases
- Showing movement buttons on Windows
- Don't show overload information when engineer is resting

## [3.9] - 2019-07-14

### Added
- New ship modules: huge extended bronze cabin, huge luxury bronze cabin, huge
  basic iron cabin, huge advanced iron cabin, huge extended iron cabin, huge
  luxury iron cabin, huge basic steel cabin, huge advanced steel cabin, huge
  extended steel cabin, huge luxury steel cabin, huge advanced titanium cabin,
  huge extended titanium cabin, huge luxury titanium cabin, huge advanced
  orichalcum cabin, huge extended orichalcum cabin, huge luxury orichalcum
  cabin, huge extended adamantium cabin, huge luxury adamantium cabin, small
  double basic bronze cabin, small double advanced bronze cabin, small double
  extended bronze cabin, small double luxury bronze cabin, small double basic
  iron cabin, small double advanced iron cabin, small double extended iron
  cabin, small double luxury iron cabin, small double basic steel cabin, small
  double advanced steel cabin, small double extended steel cabin, small double
  luxury steel cabin, small double advanced titanium cabin, small double
  extented titanium cabin, small double luxury titanium cabin, small double
  advanced orichalcum cabin, small double extended orichalcum cabin, small
  double luxury orichalcum cabin, small double extended adamantium cabin,
  small double luxury adamantiu cabin, medium double basic bronze cabin,
  medium double advanced bronze cabin, medium double extended bronze cabin,
  medium double luxury bronze cabin, medium double iron basic cabin, medium
  double advanced iron cabin, medium double extended iron cabin, medium double
  luxury iron cabin, medium double basic steel cabin, medium double advanced
  steel cabin, medium double extended steel cabin, medium double luxury steel
  cabin, medium double advanced titanium cabin, medium double extended
  titanium cabin, medium double luxury titanium cabin, medium double advanced
  orichalcum cabin, medium double extended orichalcum cabin and medium double
  luxury orichalcum cabin
- Order priority for training
- Option to edit player ship modules in debug menu
- Option to edit amount of money owned by bases in debug menu
- Info about crew to ship info
- Option to see crew, ship and cargo info after death

### Changed
- Updated help
- Updated UI
- Updated README.md

### Fixed
- Completing lists for bases and items names in debug menu
- Some typos in changelog
- Setting size of last messages window
- Debug argument not working with AppImage version
- Running the game after extracting AppImage
- Crash when starting the game in debug mode with not existing save directory
- Crash when base don't have more money to buying something
- Editing bases population and reputation in debug menu
- Setting size of ship info icon
- Infinite resizing of game window on enter crafting info
- Hide other windows on show more move map options
- Info about abandoned bases
- Resizing of the main game window
- Double opening help
- Crew members are going on rest during combat

## [3.8] - 2019-06-16

### Added
- New player's ships for inquisition faction
- Self repair ships during combat for pirates, poleis and independent factions
- Ability to have many owners to cabins and many workers to workshops, training
  rooms and medical rooms
- New ship modules: medium basic bronze cabin, medium advanced bronze cabin,
  medium extended bronze cabin, medium luxury bronze cabin, medium basic iron
  cabin, medium advanced iron cabin, medium extended iron cabin, medium luxury
  iron cabin, medium basic steel cabin, medium advanced steel cabin, medium
  extended steel cabin, medium luxury steel cabin, medium advanced titanium
  cabin, medium extended titanium cabin, medium luxury titanium cabin, medium
  advanced orichalcum cabin, medium extended orichalcum cabin, medium luxury
  orichalcum cabin, medium extended adamanium cabin, medium luxury adamantium
  cabin, large basic bronze cabin, large advanced bronze cabin, large extended
  bronze cabin, large luxury bronze cabin, large basic iron cabin, large
  advanced iron cabin, large extended iron cabin, large luxury iron cabin,
  large basic steel cabin, large advanced steel cabin, large extended steel
  cabin, large luxury steel cabin, large advanced titaniu cabin, large
  extended titanium cabin, large luxury titanium cabin, large advanced
  orichalcum cabin, large extended orichalcum cabin, large luxury orichalcum
  cabin, large extended adamantium cabin, large luxury adamantium cabin, huge
  basic bronze cabin and huge advanced bronze cabin
- Ability to set random career for new game
- Ability to set random faction for new game
- Ability to set random difficulty level for new game
- Ability to make autosaves
- Index to help entries

### Changed
- Updated crews on inquisition ships
- Updated player's character and starting crew for inquisition faction
- Contributing guide
- Maximum allowed owners for cabins and workers for all workshops
- Updated MODDING.md
- Updated interface
- Updated help
- Updated README.md
- Player ship upgrades

### Fixed
- Crews for Advanced poleis patrol ship mk VI, advanced poleis trader mk VI,
  advanced poleis patrol ship mk V, advanced poleis trader mk V, huge poleis
  patrol ship mk V, huge poleis trader mk V, advanced independent patrol ship
  mk VI, advanced independent trader mk VI, advaced independent patrol ship mk
  V, advanced independent trader mk V, huge independent patrol ship mk V, huge
  independent trader mk V, advanced pirates ship mk VI, advanced pirates
  trader mk VI, advanced pirates ship mk V, advanced pirates trader mk V, huge
  pirates ship mk V, huge pirates trader mk V, armored poleis patrol ship,
  armored poleis patrol ship mk II, armored poleis patrol ship mk III, armored
  poleis patrol ship mk IV, armored independent patrol ship, armored
  independent patrol ship mk II, armored independent patrol ship mk III,
  armored independent patrol ship mk IV, armored pirates ship, armored pirates
  ship mk II, armored pirates ship mk III and armored pirates ship mk IV
- Name for small extended adamantium cabin
- Crash on showing passenger info in ship info screen
- Giving orders to passengers
- Send previous trainee on break when new is assigned
- Return training tools back on going on break
- Assigning crew members to medical rooms
- Durability for small advanced steel cabin, small extended steel cabin and
  small luxury steel cabin
- Hide other menus on map when new appear
- Info about upgrading strength of harpoon guns in ship info
- Counting max fuel reduction upgrade for engines
- Help window resizes outside screen
- Crash on repair player ship in bases

## [3.7] - 2019-05-19

### Added
- New items: orichalcum maul, adamantium maul, bronze rapier, iron rapier,
  steel rapier, titanium rapier, orichalcum rapier, adamantium rapier, bronze
  spear, iron spear, steel spear, titanium spear, orichalcum spear and
  adamantium spear
- New crafting recipes: orichalcum maul, adamantium maul, bronze rapier, iron
  rapier, steel rapier, titanium rapier, orichalcum rapier, adamantium rapier,
  bronze spear, iron spear, steel spear, titanium spear, orichalcum spear and
  adamantium spear
- Option to delete events in debug menu
- Option to save game in debug menu
- New skills: rapiers and spears
- Ability to set difficulty levels for new game
- Added few new mobs
- Check for correctness of value ranges in data files
- Warning when fuel/food/drinks can drop below low level on player actions
  (trade, drop, give)
- Stop automovement when fuel/food/drinks drops below low level
- Remembering orders from last combat
- New starting ships for player from pirates faction
- Wake up crew on start combat and don't allow them to go sleep during it

### Changed
- Updated interface
- Always show help window maximized
- Reduced cost of recipes in bases
- Updated poleis, independent, pirates, drones and inquisition factions
  descriptions
- Updated MODDING.md (author: Michael Ax)
- Updated README.md (author: Michael Ax)
- Updated help UI
- Updated player's characters and player's ships for independent faction
- Updated player's characters and starting crew for pirates and drones faction
- Updated help
- Updated crews on poleis, independent, pirates and inquisition ships

### Fixed
- Typos in changelog
- Crash on trying to writing to the closed debug log file
- Checking availability of crafting recipes
- Check for tools for deconstructing items
- Start searching for modules on editing module name
- Missing keyboard shortcuts in wait menu
- Crash on counting max items to trade in bases
- Minimal level of skill after maluses from health, hunger, thirst and fatigue
- Possible crash on talking in bases
- Skills for one of Inquisition pilots
- Setting command line arguments in AppImage running script
- A lot of in-game messages (author: Michael Ax)
- Starting script for Linux (author: Michael Ax)
- Name of Huge pirates trader ship mk II
- Removed unneeded menu from messages and help
- Refresh game header information on drop and give items from ship cargo
- Closing windows with Escape key
- Info about low level of fuel/food/drinks
- Refresh game header information on trading items

## [3.6] - 2019-04-21

### Added
- New items: steel mace, titanium mace, orichalcum mace, adamantium mace,
  bronze dagger, iron dagger, steel dagger, titanium dagger, orichalcum
  dagger, adamantium dagger, bronze greatsword, iron greatsword, steel
  greatsword, titanium greatsword, orichalcum greatsword, adamantium
  greatsword, bronze warhammer, iron warhammer, steel hammer, titanium
  warhammer, orichalcum warhammer, adamantium hammer, bronze maul, iron maul,
  steel maul and titanium maul
- New crafting recipes: steel mace, titanium mace, orichalcum mace, adamantium
  mace, bronze dagger, iron dagger, steel dagger, titanium dagger, orichalcum
  dagger, adamantium dagger, bronze greatsword, iron greatsword, steel
  greatsword, titanium greatsword, orichalcum greatsword, adamantium
  greatsword, bronze warhammer, iron warhammer, steel warhammer, titanium
  warhammer, orichalcum warhammer, adamantium hammer, bronze maul, iron maul,
  steel maul and titanium maul
- Bigger chance to lost reputation in base after destroying friendly ship
- Better counting for info about time and fuel needed for reach destination on
  map
- Better counting amount of items for sale and buy
- New skills: Daggers, Two-handed swords and Two-handed maces
- Crew members lose morale with high fatigue
- Option to negotiate reward during accepting missions (money or reputation)
- Debug menu for modifying the game data

### Changed
- Default icon for unvisited bases
- Updated help
- Crash on healing wounded crew members in bases
- Items stats: bronze mace and iron mace
- Crafting recipes: bronze mace and iron mace
- Updated interface
- Faster gaining reputation in bases
- Even slower losing morale for factions with fanaticism
- Reduced size of save game files
- Transport passenger missions depends on cabins quality not a cabin type
- Reduced experience needed for gaining levels in skills and attributes
- Updated MODDING.md

### Fixed
- Clearing search field in trade after buy or sell items
- Don't show Sell All option when possible amount to sell is smaller than
  owned amount
- Don't show that crafting recipe is available if there are not enough
  crafting materials
- Check for having needed materials during crafting
- Show invalid path to error.log file in error report
- Crash on accepting transport passenger missions
- Crash on lack of passenger's attributes
- Crash on very low starting morale of transported passenger
- Crash on showing info about passenger in crew view
- Crash on finishing transport passenger missions
- Possible crash on counting current skill level for crew members
- Crash on dismissing passengers
- Waiting X minutes option don't use fuel
- Crash on putting on items in inventory

## [3.5] - 2019-03-24

### Added
- New ships: advanced huge poleis trader mk II, advanced huge independent
  trader mk II, advanced huge pirates trader mk II, advanced huge poleis
  patrol ship mk II, advanced huge independent patrol ship mk II, advanced
  huge poleis trader mk III, advanced huge independent trader mk III, advanced
  huge pirates trader mk III, advanced huge poleis patrol ship mk III,
  advanved huge independent patrol ship mk III, advanced huge poleis trader mk
  IV, advanced huge independent trader mk IV, advanced huge pirate trader mk
  IV, advanced huge poleis patrol ship mk IV, advanced huge independent patrol
  ship mk IV, advanced huge poleis trader mk V, advanced huge independent
  trader mk V, advanced huge pirate trader mk V, advanced huge poleis patrol
  ship mk V, advanced huge independent patrol ship mk V, advanced huge poleis
  trader mk VI, advanced huge independent trader mk VI, advanced huge pirates
  trader mk VI, advanced huge poleis patrol ship mk VI and advanced huge
  independent patrol ship mk VI
- Option to show or hide last messages window
- Option to set the game in full screen mode
- Self-closing message boxes and option to set delay for self-closing
- Option to set check boxes looks in themes
- Each battering ram attack cause unarmed enemy to stop for 1 combat round
- New factions flags: naturalarmor, toxicattack, sentientships, fanaticism and
  loner
- Natural armor ability for Undead and Drones faction members
- Toxic attack ability for Undead
- Sentient ships ability for Drones
- Fanaticism ability for Inquisition
- Bonus to damage from unarmed skill
- New skill: Maces
- New items: bronze mace and iron mace
- New crafting recipes: bronze mace and iron mace
- Loner ability to Independent and Pirates
- Better and worse food and drinks affect crew members morale

### Changed
- Updated interface
- Damage of battering rams depends on ship mass and speed too
- Updated help
- Battering rams attack once per 2 combat rounds
- Updated MODDING.md
- Raised base strength and lowered other stats for Undead player and crew.
- Group sky bases by factions during generating world

### Removed
- Configuration to show or hide last message

### Fixed
- Typos in changelog
- Default keyboard shortcut for show more move map options
- Missing AppImage .desktop file entries by probonopd #43
- Typo in README.md
- Showing new and load game buttons when data files are not loaded
- Showing info about overloading when pilot is on rest
- Enabling or disabling tooltips
- Using/taking off items in crew member inventory
- Loading inventories and cargo from data files
- Crash in combat when no gunner in player ship
- Crash on adding corpse to ships
- Harpoon gun work when enemy have an armor
- Crash on trying to trade with friendly ship
- Selling items to friendly ships
- Selling items resets item's profit info
- Crash on dismissing crew members
- Crash on delivering medicines to diseased base
- Crash on repair ship in bases
- Weapons on some ships

## [3.4] - 2019-02-24

### Added
- New ships: armored independent patrol ship mk II, armored poleis patrol
  ship mk III, armored independent patrol ship mk III, armored poleis patrol
  ship mk IV, armored independent patrol ship mk IV, advanced poleis patrol
  ship mk II, advanced independent patrol ship mk II, advanced poleis patrol
  ship mk III, advanced independent patrol ship mk III, advanced poleis patrol
  ship mk IV, advanced independent patrol ship mk IV, large poleis trader mk
  II, large independent trader mk II, large pirates trader mk II, large poleis
  patrol ship mk II, large independent patrol ship mk II, large poleis trader
  mk III, large independent trader mk III, large pirates trader ship mk III,
  large poleis patrol ship mk III, large independent patrol ship mk III, large
  poleis trader mk IV, large independent trader mk IV, large pirates trader mk
  IV, large poleis patrol ship mk IV, large independent patrol ship mk IV,
  large poleis trader mk V, large independent trader mk V, large pirates
  trader mk V, large poleis patrol ship mk V, large independent patrol ship
  mk V, huge poleis trader mk II, huge independent trader mk II, huge pirates
  trader mk II, huge poleis patrol ship mk II, huge independent patrol ship mk
  II, huge poleis trader mk III, huge independent trader mk III, huge pirates
  trader mk III, huge poleis patrol ship mk III, huge independent patrol ship
  mk III, huge poleis trader mk IV, huge independent trader mk IV, huge pirates
  trader ship mk IV, huge poleis patrol ship mk IV, huge independent patrol
  ship mk IV, huge poleis trader mk V, huge independent trader mk V, huge
  pirates trader mk V, huge poleis patrol ship mk V, huge independent patrol
  ship mk V, huge poleis trader mk VI, huge independent trader mk VI, huge
  pirates trader mk VI, huge poleis patrol ship mk VI and huge independent
  patrol ship mk VI
- Show README.md file in game about menu
- Added default icon for unvisited bases to themes
- Added default icon for player ship to themes
- Added default icon for empty space on map to themes
- Added default icon for player ship destination on map to themes
- Added default icon for story event location on map to themes
- Added default icon for showing ship overload warning to themes
- More keyboard shortcuts to move map and option to set them
- Don't allow to undock from bases if player ship is overloaded
- Keyboard shortcuts to change player ship speed

### Changed
- Made starting ships for Undead player a bit stronger (better battering ram
  and armor)
- Default font for map
- Updated README.md
- Updated MODDING.md
- Reduced fuel usage during waiting in place outside bases
- Updated interface
- Raised limit of minimal ship speed
- Updated help

### Removed
- Configuration to show or hide move map buttons

### Fixed
- Crash on selling ship modules in shipyard
- Crash when waiting in place
- Modules in ships: poleis trader mk IV, pirates trader mk IV, independent
  trader mk IV, poleis patrol ship mk IV and independent patrol ship mk IV
- Crash when checking cleanliness of cabins
- Crash on cleaning ship
- Crash on crew member back to work after rest
- Checking did player ship have enabled engines during changing ship speed
- Ship overloading
- Assigning crew member to harpoon gun
- Generating ship names
- Possible crash when showing the game map
- Crash when undocking speed is set to full stop
- Setting custom location for themes directory

## [3.3] 2019-01-27

### Added
- Option to enable or disable tooltips
- New ships: tiny pirates trader, small pirates trader, pirates trader, large
  pirates trader, huge pirates trader, advanced huge pirates trader, tiny
  poleis trader mk II, tiny independent trader mk II, tiny pirates trader mk
  II, tiny poleis patrol ship mk II, tiny independent patrol ship mk II, small
  poleis trader mk II, small independent trader mk II, small pirates trader mk
  II, small poleis patrol ship mk II, small independent patrol ship mk II,
  small poleis trader mk III, small independent trader mk III, small pirates
  trader mk III, small poleis patrol ship mk III, small independent patrol
  ship mk III, poleis trader mk II, independent trader mk II, pirates trader
  mk II, poleis patrol ship mk II, independent patrol ship mk II, poleis
  trader mk III, independent trader mk III, pirates trader mk III, poleis
  patrol ship mk III, indepdenent patrol ship mk III, poleis trader mk IV,
  independent trader mk IV, pirates trader mk IV, poleis patrol ship mk IV,
  independent patrol ship mk IV and armored poleis patrol ship mk II
- Missing message when enemy ship intercept player
- Lack of food or drinks reduce morale of crew members
- Separated icon on map for bases for each faction
- Separated icon on map for each event type
- Separated icon on map for each mission type
- Configuration files for custom themes

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
- UI/UX: Selecting character goal behaves different than other menuses
  (issue #35)
- Auto-resizing window makes it hard to exit character creation screen in
  specific curcumstances (issue #36)
- Name of faction in destroy ship goals
- Deleting mob by modification files
- Typo in character creation screen - "Carrer" instead of "Career" (issue #40)
- RFE: allow to scroll box info (during character creation) using keyboard
  (issue #41)

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
- Crews for advanced huge inquisition ship mk V, huge inquisition ship mk V,
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
