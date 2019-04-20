# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

### Added
- New items: steel mace, titanium mace, orichalcum mace, adamantium mace,
  bronze dagger, iron dagger, steel dagger, titanium dagger, orichalcum
  dagger, adamantium dagger, bronze greatsword, iron greatsword, steel
  greatsword, titanium greatsword, orichalcum greatsword, adamantium
  greatsword, bronze warhammer, iron warhammer, steel hammer, titanium
  warhammer, orichalcum warhammer, adamantium hammer, bronze maul, iron maul,
  steel maul
- New crafting recipes: steel mace, titanium mace, orichalcum mace, adamantium
  mace, bronze dagger, iron dagger, steel dagger, titanium dagger, orichalcum
  dagger, adamantium dagger, bronze greatsword, iron greatsword, steel
  greatsword, titanium greatsword, orichalcum greatsword, adamantium
  greatsword, bronze warhammer, iron warhammer, steel warhammer, titanium
  warhammer, orichalcum warhammer, adamantium hammer, bronze maul and iron
  maul
- Bigger chance to lost reputation in base after destroying friendly ship
- Better counting for info about time and fuel needed for reach destination on
  map
- Better counting amount of items for sale and buy
- New skills: Daggers, Two-handed swords and Two-handed maces
- Crew members lose morale with high fatigue
- Option to negotiate reward during accepting missions (money or reputation)

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
- Crash on dismissins passengers
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
- Loading inventories and cargos from data files
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
