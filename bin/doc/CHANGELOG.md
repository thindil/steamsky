# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

### Fixed
- Crash during boarding when attacker or defender are too tired
- Crash on selling items when no enough profit to pay crew members
- Sorting crew members in schools
- Showing orders keyboard shortcut in Firsts Steps in help
- Crash in combat when enemy crew member has too low morale

## [5.0.6] - 2020-09-20

### Fixed
- Crew members don't gain experience in begining of the game
- Don't show crafting order when any workshop don't have set it

## [5.0.5] - 2020-09-13

### Changed
- Updated help
- Better checking for tools for crafting and training
- Better checking for tools during giving orders
- When tired crew member going on rest, returns tools
- When giving orders always take tools from crew members

### Fixed
- Can't sell items when one of crew members has set percent from profit as a
  payment
- Default icons for moving map UI

## [5.0.4] - 2020-09-03

### Added
- Ability to assign a crew member to training room in ship info

### Fixed
- Crash on showing shipyard when module type was set
- Updating crew info when someone was assigned or removed from module
- Checking for crafting room for study and deconstruct orders

## [5.0.3] - 2020-08-02

### Fixed
- Showing escape option from empty bases
- Taking dock payment in empty bases
- Crash on showing orders in empty bases
- Don't show drop warning by default when looting empty bases

## [5.0.2] - 2020-07-05

### Added
- Missing message after give an item from ship cargo to a crew member

### Fixed
- Update header crew information when assigned crew to new module in ship info
  screen
- Assigning crew member to cabin when the cabin is full
- Showing help in ship info
- Typo in info about low level of item when giving it to a crew member
- No check for amount of items during droping them in abandoned bases

## [5.0.1] - 2020-05-20

### Changed
- Updated README.md

### Fixed
- Crash during resting when crew member have very high tired level

## [5.0] - 2020-05-17

### Added
- New goals: visit 10 undead bases, visit 1 drones base, visit 3 drones bases,
  visit 5 drones bases, visit 10 drones bases, visit 1 inquisition base, vitis
  3 inquisition bases, visit 5 inquisition bases, visit 10 inquisition bases,
  destroy 200 poleis ships, destroy 250 poleis ships, destroy 500 poleis
  ships, destroy 1000 poleis ships, destroy 1500 poleis ships, destroy 2000
  poleis ships, destroy 2500 poleis ships, destroy 200 independent ships,
  destroy 250 independent ships, destroy 500 independent ships, destroy 1000
  independent ships, destroy 1500 independent ships, destroy 2000 independent
  ships, destroy 2500 independent ships, kill 800 poleis members in melee
  combat, kill 1000 poleis members in melee combat, kill 2000 poleis members
  in melee combat, kill 2500 poleis members in melee combat, kill 800
  independent members in melee combat, kill 1000 independent members in melee
  combat, kill 2000 independent members in melee combat, kill 2500 independent
  members in melee combat
- New items: bronze printing press, iron printing press, steel printing press,
  titanium printing press, orichalcum printing press and adamantium printing
  press
- New crafting recipes: bronze printing press, iron printing press, steel
  printing press, titanium printing press, orichalcum printing press and
  adamantium printing press

### Changed
- Reduced amount of lost reputation for escaping from bases
- Updated bases data with new items and recipes
- Updated crafting recipes and skills training with tool quality requirement
- Updated help
- Updated interface
- Bases should no longer created at map borders

### Fixed
- Reputation requirement for recipes for steel, titanium, orichalcum and
  adamantium warhammers
- Added missing fonts
- Setting crew members health in debug menu
- Crash when player was killed in debug menu
- Game header info on player death
- Don't show everything on player death
- Crash on start the game
- Showing orders menu after docking
- Saving current configuration for bases prices bonus

## [4.9] - 2020-04-19

### Added
- New items: orichalcum 80mm explosive ammo, orichalcum 100mm piercing ammo,
  orichalcum 100mm explosive ammo, orichalcum 120mm piercing ammo, orichalcum
  120mm explosive ammo, adamantium 80mm piercing ammo, adamantium 80mm
  explosive ammo, adamantium 100mm piercing ammo, adamantium 100mm explosive
  ammo, adamantium 120mm piercing ammo and adamantium 120mm explosive ammo
- New crafting recipes: orichalcum 80mm explosive ammo, orichalcum 100mm
  piercing ammo, orichalcum 100mm explosive ammo, orichalcum 120mm piercing
  ammo, orichalcum 120mm explosive ammo, adamantium 80mm piercing ammo,
  adamantium 80mm explosive ammo, adamantium 100mm piercing ammo, adamantium
  100mm explosive ammo, adamantium 120mm piercing ammo and adamantium 120mm
  explosive ammo
- Version check to the saved games
- Better info about development/stable version of the game
- Better info about invalid saved games
- Better info about why crew member wasn't healed
- New goals: gain max reputation in 1 undead base, gain max reputation in 3
  undead bases, gain max reputation in 5 undead bases, gain max reputation in
  1 drones base, gain max reputation in 3 drones bases, gain max reputation in
  5 drones bases, gain max reputation in 1 inquisition base, gain max
  reputation in 3 inquisition bases, gain max reputation in 5 inquisition
  bases, visit 1 undead base, visit 3 undead bases and visit 5 undead bases
- Info about docking cost before dock to a base
- Ability to escape from bases without paying
- Ability to set tools quality for crafting recipes
- Ability to set training tools quality for skills

### Changed
- Updated bases data with new items and recipes
- Updated help
- Updated debug interface
- Updated items and crafting recipes with reputation requirements
- Selling items which are unavailable due to reputation requirements in bases
  gives more reputation in bases
- Docking fee is taken during undocking from the bases
- Updated MODDING.md

### Fixed
- Possible crash on trading with a friendly trader (issue #67)
- Crash in shipyard when no module is selected for install or remove
- Charcollum to whatever transformation (issue #68)
- Editing crew members stats and skills in debug menu
- Missing icon and title for help window
- Size and position of help and debug windows

## [4.8] - 2020-03-22

### Added
- New items: iron 100mm explosive ammo, iron 120mm piercing ammo, iron 120mm
  explosive ammo, steel 20mm piercing ammo, steel 20mm explosive ammo, steel
  40mm piercing ammo, steel 40mm explosive ammo, steel 60mm piercing ammo,
  steel 60mm explosive ammo, steel 80mm piercing ammo, steel 80mm explosive
  ammo, steel 100mm piercing ammo, steel 100mm explosive ammo, steel 120mm
  piercing ammo, steel 120mm explosive ammo, titanium 40mm piercing ammo,
  titanium 40mm explosive ammo, titanium 60mm piercing ammo, titanium 60mm
  explosive ammo, titanium 80mm piercing ammo, titanium 80mm explosive ammo,
  titanium 100mm piercing ammo, titanium 100mm explosive ammo, titanium 120mm
  piercing ammo, titanium 120mm explosive ammo, orichalcum 60mm piercing ammo,
  orichalcum 60mm explosive ammo and orichalcum 80mm piercing ammo
- New crafting recipes: iron 100mm explosive ammo, iron 120mm piercing ammo,
  iron 120mm explosive ammo, steel 20mm piercing ammo, steel 20mm explosive
  ammo, steel 40mm piercing ammo, steel 40mm explosive ammo, steel 60mm
  piercing ammo, steel 60mm explosive ammo, steel 80mm piercing ammo, steel
  80mm explosive ammo, steel 100mm piercing ammo, steel 100mm explosive ammo,
  steel 120mm piercing ammo, steel 120mm explosive ammo, titanium 40mm
  piercing ammo, titanium 40mm explosive ammo, titanium 60mm piercing ammo,
  titanium 60mm explosive ammo, titanium 80mm piercing ammo, titanium 80mm
  explosive ammo, titanium 100mm piercing ammo, titanium 100mm explosive ammo,
  titanium 120mm piercing ammo, titanium 120mm explosive ammo, orichalcum 60mm
  piercing ammo, orichalcum 60mm explosive ammo and orichalcum 80mm piercing
  ammo
- Reputation in bases affects available modules in shipyards, items to
  trade and crafting recipes to buy

### Changed
- Updated bases data with new items and recipes
- Fire rate of enemies ships: Disarmers shoot less often than others
- Updated MODDING.md
- Updated modules data
- Updated help
- Hide assign ammo button when no ammo is available
- Hide move item option in inventory when crew member don't have any items
  (issue #59)
- Updated help to clarify how 'Visit bases' type of goals works (issue #65)

### Fixed
- Crash when Drone member is trying to eat
- Move map buttons default look
- Crash on upgrading when setting upgrading cost is very low
- Probably fixed possible crash in selling items in bases
- Crash on showing map with non existing event
- Crash when player don't have enough money for pay crew members
- Possible crash on start ship to ship combat
- Crash when healing last crew member in bases
- Crash when trying to talk in bases with someone with Legendary Rhetoric
  skill (issue #60)
- Probably fixed sudden docking after combat (issue #62)
- Setting icon for error dialog
- Crash when trying to sell medicines to diseased base which don't have
  enough money (issue #63)
- Crash in the game statistics when player visits more than 100 bases
  (issue #65)
- Crash on trying install module after selling another (issue #66)

## [4.7] - 2020-02-23

### Added
- Show current ship speed when option "Show numeric values" is enabled
- Info about speed difference between player and enemy ships in combat
- Types of ammunition: Normal, piercing, explosive
- Guns and harpoon guns have size now and require proper size of turret to be
  installed
- New ship modules: huge bronze turret, huge iron turret, huge steel turret,
  huge titanium turret, huge orichalcum turret and huge adamantium turret
- New items: iron 10mm piercing ammo, iron 20mm piercing ammo, iron 20mm
  explosive ammo, iron 40mm piercing ammo, iron 40mm explosive ammo, iron
  60mm piercing ammo, iron 60mm explosive ammo, iron 80mm piercing ammo,
  iron 80mm explosive ammo, iron 100mm piercing ammo
- New crafting recipes: iron 10mm piercing ammo, iron 20mm piercing ammo, iron
  20mm explosive ammo, iron 40mm piercing ammo, iron 40mm explosive ammo, iron
  60mm piercing ammo, iron 60mm explosive ammo, iron 80mm piercing ammo, iron
  80mm explosive ammo, iron 100mm piercing ammo
- Fire rate for ships guns

### Changed
- Updated mobs data
- Updated some ships crews
- Updated MODDING.md
- Updated help
- Renamed Bronze turret, Iron turret, Steel turret and Titanium turret to
  Medium bronze turret, Medium iron turret, Medium steel turret and Medium
  Titanium turret
- Renamed Heavy turrets to Large turrets
- Updated factions data
- Size of turrets and guns
- Updated bases data with new items and recipes

### Fixed
- Crash on generating recruits (issue #46)
- Grammar and sentence structure by ChrisEdBurt (pull request #52)
- Upgrading player's ship hull (issue #51)
- Crash on missing tools in crafting (issue #51)
- Unable to replace hull (issue #53)
- Removing turret crashes game  (issue #55)
- Additional grammar fixes (pull request #56)
- Possible crash when save directory is not writeable
- Fix misspellings and grammar errors. (pull request #58)

## [4.6] - 2020-01-26

### Added
- Ability to set random equipment for mobs
- Ability to set more generic weapon skill for mobs
- Better search for recipes, wounded crew members and modules to repair in
  bases
- Info about used equipment by both crews members during boarding
- New crafting order - deconstruct - allow recover materials from items

### Changed
- Updated mobs data
- When updating base in debug menu, show only available for selected faction
  bases types
- Updated some ships crews
- Updated MODDING.md
- Updated contributing guide
- Updated help
- Desconstruct crafting order name to Study

### Fixed
- Orders priority for one mob
- Possible generate that same name for savegame as old name
- Deleting events in debug menu
- Crash on updating base in debug menu
- Setting base type in debug menu
- Possible crash after combat
- Showing info about finished Studying order
- Showing info about lack of materials for crafting orders
- Showing info about needed workplace

## [4.5] - 2019-12-29

### Added
- New items: huge book of blacksmithing, medium book of woodworking, large
  book of woodworking, huge book of woodworking, medium book of
  leatherworking, large book of leatherworking, huge book of leatherworking,
  bronze training dummy, iron training dummy, steel training dummy, titanium
  training dummy, orichalcum training dummy, adamantium training dummy, bronze
  small traps, iron small traps, steel small traps, titanium small traps,
  orichalcum small traps, adamantium small traps, medium book of printing,
  large book of printing and huge book of printing
- New crafting recipes: huge book of blacksmithing, medium book of
  woodworking, large book of woodworking, huge book of woodworking, medium
  book of leatherworking, large book of leatherworking, huge book of
  leatherworking, bronze training dummy, iron trainning dummy, steel training
  dummy, titanium training dummy, orichalcum training dummy, adamantium
  training dummy, bronze small traps, iron small traps, steel small traps,
  titanium small traps, orichalcum small traps, adamantium small traps, medium
  book of printing, large book of printing and huge book of printing
- Better check for correctness of bases types data
- Orders menu to bases screens (issue #49)
- Button "Buy max" in trading (issue #49)
- Setting for favorite weapon for factions

### Changed
- Reduced chance to damage for Large book of metalsmithing
- Updated Industrial and Factory bases types with new recipes and items
- Updated contributing guide
- Don't show buttons buy/sell all when unneeded (issue #49)
- Reduced chance for random max skills levels for recruits
- Amount of recruit's skills depends on player reputation in base
- Updated help
- Updated factions with favorite weapon skill
- Updated MODDING.md
- Recruits in bases have weapons of own factions
- Recruits equipment quality depends on player reputation in base
- Reduced attributes of recruits in bases
- Updated README.md

### Fixed
- Info about setting difficulty level of upgrades costs
- Adding non existing ships to the game statistics
- Crash on finishing missions in bases
- Hang on starting a new game
- Removed outdated tooltip for recruits list in bases

## [4.4] - 2019-12-01

### Added
- New items: Adamantium small toys, medium book of alchemy, large book of
  alchemy, huge book of alchemy, medium book of cooking, large book of
  cooking, huge book of cooking, medium book of gunsmithing, large book of
  gunsmithing, huge book of gunsmithing, medium book of metalsmithing, large
  book of metalsmithing, huge book of metalsmithing, medium book of medicine,
  large book of medicine, huge book of medicine, medium book of farming, large
  book of farming, huge book of farming, medium book of woodcutting, large
  book of woodcutting, huge book of woodcutting, medium book of brewing, large
  book of brewing, huge book of brewing, medium book of blacksmithing and
  large book of blacksmithing
- New crafting recipes: Adamantium small toys, medium book of alchemy, large
  book of alchemy, huge book of alchemy, medium book of cooking, large book of
  cooking, huge book of cooking, medium book of gunsmithing, large book of
  gunsmithing, huge book of gunsmithing, medium book of metalsmithing, large
  book of metalsmithing, huge book of metalsmithing, medium book of medicine,
  large book of medicine, huge book of medicine, medium book of farming, large
  book of farming, huge book of farming, medium book of woodcutting, large
  book of woodcutting, huge book of woodcutting, medium book of brewing, large
  book of brewing, huge book of brewing, medium book of blacksmithing and
  large book of blacksmithing
- Enemy escape from the combat after some time
- Added type, size, material info to modules list in shipyard when installing
  new modules or remove old ones (issue #49)
- Presetted the game difficulty levels (issue #49)
- Travelling with the mouse (issue #49)

### Changed
- Reduced base price of orichalcum small toys
- Reduced price of orichalcum small toys in Industrial and Factory bases
- Updated Industrial and Factory bases types with new recipes and items
- Updated help
- Updated MODDING.md
- Updated README.md
- Updated contributing guide
- Reduced influence of items prices on recruits prices
- Better info about needed workshop in crafting
- Exit from any station screen, lead to station menu (issue #49)
- Don't reset category/search term when buying or selling (issue #49)

### Fixed
- Crash during hiring new crew members in bases (issue #49)
- Crash when trying to sell medicines to diseased base with full cargo (issue
  #49)
- Hide orders menu when moving ship (issue #49)
- Not working Close button in combat when you enter any other info screen
  (issue #49)
- Crash on map after selling last engine
- Not working "Auto center map after set destination" configuration option

## [4.3] - 2019-11-03

### Added
- Factory base type to the Drones faction
- New base types: Market, Barracks and Agora
- Market base type to the Independent faction
- New flag for bases types: barracks
- New items: bronze cockpit simulator, iron cockpit simulator, steel cockpit
  simulator, titanium cockpit simulator, orichalcum cockpit simulator,
  adamantium cockpit simulator, medium book of engineering, large book of
  engineering, huge book of engineering, bronze gun simulator, iron gun
  simulator, steel gun simulator, titanium gun simulator, orichalcum gun
  simulator, adamantium gun simulator, medium book of rhetoric, large book of
  rhetoric, huge book of rhetoric, bronze small toys, iron small toys, steel
  small toys, titanium small toys and orichalcum small toys
- New crafting recipes: bronze cockpit simulator, iron cockpit simulator,
  steel cockpit simulator, titanium cockpit simulator, orichalcum cockpit
  simulator, adamantium cockpit simulator, medium book of engineering, large
  book of engineering, huge book of engineering, bronze gun simulator, iron
  gun simulator, steel gun simulator, titanium gun simulator, orichalcum gun
  simulator, adamantium gun simulator, medium book of rhetoric, large book of
  rhetoric, huge book of rhetoric, bronze small toys, iron small toys, steel
  small toys, titanium small toys and orichalcum small toys
- Barracks base type to the Undead faction
- Agora base type to the Poleis faction

### Changed
- Pirates, Inquisition, Poleis and Independent factions ships
- Factory base type description
- Updated help
- Updated MODDING.md
- Updated Industrial and Factory bases types with new recipes
- Updated README.md to avoid confusion with `run.sh` script (issue #45)
- Updated README.md
- Updated contributing guide
- Updated factions description with information about unique bases

### Removed
- Industrial and Shipyard bases types from the Drones faction

### Fixed
- Crash when viewing available recruits in bases
- Possible crash during generating new missions in bases
- Crash during crafting (issue #46)
- Showing all bases when Undead faction is selected as default
- Crash on showing battering ram info in Shipyard (issue #47)
- Inventory window doesn't show properly, most of the interface is not drawn,
  flickering on and off (issue #48)

## [4.2] - 2019-10-06

### Added
- New ship modules: huge orichalcum workshop, huge adamantium workshop, medium
  bronze greenhouse, medium iron greenhouse, medium steel greenhouse, medium
  titanium greenhouse, medium orichalcum greenhouse, medium adamantium
  greenhouse, large bronze greenhouse, large iron greenhouse, large steel
  greenhouse, large titanium greenhouse, large orichalcum greenhouse, large
  adamantium greenhouse, huge bronze greenhouse, huge iron greenhouse, huge
  steel greenhouse, huge titanium greenhouse, huge orichalcum greenhouse, huge
  adamantium greenhouse, medium bronze training room, medium iron training
  room, medium steel training room, medium titanium training room, medium
  orichalcum training room, medium adamantium training room, large bronze
  training room, large iron training room, large steel training room, large
  titanium training room, large orichalcum training room, large adamantium
  training room, huge bronze training room, huge iron training room, huge
  steel training room, huge titanium training room, huge orichalcum training
  room, huge adamantium training room, medium bronze medical room, medium
  iron medical room, medium steel medical room, medium titanium medical
  room, medium orichalcum medical room, medium adamantium medical room,
  large bronze medical room, large iron medical room, large steel medical
  room, large titanium medical room, large orichalcum medical room, large
  adamantium medical room, huge bronze medical room, huge iron medical
  room, huge steel medical room, huge titanium medical room, huge orichalcum
  medical room and huge adamantium medical room
- Option to set which bases types are available for each faction
- Option to set chance for spawn for each base type for each faction
- New flags for bases types: temple and blackmarket
- New base types: Temple, Black market and Factory
- Added temple base type to the Inquisition faction
- Added flags shipyard, temple and blackmarket to the help
- Added black market base type to the Pirates faction

### Changed
- Updated MODDING.md
- Changed prices for items in bases on one price in data files
- Updated items, recipes and factions data
- Updated README.md
- Updated contributing guide
- Updated help

### Removed
- Info about which base type sells recipes from recipes data file

### Fixed
- No base type selected in new game setting on first run
- Crash on editing not visited base in debug menu
- Color of agricultural bases on the map
- Parsing factions names for selected flag in help

## [4.1] - 2019-09-08

### Added
- New ship modules: huge steel alchemy lab, huge titanium alchemy lab, huge
  orichalcum alchemy lab, huge adamantium alchemy lab, medium bronze furnace,
  medium iron furnace, medium steel furnace, medium titanium furnace, medium
  orichalcum furnace, medium adamantium furnace, large bronze furnace, large
  iron furnace, large steel furnace, large titanium furnace, large orichalcum
  furnace, large adamantium furnace, huge bronze furnace, huge iron furnace,
  huge steel furnace, huge titanium furnace, huge orichalcum furnace, huge
  adamantium furnace, medium bronze water collector, medium iron water
  collector, medium steel water collector, medium titanium water collector,
  medium orichalcum water collector, medium adamantium water collector, large
  bronze water collector, large iron water collector, large steel water
  collector, large titanium water collector, large orichalcum water collector,
  large adamantium water collector, huge bronze water collector, huge iron
  water collector, huge steel water collector, huge titanium water collector,
  huge orichalcum water collector, huge adamantium water collector, medium
  bronze workshop, medium iron workshop, medium steel workshop, medium
  titanium workshop, medium orichalcum workshop, medium adamantium workshop,
  large bronze workshop, large iron workshop, large steel workshop, large
  titanium workshop, large orichalcum workshop, large adamantium workshop,
  huge bronze workshop, huge iron workshop, huge steel workshop and huge
  titanium workshop
- More random cargo to the enemies ships in combat

### Changed
- Updated README.md
- Updated contributing guide
- Added cargo to traders ships so they will be slower in combat
- Updated interface
- Moved bases types to separated data file

### Fixed
- Crash on returning mission with max reputation as reward
- Patrol missions should be generated only on visited map cells
- Lack of fonts on Windows
- Possible crash on generating missions
- Moved cheapest harpoons buying option to Military bases instead Industrial
