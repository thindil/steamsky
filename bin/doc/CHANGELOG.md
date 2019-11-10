# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

### Added
- New items: adamantium small toys, medium book of alchemy, large book of
  alchemy, huge book of alchemy, medium book of cooking, large book of cooking
- New crafting recipes: adamantium small toys, medium book of alchemy, large
  book of alchemy, huge book of alchemy, medium book of cooking, large book of
  cooking

### Changed
- Reduced base price of orichalcum small toys
- Reduced price of orichalcum small toys in Industrial and Factory bases
- Updated Industrial and Factory bases types with new recipes

### Fixed
- Crash during hiring new crew members in bases (issue #49)
- Crash when trying to sell medicines to diseased base with full cargo (issue
  #49)
- Hide orders menu when moving ship (issue #49)
- Not working Close button in combat when you enter any other info screen
  (issue #49)
- Crash on map after selling last engine

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
