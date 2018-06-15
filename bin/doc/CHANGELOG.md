# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

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
  combat, kill 2000 undead in melee combat, kill 4000 undead in melee combat
  and kill 6000 undead in melee combat
- Option to set player faction
- Random equipment to recruits in bases
- Payment for crew members
- Negotiating hiring of recruits in bases

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
