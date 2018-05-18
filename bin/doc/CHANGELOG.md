# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

### Added
- New items: titanium woodworker set, orichalcum woodworker set, titanium
  repair tools, orichalcum repair tools, titanium sewing kit, orichalcum
  sewing kit, orichalcum 60mm ammo, orichalcum 80mm ammo, orichalcum 100mm
  ammo, orichalcum 120mm ammo, orichalcum harpoon, orichalcum short sword and
  orichalcum sword
- New crafting recipes: titanium woodworker set, orichalcum woodworker set,
  titanium repair tools, orichalcum repair tools, titanium sewing kit,
  orichalcum sewing kit, orichalcum 60mm ammo, orichalcum 80mm ammo,
  orichalcum 100mm ammo, orichalcum 120mm ammo, orichalcum harpoon, orichalcum
  short sword and orichalcum short sword
- Showing license full text in info about game
- New sky base type: military
- Damage type for personal weapons
- Option to search through messages
- More info about player death in melee combat
- Option to set max amount of stored messages in game
- Option to set max amount of saved messages
- Statistics for killed enemies in melee combat which made saves from previous
  version incompatible
- New type of goal: kill X enemies in melee combat
- New goal: kill 800 enemies in melee combat

### Changed
- Updated interface
- Updated README.md
- Updated items data
- Updated MODDING.md
- Updated recipes data
- Updated help

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
- Checking for needed materals before start crafting
- Don't allow to go on rests crew members in boarding party when ships are not
  connected by harpoons
- Issue #17 - Crash when boarding party member don't have weapon
- Showing information about crew member order when he/she boarding enemy ship
- Probably fixed issue #18 - Crash when crew member have more than maximum health

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
