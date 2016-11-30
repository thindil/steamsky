# Change Log
All notable changes to this project will be documented in this file.

## [Unreleased]

### Added
- Weapon damage, crafted amount of items and speed of rest depends on how much
  ship module is damaged
- New random events: bad weather, full docks in bases, attacks on bases and
  disease in bases
- Info about events on sky map
- List of know events
- New game logo
- Option to ask for events in bases
- Cargo to all ships
- Need and using ammo in combat by npc ships
- Player reputation in bases and it impact on some player actions
- More detailed informations about skills levels
- Check for minimal console size
- New item type: medicines
- New item: medical supplies
- More possible ammo for this same gun types
- Option to assign ammunition to selected gun
- Damage in combat depends on ammo too
- New items: steel 10mm ammo, steel 20mm ammo and steel 40mm ammo
- New crafting recipes: steel 10mm ammo, steel 20mm ammo and steel 40mm ammo
- New ship module: bronze turret
- New enemy ships: pirate ship, armored pirate ship

### Changed
- Updated help
- Most of random events have duration time, they are saved to savegame which
  made saves from version 0.5 incompatible
- Starting order for gunner from On break to Operate gun
- Don't change ship speed in fight when no engineer on duty
- When change ship speed outside combat, check only for engine and engineer
- Updated interface
- Read game changes in News screen from CHANGELOG.md
- Ammo name from size ammo to Iron size ammo
- Updated enemies selection

### Fixed
- Don't craft items when worker is on break
- Crash in bases list on small screens
- Item type for steel plates
- Crash when terminal not support changing colors

## [0.5] - 2016-11-13

### Added
- New type of ship modules - furnaces
- New skills: metalsmith and perception
- New ship modules: Small bronze furnace, Advanced medium bronze hull, Large
  bronze hull, Advanced large bronze hull, Bronze armor, Heavy bronze armor and
  Iron 40mm gun
- Minimum fuel usage during fly
- New types of items: copper, tin, copperore, tinore, ironore and steel
- New items: copper plates, tin plates, copper ore, tin ore, iron ore, 40mm
  ammo and steel plates
- New crafting recipes: copper plates, tin plates, bronze plates, iron plates,
  40mm ammo and steel plates
- Option to rename player ship
- Impact of hunger/thirst/wounds on crew members skills
- Option to assign crew members to selected module
- Option to assign crew members to trade with bases
- Discovering bases
- Time passing to recruit new crew members in bases
- Option to move sky map to selected coordinates
- Show informations about selected map cell
- List of known bases
- Option to set travel destination and auto travel to it
- Ability to be auto attacked by enemy ship
- Option to stop current module update
- Restore ship speed after fight
- Gaining experience in fight for pilot and engineer

### Changed
- Each crew member have separate work timer which made saves from 0.4
  incompatible
- Each crafting module have own separate crafting order
- Updated help
- Updated interface
- Price of bronze plates
- Name of ship module Bronze armor to Light bronze armor
- Always start new game in large base
- Skill name from Bartering to Rhetoric
- Starting order for player from On break to Talking in bases
- Moved items types to separated text file
- Starting combat with other ships
- Item type ammo on ammo10, ammo20, ammo40
- Updated in-game news

### Fixed
- Crash on updating modules
- Crash on check for new recruits in bases
- Clearing module owner on change crew member order
- Time needed for rest in wait command
- Check for random events if there was no ship move
- Gaining experience for trading
- Ammo for Iron 10mm gun
- Messed screen on deleting messages
- Info about lack of crew on selected position
- Gaining experience for gunner for last round of combat
- Crash on assign crew members to positions during combat
- Assign new pilot when older is on break
- Crash on repair ship modules

## [0.4] - 2016-10-16

### Added
- Auto-resize game screen on resize console
- Population to bases which made saves from 0.3 incompatible
- Option to repair ship in bases
- New type of bases - shipyards
- Separated repair skill for each module
- Maximum amount of messages - 500
- Option to delete all messages
- Upgrading ship modules
- Cabin quality to speed of rest
- New item to game: 10mm ammo
- New crafting recipe: 10mm ammo
- Installing/removing modules from ship
- Fuel usage during movement depends on amount of engines installed on ship
- New ship modules: small advanced bronze engine, medium bronze engine, small
  advanced bronze hull and medium bronze hull, medium advanced bronze engine,
  large bronze engine, large advanced bronze engine
- Info about unique ship modules
- Recruitment new crew members in bases
- Dismissing crew members
- New enemy ship - small clockwork drone
- Support for many weapons for ships in combat
- Option to install battering ram on player ship
- Attack with battering ram by player
- Ship modules size

### Changed
- Updated help
- Updated interface
- Showing more messages on combat screen
- Add only one crafting message with summary of manufacturing
- Names of some ship modules
- Moved skills names to separated text file
- Fuel usage during fly depends on engine
- Enemy attack range depends on weapon type not on ship
- Limit player ship attacks distance
- Amount of enemies attacks depends on weapon type
- Updated in-game news

### Fixed
- Crash on invalid data in save game file
- Adding destroyed cargo bay to free cargo space
- Crash in combat on small screens
- Coloring missing materials on recipes screen
- Showing negative free cargo space
- Some memory leaks
- Crash on updating cargo during manufacturing
- Crash on show info about empty cabin
- Crash on show info about empty turret
- Informations about crew members skills levels

## [0.3] - 2016-09-18

### Added
- New enemy ships - small pirate ship and small undead ship
- Support for enemy armor in combat
- Types of messages (combat/crafting/etc.)
- Option to sort messages by types on messages screen
- New items to game: illandru logs, fine rations, gunpowder, iron plates
- New types of items: gunpowder, iron
- New crafting recipes: 20mm ammo and fine rations
- New skills: cooking and gunsmith
- Some iron to starting cargo
- Info which ships are enemies
- Very simple enemy AI in combat
- Option to rename player ship modules
- Option to drop selected item from ship cargo
- Show information about ship/cargo/crew on combat screen
- Better handling crew members deaths
- Owners for ship modules
- Assign ship guns to turrets
- Destruction of guns after destroying turrets
- Killing crew member when selected ship module is destroyed
- Evasion bonus for enemies
- Check for modules when giving orders to crew members
- Crew member will search for empty cabin when going on break
- Pilot skill to evade chance in combat
- Separated loot amount for each enemy

### Changed
- Renamed old enemies from small to tiny
- Saving new messages format to savegame file which made saves from 0.2 incompatible
- Show last messages as first on last messages screen
- Item type FuelMaterial on Wood and RepairMaterial on Bronze
- Crafting recipes now can have more than one material needed to craft
- All ship modules have own repair materials
- Each crafting recipe use separated skill
- Updated interface
- Split help on topics
- Updated help
- Raised ships speed
- Counting ships speed on sky map
- Raised durability of cockpit and turret
- Raised amount of loot from combat
- Speed of rest depends on did crew member have cabin
- Don't recover health if crew member don't have cabin
- Name of ship module from cargo space to cargo bay
- Bonus to hit/evade in combat depends on ships real speed not on engineer orders
- Removed need for dock to base to discover its type
- Moved syllables for bases and crew names to separated text file
- Better bases names generator
- Updated in-game news

### Fixed
- On combat screen showing messages from previous combat
- Moving ship with arrows keys
- Starting random events on map moving or centering map
- Giving orders to gunner during combat
- Healing dead crew members
- Information about no weapon on ship in combat
- Crash on giving manufacturing order when ship manufacturing order isn't set

## [0.2] - 2016-08-21

### Added
- Wait 1 game minute command
- Fatigue, hunger, thirst, health to crew members
- Show last message on screen
- Informations why action failed (trade/ship moving/orders)
- Check for free cargo space during trade items
- End game on player death
- Confirmation dialog for quit to main menu
- New game window with ability to set player and ship names and player gender
- Version to savegame file which made saves from 0.1 incompatible
- Window with informations about current location
- Gaining experience in skill by crew members
- Pilot/Engineer skills influence on flying time
- Player Bartering skill influence on item prices in bases
- Random names for bases and crew members
- Armor module for ships
- Combat between ships
- Random event during fly (combat)
- Items manufacturing
- New items: bronze plates, fresh fruits, fresh vegetables, fresh meat, andrae logs
- Save to file last 10 messages
- Ability to move map without moving ship
- Informations about license to main menu
- Menu with commands to wait certain amount of time
- Informations about in game changes to main menu
- Random gender to crew members
- New ship weapon type - battering ram

### Changed
- Updated README.md
- Updated in-game menu
- Moved ship orders to game menu
- Updated help
- Updated interface
- Player and gunner starts game with on break command
- Show ship name instead speed in ship info screen
- Moved more items informations to prototypes
- Raised starting skills levels
- Moved help text to separated text file
- Raised price of basic rations
- Moved items data to separated text file
- Better world generation
- Moved ship modules data to separated text file
- Moved ships data to separated text file
- Moved ship cargo informations to separate screen

### Fixed
- Crash in save game when game was started from other directory
- Giving orders to dead/starving/dehydrated/too tired crew members
- Crash on too long list of messages to show

### Removed
- Duty order for player

## [0.1] - 2016-07-24
Initial release
