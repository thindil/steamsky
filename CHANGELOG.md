# Change Log
All notable changes to this project will be documented in this file.

## [Unreleased]

### Added
- New type of ship modules - furnaces
- New skill: metalsmith
- New ship module: Small bronze furnace

### Changed
- Each crew member have separate work timer
- Updated help
- Updated interface

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
