# Change Log
All notable changes to this project will be documented in this file.

## [Unreleased]

### Added
- New enemy ship
- Support for enemy armor in combat
- Types of messages (combat/crafting/etc)

### Changed
- Renamed old enemies from small to tiny
- Saving messages to savegame file which made saves from 0.2 incompatible

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
