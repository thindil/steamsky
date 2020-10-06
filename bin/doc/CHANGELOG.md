# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

### Changed
- Updated interface
- Updated README.md

## [5.3] - 2020-10-04

### Added
- Ability to cancel crafting orders in the selected module
- Crew members lose morale if they don't get their part of trade profit
- Ability to show the home base from ship info

### Changed
- Updated interface
- Better checking for tools for crafting and training
- Better checking for tools during giving orders
- When tired crew member going on rest, returns tools
- When giving orders always take tools from crew members
- Updated help
- Updated README.md
- Moved crew info to ship info screen

### Fixed
- Code documentation
- Showing the player ship's crew info
- Crash on showing the player ship cabin's info when upgrading it
- Crash on showing the player ship crew info
- Order of columns in inventory info
- Showing messages list
- Crash on giving orders to the crew members
- Background color for debug menu
- Showing repair/clean all buttons
- Showing repair/clean orders for each crew member
- Showing info about assigned skill to training room
- Positioning of orders menu
- Showing clean all button when not needed
- Updated assign crew member list when crew member go to another work
- Showing list of items after selling or buying items
- Showing color messages on messages list
- Auto-resizing of last messages widget
- Update crew info in ship info on assign or unassign crew member to module
- Showing module damamge in module info
- Showing main menu when the player tries load invalid savegame
- Crew members don't gain experience in begining of the game
- Don't show crafting order when any workshop don't have set it
- Can't find fonts on Linux
- Crash on non-existing the game theme
- Redraw map after changed size of map font
- Selecting save game from list, load it
- Showing main menu after deleting last savegame
- Crash on trying to show destination menu on start the game
- Resizing header after again start the game

## [5.2] - 2020-09-06

### Added
- Ability to reset the player ship destination in ship info screen
- Ability to assign a crew member to training room in ship info screen

### Changed
- Set General career as default for each faction
- Updated MODDING.md
- Updated README.md
- Updated interface
- Updated help

### Removed
- Checkboxes images setting from the UI themes

### Fixed
- Setting the game difficulty level from the game setting
- Reading the game version from the game setting
- Don't check fuel amount during trading
- Don't show last messages on messages screen
- Crash on ship removing module in shipyard
- Crash on showing crew information
- Resetting module type to show after reentering shipyard
- Checking for crafting location for study and deconstruct orders
- Can't sell items when one of crew members has set percent from profit as a
  payment

## [5.1] - 2020-08-09

### Changed
- Updated README.md
- Updated MODDING.md
- Rewritten to use Tk library instead of GTK

### Fixed
- Crash during resting when crew member have very high tired level
- Showing escape option from empty bases
- Taking dock payment in empty bases
