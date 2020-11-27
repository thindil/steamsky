# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

### Changed
- Updated interface

### Fixed
- Showing horizontal scrollbar in known bases info
- Position of destination menu
- Showing base type when its name has more that one word
- Crash when default base type has more than one word in name
- Showing player when he/she is at left or bottom border of the map
- Showing story icon on the map when player don't have any active
- Showing scrollbars for stories on knowledge screen
- Don't show the move map dialog on other than the map screens
- Setting amount of saved messages in saved games
- Don't show the game menu when there is any dialog
- Crash on giving rest order to the crew members
- Setting and remembering size of messages window
- Wait order when docks in base are full
- Showing combat UI
- Typo in info about daily payments
- Game menu in combat
- Crash on show/hide the game help
- Close button in the game option returns to map instead of combat
- Menu shortcuts in combat

## [5.4] - 2020-11-01

### Changed
- Updated interface
- Updated README.md
- Updated help
- Updated MODDING.md
- Moved cargo info to ship info screen
- Disable menu keyboard shortcuts when entering the text
- Moved known bases list to knowledge screen
- Moved known events list to knowledge screen
- Moved accepted missions list to knowlegde screen
- Moved know stories list to knowledge screen

### Removed
- Settings to show bases and items info

### Fixed
- Showing keyboard shortcuts in help
- Crash on the new turn
- Crash during boarding when attacker or defender are too tired
- Crash on selling items when no enough profit to pay crew members
- Showing orders keyboard shortcut in Firsts Steps in help
- Crash in combat when enemy crew member has too low morale
- Info about bases symbols on map in help
- Look of setting menu shortcuts
- Resizing of fonts in help tags
- Auto-resizing of help window
- Saving the main game window size
- Unmaximize main menu window when quit the game
- Hide close button in combat
- Look of map info window
- Typos in README.md, CONTRIBUTING.md, MODDING.md and help
- Showing orders for the whole crew
- Showing info about the event on map
- Crash on map with starting a story

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
