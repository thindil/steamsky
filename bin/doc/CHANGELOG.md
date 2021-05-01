# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

### Changed
- Updated README.md

### Fixed
- Showing crafting recipe as uncraftable when materials are present
- Hiding close button after setting base/event/mission/story as the player's
  ship's destination or showing it on map

## [6.0.1] - 2021-04-25

### Changed
- Installer shouldn't longer require Administrator's rights on Windows
- Updated README.md

### Fixed
- Look of tooltips in the game options
- Crash during upgrading modules when upgrading material is the last in the
  player ship cargo
- Crash during repair ship when repair material is the last in the player
  ship cargo
- Checking permissions to save game in the selected directory
- Scrollbar in setting boarding party in combat
- Autohide scrollbars in the game options
- Showing last messages after back to the map

## [6.0] - 2021-04-18

### Added
- Pagination to lists of known bases and events, accepted missions, shipyard,
  trade, player's ship cargo, crew and modules, available recruits in bases
  and available items to loot from bases
- Missing info about free space in player ship cargo
- Missing info about free space in crew members inventory

### Changed
- Don't create patrol and explore missions on fields with bases
- Updated interface
- Redesigned crew member's inventory screen
- Updated README.md

### Fixed
- Closing wait menu with Escape key returns to map
- Refresh ship info and knowledge screens after wait some time
- Resetting ugprade progress after finishing upgrading modules
- Crash on generating recruits in unfriendly bases
- Search on trade list doesn't work if item isn't in the ship cargo
- Crash on entering the knowledge screen again
- Possible crash when ending the game
- Clear search entry in trade when refreshing the items list
- Crash when player accepts the last mission in base
- Looking for items in trade when the item's type is selected
- Typos in trade screen
- Autoresizing of tables
- Remembering size of last messages window
- Generate bases at borders of the map
- Showing info about the highest skill of recruits in bases
- Showing list of known bases after reenter knowledge screen
- Reseting search field in install modules in shipyard screen
- Showing info about docking cost in abandoned bases
- Crash on lack of permission to write to directory selected for saves
- Showing the game version in main menu when showing error about the game data
- Crash on set invalid data directory
- Show console window on Windows
- Asking friendly ships for bases and events
- Lack of name of friendly ship in message when asking it for bases or events
- Color of the info about friendly trader in map info
- Starting and completing missions
- Progressing stories
- Delivering medicines to the diseaded bases

## [5.9] - 2021-03-21

### Added
- Missing option to remember combat orders (Github issue #74)

### Changed
- Updated interface
- Finished redesign of the ship info screen

### Fixed
- Hiding enemy ship's info after combat
- Crash on entering the ships combat again
- Crash when trying to load configuration file from older game versions
- Crash when trying to enter the game settings on Windows (Github issue #73)
- Crash on showing messages in main menu
- Showing the game statistics on resign from the game
- Position of Close button on the game statistics screen when resigning from
  the game
- Showing the game after reloading it (quit to main menu and load or start the
  game again)
- Crash on damaged engines during fly (Github issue #74)
- Crash after victory in combat (Github issue #75)
- Crash when player ship crew member die in melee combat
- Setting themes directory via command line arguments
- Hide next turn button after finishing boarding combat
- Crash on finishing deliver item missions
- Showing profit in trade screen
- Counting max sell amount in trade
- Showing sell option when selling is not possible
- Crash when can't accept mission in bases
- Showing info about the selected module when it is upgraded

## [5.8] - 2021-02-21

### Changed
- Redesigned installing and removing module screen in shipyard bases
- Redesigned looting bases screen
- Updated interface
- Updated contributing guide
- Updated README.md

### Fixed
- Showing the selected modules type in shipyard with the current search text
- Showing the selected type of items in loot screen
- Showing longer description for modules in shipyard
- Possible crash on accepting missions in bases
- Cost of the player's ship repairs in Industrial base
- Typos in knowledge screen
- Crash on showing the current player's goal

## [5.7] - 2021-01-24

### Changed
- Updated README.md
- Updated contributing guide
- Updated modding guide
- Updated interface
- Redesigned trade and hire recruits screen

### Fixed
- Showing profit info on trade screen
- Crash on trying to trade with friendly ships
- Crash on showing info about errors
- Updating messages after wait action
- Showing long goals names on button in the game statistics
- Looking for crafting recipes to buy in bases
- Button text for buying recipes or repairs in bases

## [5.6] - 2020-12-27

### Added
- Zooming in and out map with the mouse wheel
- Option to set defenders in combat screen

### Changed
- Updated interface
- Keyboard shortcut for the next turn in combat to Enter
- Updated descriptions for harpoon guns and battering rams
- Updated README.md

### Removed
- Setting for the animations

### Fixed
- Escape key behavior in inventory and crew member dialogs
- Position of messages window after leaving the last messages screen
- Some typos in changelog
- Position of the game statistics
- Showing player's ship damage in combat
- Showing the enemy's ship's modules info when it is destroyed
- Scrolling last messages window when older messages showed as first
- Font on help topics list
- Scrolling the crew member info, assigning crew member to module and crew
  members inventory info
- Possible crash when ending delivery mission (issue #71)
- Scrollbar position in crew member info dialog
- Ending combat turn with space key
- Showing the lists of gunners in combat
- Crash in combat when the player's ship doesn't have installed guns
- Crash on giving order to back to ship in boarding combat
- Setting crew members positions (pilot, engineer, gunner) in combat
- Crash on setting crew members orders priorities

## [5.5] - 2020-11-29

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
- Keyboard shortcuts on information screens in combat back to map

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
- Moved accepted missions list to knowledge screen
- Moved know stories list to knowledge screen

### Removed
- Settings to show bases and items info

### Fixed
- Showing keyboard shortcuts in help
- Crash on the new turn
- Crash during boarding when attacker or defender are too tired
- Crash on selling items when no enough profit to pay crew members
- Showing orders keyboard shortcut in Firsts Steps in help
- Crash in combat when enemy crew member has low morale
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
- Showing module damage in module info
- Showing main menu when the player tries load invalid savegame
- Crew members don't gain experience in beginning of the game
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
