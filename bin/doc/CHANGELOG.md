# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

### Added
- Recipe name to the recipe info dialog's header

### Changed
- Redesigned the main in-game, load game, known event, item in cargo, crafting
  recipes, recruits, item in crew inventory, mission in base, ship's module,
  known base, accepted mission, trading item, crew member, buying recipes,
  heal wounded, repair ship in base menus

### Fixed
- Showing the proper event on the map after selecting it from the list
- Selecting available mission in bases after sorting their list

## [6.7] - 2021-10-31

### Added
- Close entry to the main game menu (GitHub #85)
- Skill level info to schools in bases
- Info about needed skill to the list of crew members when assigning them
  to workshops
- Options to reset the game keyboard shortcuts to default (GitHub #87)

### Changed
- Remove skill from skills list in school after training it to the max
- Updated look of the player's ship's crew members info dialog
- Updated help
- Default keyboard shortcuts on Linux to work without enabled Num Lock
- Default keyboard shortcut to click mouse on the map
- Redesigned destination and orders menus

### Fixed
- Crash Upon Transporting Drone Passenger GitHub #81
- Counting finished passenger missions as success and failure together
- Can't accept passenger missions when multiowner cabin has one free space
- Crash when trying to train maximized skill in bases
- Numpad Keys Dysfunctional GitHub #84 on Windows
- Wrong order of movement keys in the game options
- Keybinds Unavailable In Menu GitHub #85
- Showing attributes level in bar when attribute is very low
- Crash during opening help window when other exists
- Showing skills names during training crew members in bases
- Not blocking menu keyboard shortcuts when searching for items in various
  places
- Keybind Issues - Half-resolved GitHub #87, wrong detection of capital
  letters
- Default keyboard shortcut for center map on home base
- Keybind Issues - Half-resolved GitHub #87 (not removing invalid shortcuts)
- Reset the current options tab on reenter the game options
- Mission Auto-complete Fails to Activate GitHub #91 (lack of info when
  missions will not auto finish)
- Scrolling: Extremely Tedious GitHub #80 (scrolling with the mouse doesn't
  work on Windows)
- Show workshops for study or deconstructing orders when more than one
  alchemy lab is installed

## [6.6] - 2021-10-03

### Added
- Ability to sort the items on the lists in the game statistics screen
- Ability to traverse by various lists in the game with keyboard
- Ability to set the amount of items on various lists (like crew members,
  modules, etc.)

### Changed
- Updated modding guide
- Updated contributing guide
- Updated README.md

### Fixed
- Pressing space key after starting a new game starts it again
- Pressing space key after back to the map refreshing it
- Closing info about attribute, skill or item in inventory doesn't select
  Close button
- Error when using mousewheel on Windows GitHub #77
- Crash Upon Opening Skills GitHub #78 with long names of skills
- Crash Upon Approaching Enemy #79 when player's ship don't have guns
- Crash during accepting transport passenger missions in bases

## [6.5] - 2021-09-05

### Added
- Ability to sort the items during trading and looting bases, modules
  in bases shipyards, recruits in bases, crafting orders, buying recipes,
  healing wounded crew members, repair ship, available missions in bases

### Changed
- Updated debug interface
- Updated README.md
- Updated modding guide

### Fixed
- Showing tools information in crafting recipe info
- Crash when entering non-digit into amount entry in various locations
- Crash when setting max allowed values for center the map
- Sorting the list of accepted missions by time and reward

## [6.4] - 2021-08-08

### Added
- Ability to move (drag) dialogs with mouse after clicking on their headers
- Ability to sort saved games, the player's ship's modules, crew, cargo, the
  crew members inventories, known bases, known events and accepted missions
  lists

### Changed
- Updated help
- Updated interface
- Updated contributing guide
- Updated debug interface

### Fixed
- Showing the list of available missions in bases after accepting one
- Some typos in changelog
- Crash when trying to enter help from crafting screen
- Some typos in help
- Crash on non-existing help topics
- Crash when changing the name of the selected crew member
- Crash when changing the name of the selected ship module
- Showing info about taken cabin in the available in base missions list
- Showing info about overloaded ship
- Reset scrollbar when showing the list of known bases
- Showing bases types in debug window

## [6.3] - 2021-07-11

### Added
- Info about current gain or cost when selling or buying items
- Info about items weight to the trade screen
- Info about all characters' attributes and skills to help

### Changed
- Updated interface
- Move mouse cursor on map a bit faster
- Redesigned school, load game, assign skills and list of available missions
  in bases screens
- Updated help
- Made scrollbars more visible
- Updated the game default UI theme
- Use word *attributes* everywhere when referencing to characters' statistics

### Fixed
- Added missing headers to dialogs with information about modules to install or
  remove in bases shipyards
- Restoring position on last messages window after closing the game options
  screen
- Default keyboard shortcuts for moving map
- Moving cursor on the map with keyboard
- Scrolling the last messages window to the bottom when there are long messages
- Showing long goals names in the game statistics screen
- Crash after recruit a new crew member in bases
- Reset the selected modules type in shipyard after buying a module
- Showing items with zero amount in trading screen
- Typos in help
- Showing header for the goals' selection dialog when scrollbar is visible
- Help window blocks the main game window
- Crash when trying to preview recruit's attributes
- Typo in description of barley seeds
- Setting reward for missions in bases

## [6.2] - 2021-06-13

### Added
- Headers to dialog windows in the game

### Changed
- Updated interface
- Updated help
- Default keyboard shortcuts for change the player's ship speed
- Redesigned crafting, buying recipes, heal wounded and buying repairs screens
- Updated README.md

### Fixed
- Removed frame around map after clicking on it
- Typo in asking for rest when crew members are tired
- Typos and punctuation in help
- Typos in info about experience needed for the next level of skills and
  statistics
- Typo in Strength attribute description
- Losing focus by text fields when setting keyboard shortcuts
- Can't assign 1-5 keys to shortcuts
- Giving crew members items from the ship's cargo
- Showing error messages in main menu blocks error message
- Crafting info window don't show all information
- Showing info about training room

## [6.1] - 2021-05-16

### Added
- Option to set which mouse button use for showing in game menus
- Ability to set own fonts in the game themes
- Moving in game screens with the mouse wheel

### Changed
- Updated README.md
- Installer shouldn't longer require Administrator's rights on Windows
- Redesigned the game options screen
- Updated interface

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
- Showing crafting recipe as uncraftable when materials are present
- Hiding close button after setting base, event, accepted or available mission,
  story as the player's ship's destination or showing it on map
- Hiding guns orders in combat when nobody is assigned to them
- Crash on setting new crew member to empty position in combat
- Crash on setting new crew member to empty gun in combat
- Setting gunner in combat
