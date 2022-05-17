# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

### Fixed
- Crash during entering boarding combat
- Crash when showing information about boarding combat related crew info in
  combat, ticket #303b13c5d3

## [7.4] - 2022-05-15

### Added
- Check if entered the player's ship, modules or crew members names are not
  too long
- Info about bigger hull's size on the list of modules to install in shipyards

### Changed
- Redesigned error reporting dialog
- Updated contributing guide
- Updated modding guide
- Updated help
- Better information about differences between modules during installating
  them in bases' shipyards

### Fixed
- Reset the map after quit and start a new game
- Crash when pressing keyboard shortcut for move to button
- Amount of installed modules should'nt be raised when installing a new armor,
  gun or harpoon gun
- Crash when the player don't have permissions to write to directory with
  saved games
- Crash when the player don't have permissions to read the game configuration
  file

## [7.3] - 2022-04-17

### Changed
- Updated README.md
- Icons for checkboxes, radioboxes: Author:  Delapouite (https://game-icons.net),
  license CC BY 3.0
- Updated interface
- Generate less destroy ship type missions with traders as targets
- Changed text on giving crafting orders to the crew members from workshop name
  (Work in Alchemy Lab) to crafting order (Manufacture Iron Plates)
- Updated modding guide

### Removed
- Font Awesome from the game, it is replaced by images

### Fixed
- Crash on entering the list of available missions in bases, ticket #56ddcf602a
- Crash when the name of saved game file has non standard name
- Crash when trying to show crew member menu in the ship info screen when there
  are more than one gun is installed, ticket #d9f2c62a7e
- Assigning crafting orders when more than one workshop with the same name is
  installed on the ship.
- Crash when trying to show crew member menu in the ship info screen when there
  are more than one workshop is installed
- Crash when trying to show crew member menu in the ship info screen when there
  are more than one training room is installed
- Crash when trying to show Orders menu when finishing delivery of item to a
  base mission, ticket #bf9614da07
- Crash when waiting for the player's ship's crew rest, ticket #b1947ebf50

## [7.2] - 2022-03-20

### Added
- New icons for moving map and player's ship left, right, up and left, up and
  right, down and right, down and left, wait 1 minute, move one step, move to,
  menus, close button, random button, male gender, female gender, edit, show,
  cancel, remove, help. Authors: Delapouite, Lorc, Skoll, Sbed
  (https://game-icons.net), license CC BY 3.0

### Changed
- Updated modding guide
- Icons for show map buttons, hide map buttons, move map buttons left, move map
  buttons right. Author: Delapouite (https://game-icons.net), license CC BY 3.0

## [7.1] - 2022-02-20

### Added
- New icons for lack of pilot, engineer, gunners, trader and cleaning, repair,
  upgrading the player's ship workers, crafting, fuel, food, drinks, overloaded
  ship, moving the map buttons, moving map and player's ship up, down. Authors:
  Delapouite, Lorc (https://game-icons.net), license CC BY 3.0
- Message about finished cleaning the player's ship

### Changed
- Updated README.md
- Moved information about the selected module in shipyards a bit higher
- Updated modding guide

### Fixed
- Setting fonts' sizes during start the game
- Updating the info about the player's ship's upgrade after setting one
- Updating the info about the player's ship's upgrade after canceling one
- Stopping cleaning the ship when one of the working crew members has tools
