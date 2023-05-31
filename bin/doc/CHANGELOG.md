# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

### Added
- Ability to assign the crew members to the player's ship's training rooms in
  the module info dialog
- Ability to assign the skill to train in the player's ship's training rooms in
  the module info dialog

### Changed
- Updated look of the module info dialog
- Show the info dialog with available actions instead of menu in the list
  of installed modules in the player's ship

### Fixed
- Typos in changelog
- Crash when browsing modules to install in shipyards
- Saved game doesn't load when training room on the player's ship doesn't
  have set a skill to train
- Recruits in bases change every time intead of every in-game week
- Crash when showing the player's ship's module's info dialog for a gun
  with assigned ammunition

## [8.7] - 2023-05-14

### Added
- Icon for assigning ammunition to guns. Author: sbed (https://game-icons.net),
  license CC BY 3.0
- Ability to assign the crew members to the player's ship's guns in the module
  info dialog
- Ability to assign the ammunition to the player's ship's guns in the module
  info dialog
- Ability to start the upgrade of the player's ship's battering ram's damage in
  the module info dialog
- Ability to start the upgrade of the player's ship's hull's size in the module
  info dialog
- Ability to assign the crew members to the player's ship's workshops in the
  module info dialog
- Ability to cancel the working order of the player's ship's workshops in the
  module info dialog
- Ability to assign the crew members to the player's ship's medical rooms in
  the module info dialog

### Changed
- Updated look of the module info dialog
- Updated modding guide

### Fixed
- Setting the names for cabins in newly created ships
- Not showing the list of assigned to the module crew members in the module
  info dialog

## [8.6] - 2023-04-16

### Added
- Icons for setting the upgrades for modules and enabling or disabling engines.
  Authors: Delapouite, Lord Berandas (https://game-icons.net),
  license CC BY 3.0
- Ability to start the upgrade of the player's ship's module's durability in
  the module info dialog
- Ability to start the upgrade of the player's ship's cabin's quality in the
  module info dialog
- Ability to cancel the upgrade of the player's ship's module in the module
  info dialog
- Ability to start the upgrade of the player's ship's engine's power in the
  module info dialog
- Ability to start the upgrade of the player's ship's engine's fuel usage in
  the module info dialog
- Ability to enable or disable the player's ship's engine's in the module info
  dialog
- Ability to assign the crew members to the player's ship's cabin's in the
  module info dialog
- Ability to start the upgrade of the player's ship's gun's damage in the
  module info dialog

### Changed
- Updated modding guide
- Updated look of the module info dialog

### Fixed
- Crash in the bases loot screen when the player's ship cargo is the same
  length as the table's page
- Crash when cleaning ship in open space
- Showing info about the module's upgrade progress in the module' info dialog
- The player's ship's module's info dialog size
- Gaining experience after loading the game

## [8.5] - 2023-03-19

### Added
- Column with coordinates to the lists of known bases and known events
- Ability to select crew members on the player's ship's crew members list
- Ability to give orders to the selected crew members on the player's ship's
  crew members list
- Ability to change a module's name in the module's info dialog
- Icon for setting the repair priority for modules. Author: Lorc
  (https://game-icons.net), license CC BY 3.0
- Ability to set the repair's priority in the module info dialog

### Changed
- Always show the status of the module in the module info dialog
- Updated look of the module info dialog
- Updated modding guide

### Fixed
- Showing too much columns in the buying recipes in bases menu
- Color of text for distance column in the list of known events
- Possible crash when trying to create a save game file
- Crash when starting the game with debug menu
- Deleting damaged items from a crew member's inventory
- Crash when shooting to enemies in ships combat
- Crash when the player's ship crew member gains level in a skill
- Showing empty categories of items in the trade screen
- Crash in the trade screen when the player's ship cargo is the same length
  as the table's page

## [8.4] - 2023-02-19

### Added
- Icon for sending all ship's crew member to rest. Author: Delapouite
  (https://game-icons.net), license CC BY 3.0
- Order Go Rest for the whole player's ship's crew
- Column with coordinates to the list of available missions in the bases and
  to the list of accepted missions in the knowledge screen
- Close the list of available missions in bases when the player reached the
  limit of missions to get
- Showing all missions available in the base during previewing them on the map

### Changed
- Updated icon for checked and unchecked options, for example, the random
  game's difficulty
- Updated icon for select or unselect all in the player's ship's crew member
  inventory
- Updated look of the dialog for giving orders to the player's ship's crew
  members
- Updated modding guide

### Fixed
- Updating the player's ship's crew member orders
- Starting crew for players from Independent faction
- Reputation with other factions for Independent faction
- Not showing pagination buttons on the list of known bases
- Getting wrong amount of points for finishing the current in-game goal

## [8.3] - 2023-01-22

### Added
- Button to open inventory to the crew member info dialog
- Icon for dismissing the player ship's crew member. Author: Delapouite
  (https://game-icons.net), license CC BY 3.0

### Changed
- Updated look of the map cell info
- Moved setting the player's ship's crew member's priorities to the crew
  member's info dialog
- Updated modding guide
- Show the info dialog with available actions instead of menu in the list
  of the player's ship's crew members

### Fixed
- Typos in changelog
- Crash when moving all items from the player's ship crew member inventory to
  the ship's cargo
- Close the player's ship's crew member's inventory after moving all items
  from it
- Counting amount of work needed to upgrade the player's ship's module's
  durability
- Crash when upgrading player's ship's module's durability

## [8.2] - 2022-12-25

### Added
- Ability to set colors for various the game's elements, like the map,
  messages, etc. in themes
- Icon for set the player's ship destination. Author: Delapouite (https://game-icons.net),
  license CC BY 3.0
- Help entry for the knowledge screen
- Hide Equip button in the player's ship crew inventory if the item cannot be
  equipped
- Use colors of events when showing information about them in the map cell info

### Changed
- Updated look of in-game tables
- Color of buttons remove repair priority and cancel the ship destination
- Updated modding guide
- Show the info dialog with available actions instead of menu in the list
  of known bases and the list of known events
- Updated look of the accepted mission's actions menu
- Reduced influence of battering ram installed on the player's ship on
  generating enemies
- Some colors of the game default theme
- Updated look of the list of known events

### Removed
- The icon for remove actions, replaced it with icon for cancel actions

### Fixed
- Counting the amount of enemies killed in boarding combat
- Typos in help text

## [8.1] - 2022-11-27

### Added
- Ability to clear numeric fields (GitHub issue #95)
- Numeric fields to sliders (GitHub issue #96)
- Block the player from moving in-game dialogs outside the game window
- The separated color on the list of known bases for the base which is set
  as target for the player ship
- The separated color on the list of known events for the event which is set
  as target for the player ship
- The separated color on the list of accepted missions for the mission which
  is set as target for the player ship

### Changed
- Updated README.md
- Updated look of the crew members info's dialog, lists of the known bases and
  events

### Fixed
- No check for the correct amount of items to craft during setting a crafting
  recipe
- Set proper max amount of money to train when the player doesn't have enough
  money for training
- Crash when moving around in-game dialogs with mouse (GitHub issue #98)
- Cursor position inside some numeric fields after entered a number
- Loading weapons into the game
- Setting the list of available items' types during trading, looting bases and
  in the player's ship's cargo
- Keyboard shortcuts for setting the player's ship's speed
- Showing the destroyed ships in the game statistics
- Crash on finished boarding combat
- Crash on entering the game statistics when there is a list of killed mobs
- Starting ship for the Inquisition faction and Hunter career
- Reading factions' flags from files
- Faction with `fanaticism` flag should start the game with maxed morale
