# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

### Changed
- Updated color of no pilot warning icon to match the default colors of the
  game
- Added money name to reward info in the mission's info dialog

### Fixed
- Removed colon from the end of lists of boarding party and defenders in the
  combat screen
- Setting a gunner in combat to Nobody removes completely gunner setting for
  the gun
- The info about positions on the player's ship doesn't upgrade when the player
  change a member's order in combat
- Crash when updating a base's population during the game
- Showing the enemy's ship's damage during combat
- Showing or hiding options in trade screen after changed the type of items to
  show
- Showing or hiding options in shipyards in bases after changed the type of
  modules to show
- Showing or hiding searching field when buying recipes from bases
- Assigning crew members to boarding party or defenders during combat
- Hiding the crew managing option in the player's ship's info screen
- Rouding the time needed to repair the player's ship in bases
- Gaining or losing reputation in factions
- Don't create bases at the map's borders
- Showing information about availability of new missions in bases in the
  knowledge screen
- Showing information about home base when there are new missions available in
  the selected base

## [10.9] - 2025-03-23

### Added
- Auto setting the destination for the player's ship when accepting a mission
  in a base. It can be enabled or disabled in the game's options. By default
  it is enabled
- Check if the player doesn't spend money below the low level of fuel during
  buying items
- The player's reputation among the factions. At the moment it doesn't affect
  the game in any way

### Fixed
- Reading the game's themes from files
- Undocking from bases doesn't charge for docking
- Crash when asking for events in bases
- Gaining or losing reputation in bases when the player reached max or min
  level of the reputation
- Selecting an event after sorting the list of known events
- Tooltip for wait button doesn't update when the amount of minutes to wait
  was changed
- Counting speed of the player's ship

## [10.8] - 2025-02-23

### Added
- Ability to manage workshops' crafting orders from the crafting screen
- Ability to show crafting recipes only for a selected workshop in the
  crafting screen
- Button "More options" to the list of crafting recipes in the crafting
  screen

### Changed
- Show "More options" button in the shipyard only when needed - the tab
  "Install module" is shown.

### Fixed
- Crash when starting a new game with random goal
- Searching for crafting recipes in the crafting screen isn't case-insensitive
- Searching for modules to install in shipyards

## [10.7] - 2025-01-12

### Changed
- Updated modding guide
- **BREAKING**: The bases' setting now accepts any CSS name of color as the
  bases' types' color on the map. This change should affect only the modified
  version of the game. Most of players shouldn't encounter any problems.
- Updated the game's data to the new version of bases' types' colors
- Updated colors of icons to match the default colors of the game

### Fixed
- Counting ships' speed
- Showing information about overloading the player's ship
- Showing 'Repair ship' order multiple time on the player's ship's crew's
  members' orders list
- Crash when trying to start crafting order when there is no free space on
  the player's ship

## [10.6] - 2024-12-15

### Added
- New icons for the map's manipulation: move up, move left, move right, move
  down left, move down, move down right, author:
  Delapouite (https://game-icons.net), license CC BY 3.0

### Changed
- Updated modding guide
- The look of buttons used to move the map around

### Fixed
- Typo in changelog
- Showing the visited bases' colors on the map
- Giving items from the ship's cargo to a crew member

## [10.5] - 2024-11-17

### Added
- Image of the game's logo
- New icons for the map's manipulation: menu, move up-left, move up-right,
  authors: Lorc, Delapouite (https://game-icons.net), license CC BY 3.0

### Changed
- Updated modding guide
- **BREAKING**: Redesigned how the technical logging works. This changes the
  game's command line parameters, which made incompatible with the previous
  versions of the game. But most people shouldn't notice it.
- Updated the look of the trade's screen, the game's main menu, the
  shipyard's screen, the buying recipes' screen and the available missions
  screen
- Updated README.md

### Removed
- The Rye font as the game's logo was replaced by an image

### Fixed
- Showing the error dialog in the game's main menu

## [10.4] - 2024-10-20

### Added
- New icons for expand, contracts and more options in info's sections. Author:
  Delapouite (https://game-icons.net), license CC BY 3.0

### Changed
- Updated modding guide
- Use the new icon for expand and contract infos' sections on various screens
- Updated the look of the player's ship's crew and cargo info sections in the
  ship's info screen
- Updated the look of the list of known bases in the knowledge screen
- Updated README.md
- Updated contributing guide

### Fixed
- Setting the game's difficulty level when starting the game
- Showing the amount of available ammunition for guns in combat

## [10.3] - 2024-09-22

### Added
- Information about amount of food, fuel and drinks in the player's ship to the
  game's header (GitHub issue #97)
- New icons for fuel and drinks amount information. Authors: Lorc, Delapouite
  (https://game-icons.net), license CC BY 3.0

### Changed
- Updated modding guide
- Show the player's ship's orders' button only when there are any available
  orders
- Updated the look of the dialog with information about an error

### Fixed
- Typo in changelog
- Crash on finishing a crafting order by the player's ship's crew member
- The next button on the list of recipes to buy in bases

## [10.2] - 2024-08-25

### Added
- New icon for food amount information. Author: Lorc (https://game-icons.net),
  license CC BY 3.0

### Changed
- Updated look of information about available missions in bases
- Updated look of information about available money when buying recipes, repair
  the ship or healing wounded crew members in bases
- Updated look of information about available money and space on the player's
  ship in shipyards
- Updated look of information about available space on the player's ship
  during looting an empty base
- Made checked checkbutton a bit darker to match the game's default theme
- Don't reset the items' type selection when buying or selling items in bases
  or with ships
- Redesigned dialogs for selling, buying and dropping items from the player's
  ship cargo
- Redesigned the movement's buttons on the map, added the player's ship's
  orders button

### Fixed
- Crash when dropping items in empty bases
- Showing the last combat screen when restarting the game

## [10.1] - 2024-07-28

### Added
- Information about durability difference between modules when installing a
  module in shipyards
- Resize button to the enemy's ship's status in the combat screen

### Changed
- Updated README.md
- Updated look of information about the free space in the player's ship's cargo
- Updated look of information about the free space in the player's ship's crew
  members' inventory
- Updated look of the game's statistics' screen
- Updated look of the enemy's information's section in the combat screen
- Updated look of information about money and free space in the trade screen
- Updated look of information about money in the bases' schools' screen

### Fixed
- No refreshing information about the player's ship after trading
- Can't hire the last recruit from a base
- Crash when comparing modules in a shipyard
- Crash when showing information about the skill of the recruit in bases
- Showing information about the module's status in a shipyard
- Outdated the player's ship's information after entering a shipyard screen
- Tooltip for stop upgrading the module's button.
- Game doesn't clear the list of accepted missions after quit
- Resizing sections in the combat screen
- Typos in the enemy's information in the combat screen
- Information about the player's money in bases' shipyards
- Crash on viewing information about an installed engine in shipyards
- Showing information about the installed cockpit in shipyards
- Showing information about owned money in bases' schools
- Typo in old changelog
