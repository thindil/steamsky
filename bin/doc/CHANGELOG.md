# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

### Added
- Resize button to the enemy's ship's status in the combat screen

### Fixed
- Resizing sections in the combat screen
- Typos in the enemy's information in the combat screen

## [10.0.1] - 2024-07-14

### Fixed
- No refreshing information about the player's ship after trading
- Can't hire the last recruit from a base
- Crash when comparing modules in a shipyard
- Crash when showing information about the skill of the recruit in bases
- Showing information about the module's status in a shipyard
- Outdated the player's ship's information after entering a shipyard screen
- Tooltip for stop upgrading the module's button.
- Game doesn't clear the list of accepted missions after quit

## [10.0] - 2024-06-30

### Added
- New mob for the starting player's ship

### Changed
- Updated the look of the dialog used to buy or sell items in bases
- Crew for the starting ship for Inquisition's Hunter career

### Fixed
- Showing information about the selected crew member
- Typo in changelog
- Centering the map on the player's ship after move
- Wrong position of ship after trading
- Tooltip for the close button in move item from a crew member to the ship's
  cargo
- Setting an event or a mission as the destination for the player's ship
- Updating the debug menu after changes
- Refreshing the list of known bases
- Crash on closing some in-game dialogs
- Showing information about the player's ship's cargo
- Tooltip for an item's durability in the player's ship's cargo
- Saving the map and the player's ship
- Crashing on Windows
- Sorting items in the trade screen doesn't remember the selected type
- Crash on loading the game's images on Windows
- Hangs on starting a new game on Windows
- Hangs when an event ends
- Starting upgrading an engine's fuel usage
- Showing information about upgrading a gun's damage
- Showing the wrong icon when there is the player's ship upgrade in progress,
  but no one is assigned to it
- The player's ship is undocked when it is repaired in a base
- Showing the list of available missions in bases outside the starting base
- The game doesn't save known bases
- Typo in the timed close dialog's button
- No option to execute mission in the orders' menu
- Not showing information about a mission on the map after loading the game
- Showing information about double price events in the map info
- Saving information about visited bases
- Crash when the player's ship's armor is destroyed during combat
- Tab key doesn't work in Orders menu
- Crash when close the message's dialog before its timer ends
- Crash on starting a new game
- Crash when a in-game events expire
- Various small problems with displaying the game UI
- Crash when trying to change order for a crew member after assigned another
- Crash when starting a new game as Undead
- Starting a new game as Undead with empty ship
- Loading events from saved game
- Crash in combat when using a battering ram
- Defenders don't back to their defend position when they have set the proper
  orders' priority
- Drones can't find food in the player's ship's cargo
- Wrong in-game time after waiting for rest of tired crew members
- Wrong in-game time after starting a new combat
- Typos in the trade's errors' dialogs
- Drawing the sky map when the player's ship is near its edges
- Typo in the skill's level's tooltip

## [9.9] - 2024-04-14

### Added
- Colored version of icons for setting study and deconstruct orders plus
  giving orders to the player's ship's crew members

### Changed
- Updated README.md
- Updated modding guide
- Updated look of the dialog used to start a crafting order
- Updated look of the dialog used to assign an order to the player's ship's
  crew member

### Fixed
- Setting the player's cabin in the ship during starting the new game
- Assigning crew members to the player's ship's modules
- Showing a module's name in the player's ship's information screen
- Position of the button to cancel crafting order in the module's info dialog
- Typos in tooltips in the player's ship's module's info dialog
- Possible crash when trading with another ship
- Skills use wrong attributes
- Crash on loading a saved game
- Crash on starting a new game
- Crash when trying to show information about error on Windows
- Crash when starting a new game on Windows

## [9.8] - 2024-03-17

### Added
- Colored version of icons for accept negotiations with a recruit in a base and
  setting a crafting order

### Changed
- Updated look of the dialog with information about a recruit
- Updated look of the dialog used to negotiate hiring a recruit
- Updated look of the dialog with information about an available mission in a base
- Updated look of the dialog used to accept an available mission in a base
- Updated look of the dialog with information about a crafting recipe
- Updated modding guide

### Fixed
- Typo in the changelog
- No icon on sell button in the trade screen if selling is the only option

## [9.7] - 2024-02-18

### Added
- Colored version of icon for show something on the map

### Changed
- Updated look of the dialog with information about the player's ship's crew
  members
- Updated look of the dialog with information about the player's ship's modules
- Updated look of the dialog with information about a known base
- Updated look of the dialog with information about a known event
- Updated look of the dialog with information about an accepted mission
- Color of image for target action
- Updated modding guide
- Updated look of the dialog with information about a recruit

### Fixed
- Showing assigned ammunition to a gun on the player's ship's module's list
- Loading recruits in bases from saved games

## [9.6] - 2024-01-21

### Added
- Ability to set the amount of time to wait for the "Wait" button
- Ability to set the unit of time (minutes, hours or days) for custom waiting
  time in the wait menu

### Changed
- Made the dialog with information about the player's ship's module wider

### Fixed
- Reading the player's ship's crew members' equipment from saved games
- Crash when trying to show information about the boarding party's combat
- Generating equipment for the player's and npc's ships' crew members
- Setting random faction and career in the new game setting doesn't work
- Showing information about ammunition in the player's ship's module's info
  dialog
- Keyboard shortcut for undocking from bases
- Default keyboard shortcuts for moving the map
- Scrolling the player's ship modules and crew members lists with mouse wheel
- Showing the player's statistics after they death
- Crash when updating any ship's cargo, ticket #98e4ec7bff

## [9.5] - 2023-12-24

### Added
- Colored version of icon for edit the player's ship's, crew members or ship's
  modules names

### Changed
- Updated look of the items' information dialogs in the trading, ship info and
  looting bases screens
- Updated README.md
- Updated look of the rename dialogs

### Fixed
- Don't show empty categories of items in the trading screen
- Crash when trying to show the crew members with the selected skill in the
  ship info screen
- The last member of the player's ship's crew doesn't work
- Crash when the game is trying to create an enemy patrol at a friendly base
- Coloring in-game messages in the combat screen
- Crash when trying to show the player's ship's gun's info after assigning an
  ammo to it
- Coloring messages outside the combat screen

## [9.4] - 2023-11-26

### Added
- Icon for the player's ship's crew members' inventory. Author: Delapouite
  (https://game-icons.net), license CC BY 3.0
- Colored version of icons for move and give an item to the player's ship's cargo
- Colored version of icon for drop an item from the player's ship's cargo

### Changed
- Made some dialogs' buttons colorful
- Updated modding guide
- Updated look of the items information dialog in the trading screen

### Fixed
- Displaying tables if the first column has an empty header

## [9.3] - 2023-10-29

### Added
- Colored version of buy and sell icons

### Changed
- Updated look of dialogs in the shipyard screen
- Updated README.md
- Made some dialogs' buttons colorful
- Updated modding guide

### Fixed
- Typos in the changelog
- Loading the player's ship's upgrade and repair priority statuses
- Loading do the sky base is known to the player
- Crash when trying to sell the ship's module which has owners
- Crash when trying to see an harpoon gun information in the shipyard's screen
  during installation when another harpoon gun is installed
- Recruits in bases have wrong items equipped
- Game time information doesn't update after training skill in bases
- Crash when starting a combat
- Setting names for Drones bases

## [9.2] - 2023-10-01

### Changed
- Updated the default game's theme
- Updated look of dialogs in the shipyard screen
- Module's damage information in the module's remove dialog in bases' shipyards

### Fixed
- Info about the module's damage in the remove module dialog in a base's
  shipyard
- Setting the player's ship's speed on the map
- Events don't disappear when they end
- Crash when starting melee combat
- Saving the game in the debug menu
- Showing the player's ship's orders menu during combat
- Smaller factions start in large factions' bases
- Counting the amount of time need to upgrade the player's ship's modules
  when starting an upgrade

## [9.1] - 2023-09-03

### Added
- The game date to the in-game messages

### Changed
- Redesigned the map info dialog
- Updated look of dialogs in the shipyard screen
- Updated look of the shipyard screen
- Updated look of in-game dialogs
- Updated header of the player's ship's crew member dialog
- Color of images for buy and sell actions

### Fixed
- Typos in old changelog
- Game not update the time during docking or undocking from bases
- Saving the list of destroyed enemy's ships
- Typo in the dialog after finishing a story
- Removed empty lines from the map info dialog after finishing an event
- Loading the player's goals into the game
