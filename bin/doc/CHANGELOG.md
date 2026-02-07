# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

### Changed
- Added some time for deliver items to bases missions

### Fixed
- Updating the list of items for trade after buying or selling when the list
  was sorted
- Perhaps fixed running the game on Linux
- Selling items in bases
- Counting price of damaged items when selling them
- Counting profit from selling items
- Using materials during upgrading the player's ship's modules
- Counting bonus to the player's ship's speed from the crew members' skills
- Crash when buying items
- Possible crash when meeting a friendly ship
- Sometimes game doesn't generate missions in bases

## [11.9] - 2026-01-25

### Added
- Ability to set special feature for an item whe setting crafting orders

### Changed
- Updated help

### Fixed
- Generating enemy ship events after loading a game
- Default crafted items' quality
- Saving the current story to a file
- Starting a new story
- Checking for needed fuel amount during the ship's movement
- Progressing the current story
- Counting fuel usage when waiting in place in the sky
- Counting distance traveled by the player's ship during the game
- Showing keyboard shortcuts in the in-game help
- Crash when giving the last item from the player's ship's cargo to a crew
  member
- Possible crash when accepting missions in bases

## [11.8] - 2025-11-30

### Added
- The new section in in-game help with information about items
- Missing option to sorting workshops in the crafting screen
- Ability to set desired item's quality when setting crafting orders

### Changed
- Reduced chance for different than normal quality items for recruits in bases
- Updated help

### Fixed
- Items can be studies or deconstructed without a proper tool

## [11.7] - 2025-11-02

### Added
- The quality of food and drinks affects their effectiveness and the impact on
  morale of the player's ship's crew members when consumed
- The quality of medicines affects their effectiveness during healing wounded
  crew members on the player's ship
- The quality of tools and materials affects effectiveness of upgrading and
  repairing progress of the player's ship's modules
- The crafter's skill, the recipe's difficulty, used crafting tools and
  materials affects quality of the result's items during crafting
- Information about the crafting recipe's difficulty to the recipe's
  information dialog
- Information about the quality of owned items to the information about a
  recruit available to hire

### Changed
- Updated help
- Recruits in bases can have now equipment with random quality

### Fixed
- Updating the game's header after installing or removing modules in a shipyard
- Crash when trying to disable an engine on the player's ship
- Information about trainees in a training room in the player's ship
- Showing information about failed assigning the player's ship's crew member to
  a module

## [11.6] - 2025-10-05

### Added
- Information about items' quality in the player's ship's info and bases' loot
  screen
- Crafting recipe for Medical Herbs Seeds
- Ability to buy the new recipe in Agricultural bases

### Fixed
- Tooltips for select and unselect buttons in the player's ship's crew member's
  inventory dialog
- The player's ship's crew members don't back to work after finished training
  in bases
- Gaining experience by trader when training skills in schools
- Training skills in schools by the player's ship's traders
- Moving an item from the player's ship's crew members' inventory to the ship's
  cargo unequip another item in the inventory
- Crash during cleaning the player's ship

## [11.5] - 2025-09-07

### Added
- Information about a mission in the map's info when previewing available
  missions in bases

### Fixed
- Entries in changelog
- Crash on sorting the player's ship's crew members' inventory
- Closing the player's ship's orders menu with Escape key
- Closing the player's ship's travel destination menu with Escape key
- Blocking the player's ship's crew member's inventory window when moving
  items from it to the ship's cargo
- Scrolling with mouse's wheel doesn't work in general info in the player's
  ship info screen
- Using healing tools during healing the player's ship's crew members

## [11.4] - 2025-08-10

### Added
- Limit items in bases. Large bases can have max 128 various items for trade,
  medium 64 and small 32

### Changed
- Text on some close buttons to match the rest
- Updated help

### Fixed
- Reading amount of installed modules on the player's ship from saved game
- Icon for cancel button when installing modules in bases
- Typos in the list of available missions in bases

## [11.3] - 2025-07-13

### Changed
- Generated cargo in bases is now more random
- Updated help

### Fixed
- Undocking or escaping from a base doesn't show the map
- The player's ship's crew members escape from boarding party
- Don't show the player's ship's orders' menu in combat
- Possible crash when showing information about recruits in bases
- Showing info about owned money in the negotiate hiring dialog
- Loading recruits equipment from saved games
- Moving in-game dialog with setting a new goal
- Showing information when there is an issue during trading items
- Timer when closing the message's dialog when there is an issue during trading
  items

## [11.2] - 2025-06-15

### Added
- Faction's descriptions to the list of reputations in the ship's info screen

### Changed
- Updated help
- Updated descriptions of factions
- Generating enemies and friendly ships now depends on the player's reputation
  in factions instead of the player's ship's modules and ammunition

### Fixed
- Crash when starting a combat
- Typo in tooltip when training a crew member in bases
- Sorting the player's ship's crew members by orders
- Updating the game's header after training a skill in a base and hiring a
  recruit
- Crash when dropping items from the player's ship's cargo
- Screen is blocked after giving an item from the player's ship's cargo to a
  crew member
- Showing information about the game's data error when starting the game
- Tooltips for buttons in the dialog with information about a recruit in a
  base
- Scrolling the general ship information section with mouse wheel

## [11.1] - 2025-05-18

### Changed
- Updated color of cleaning ship icon to match the default colors of the
  game
- Updated modding guide

### Fixed
- Possible crash when updating population in bases
- Generating enemies' equipment
- Tab key traversal in the negotiate hire a recruit dialog
- Hide the negotiate dialog with Escape key when Hire button is selected
- Don't generate shield for enemies if they use two handed weapons
- Closing the buying recipes dialog with Escape key
- Hide the dialog with information about a mission in a base with Escape key
- Tab key traversal in the accept a mission in a base dialog
- Tab key traversal in the crew member info dialog
- Crash on Tab key traversal in the crew member priorities list
- Look of the base's info dialog when the player is unknown in the base
