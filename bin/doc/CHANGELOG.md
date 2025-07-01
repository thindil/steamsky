# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

### Fixed
- Undocking or escaping from a base doesn't show the map
- The player's ship's crew members escape from boarding party
- Don't show the player's ship's orders' menu in combat
- Possible crash when showing information about recruits in bases
- Showing info about owned money in the negotiate hiring dialog
- Loading recruits equipment from saved games

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
