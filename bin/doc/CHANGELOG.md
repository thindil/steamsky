# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

### Changed
- Updated interface

### Fixed
- Code documentation
- Showing the player ship's crew info

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
