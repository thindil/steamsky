# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

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
