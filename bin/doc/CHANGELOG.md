# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

### Added
- Option to select and unselect all item in the player's ship's crew members'
  inventory
- Option to wear, take off, move to the player's ship's cargo the selected
  items from a crew member's inventory
- Information about the current order of the player's ship's crew member to
  his/her information dialog
- Option to set the selected crew member order on the player's ship's crew
  members' list and in the crew member's info dialog
- Icon for give orders to crew members. Author: Delapouite (https://game-icons.net),
  license CC BY 3.0
- Ability to scroll the information in the player's ship crew member's info
  dialog

### Changed
- Updated modding guide
- Updated look of general information about the selected player's ship's crew
  member
- Small update to the information about the selected player's ship's module
- Removed the list of orders from the crew members' actions' menu

### Fixed
- Position of Close button in the set skill for training rooms dialog,
  the message's dialog and the information about the selected module dialog
- Tab traverse in setting skill for training rooms dialog
- Closing setting skill for training rooms dialog after choosing the skill
- Closing setting skill for training rooms dialog with Escape key
- Position of the confirmation dialog for deleting a saved game
- Setting icon for showing that all crafting orders are running
- Use proper icon for showing that all crafting orders are running
- Crash when accepting transporting passenger missions
- Position of scrollbar in the information dialog about the selected crew
  member
- Showing the title of the crew member's information dialog when the
  scrollbar is visible
- Position of information about tiredness and health of the selected crew
  member in the information dialog

## [7.8] - 2022-09-04

### Added
- Icons for move item to cargo, equip item, unequip item, unchecked empty
  checkboxes. Authors: Lorc, Delapouite (https://game-icons.net), license
  CC BY 3.0
- Ability to move, equip or take off a couple of items in the player's
  ship's crew members' inventory

### Changed
- Updated modding guide
- Updated look of Show item info in crew inventory and Move item dialogs
- Show item info dialog instead of actions menu in the player's ship's crew
  member inventory list
- Updated README.md
- Updated contributing guide

### Fixed
- Showing information about the item in the player's ship's crew member's
  inventory
- Showing information about the item damage chance in the items' info dialog
- Crash on start in Windows version of the game
- Typos in README.md

## [7.7] - 2022-08-07

### Added
- Option to show all, craftable or non-craftable recipes in Crafting screen
- Icon for craft, study, deconstruct and negotiate buttons. Authors: Lorc,
  Delapouite (https://game-icons.net), license CC BY 3.0
- Information where the selected recruit wears its equipment

### Changed
- Image for unchecked checkboxes and values in tables. Author: Bartek Jasicki
- Updated images for checked checkboxes and values in tables, unchecked and
  checked radioboxes, cancel action.
- Updated modding guide
- Updated look of Show crafting recipe, Give, Drop, Take, Buy, Sell items,
  Show inventory, Set crafting recipe, Recruit info, Hire recruit, Show
  mission info, Accept mission and Rename dialogs
- Show both, text and icon on "Close" button in dialogs
- Show the crafting recipe info instead of the actions menu in the crafting
  recipes list
- Show the recruit info instead of the actions menu in the recruits lists in
  bases
- Show the mission info instead of the actions menu in the lists of available
  missions in bases

### Removed
- Craftable column from crafting recipes list in Crafting screen

### Fixed
- Showing the Close button in the available mission, known base info and
  installed module info dialogs
- Generating equipment for recruits in bases
- Crash when trying to buy items in bases
- Tab traverse in negotiate hire recruit dialog and school in bases
- Closing the hire and accept mission dialogs with Escape key when another
  element than buttons is selected

## [7.6] - 2022-07-10

### Added
- Info about free cargo space of the player's ship during looting empty bases
- Tooltip to max amount button when dropping items in empty bases
- Buttons to give the item to the crew member, drop the item from the
  player's ship cargo, buy and sell item from the base or trader ship in the
  item's information's dialog
- Icons for give, drop, buy and sell buttons. Authors: Delapouite, Skoll
  (https://game-icons.net), license CC BY 3.0

### Changed
- Button "Take all available" to "Take X of them" when looting items from
  empty bases
- Show take actions buttons when looting items from bases only when the player
  has free space in the ship's cargo
- Text "Ok" on buttons in the items' manipulation dialogs, like trade, give or
  drop to name of the proper action
- Text on th button "Close" for some dialogs to icon
- Updated modding guide
- Show item info dialog with available actions instead of menu in player's
  ship's cargo, during looting empty bases and trading items
- Updated look of Give, Drop, Take, Buy, Sell items and Show Inventory dialogs
- Image for cancel action. Author: Sbed (https://game-icons.net),
  license CC BY 3.0

### Fixed
- Typos in CHANGELOG.md
- Showing tooltip with information about bonus to trade from skills and
  reputation during taking items in empty bases
- Crash when trying to see detailed information about items
- Showing Close button in the crafting recipe and the crew member info dialogs
- Updating price info during buying or selling items when setting the amount
  with the max amount button
- Tab traversing in Give items dialog

## [7.5] - 2022-06-12

### Added
- More information about gained money during buying or selling items in bases
  or from traders
- Changed max amount information during buying or selling items in bases or
  from traders to button which enter that amount in the entry field
- Tooltip with information that max amount of items to buy or sale counts
  also the player's reputation in the base and the trader's proper skill
- Changed max amount information during dropping items from the ship's cargo
  to button which enter that amount in the entry field
- Colors to underlined, bold and italic texts in the help
- Option to configure color of special names and keys, bold, italic and
  underlined text in the help in themes
- Changed max amount information during moving items from the crew members'
  to the ship's cargo to button which enter that amount in the entry field

### Changed
- Updated modding guide
- Better information about differences between modules during installing
  them in bases' shipyards
- More general information about type of ammunition needed by guns and
  harpoon guns during installing them in bases' shipyards
- Updated help
- Better help formatting
- Don't show empty crew member's inventory
- Better dialog to give items from the player's ship's cargo to the selected
  crew member

### Fixed
- Crash during entering boarding combat
- Crash when showing information about boarding combat related crew info in
  combat, ticket #303b13c5d3
- Position of information about boarding party and defenders during combat
- Can't start a new game after resigning from old one and showing its stats,
  ticket #915e2167fa
- Crash when entering the list of available goals
- Not setting properly amount of installed modules after installing a new
  module in shipyard
- Problems with paginations when switching between installing and removing
  modules in bases' shipyards
- Typos in contributing guide
- Typo in info about unique module during installation modules in shipyards
- Typos in help
- Info in help about installing or removing the ship's modules
- Crash on moving items from the crew members' inventory to the ship's cargo
- Crash after moving the last item from crew members' inventory to the ship's
  cargo
- Crash when ship's cargo is empty

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
- Better information about differences between modules during installing
  them in bases' shipyards

### Fixed
- Reset the map after quit and start a new game
- Crash when pressing keyboard shortcut for move to button
- Amount of installed modules shouldn't be raised when installing a new armor,
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
