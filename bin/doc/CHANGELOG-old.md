# Changelog
All notable changes to this project older versions are documented in this file.

## [10.0] - 2026-06-30

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
- Crash when an in-game events expire
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

## [9.0] - 2023-08-06

### Added
- Message about spotted en enemy patrol to the event

### Changed
- Made more items available for buy in traders' ships
- Updated README.md

### Fixed
- Generated NPC's ships start with docked speed, so the player can always
  escape them
- Crash when the game deletes an expired event
- Showing Ask For Events button even after asking for them
- Crew members don't need to rest or consume anything
- Resetting the selected type of items after buying or selling something in
  bases or to traders
- Game hangs on creating a new game when there are no big bases on the map
- Saving the player's career
- Adding full docks event in debug menu
- Possible crash when encountering NPC's ship
- Updating events after wait in place
- Sky bases at the game's start can be created at the border of the map
- Typos in messages about started events
- In-game error when starting a new game and move the mouse before the map is
  shown
- Prices in bases change after every transaction

## [8.9] - 2023-07-09

### Added
- Various information about some modules types to the list of the player's
  ship's modules
- Remembering the last entered value of custom wait time in the wait menu
- Info about the game's factions, player's careers and available bases types
  to the help
- Ability to select or unselect all crew members during setting boarding or
  defending parties in combat
- Remembering the size of the messages' window

### Changed
- Redesigned assigning crew members to boarding and defending parties in
  combat

### Fixed
- Showing information about finished stories
- Typo in changelog
- Look of the base's info's dialog when the player is unknown in the base
- Updating the map after move the player's ship
- Saving the player's ship's modules' upgrade action
- Crash during docking or undocking from bases
- Saving the player's ship's module's upgrade progress

## [8.8] - 2023-06-11

### Added
- Ability to assign the crew members to the player's ship's training rooms in
  the module info dialog
- Ability to assign the skill to train in the player's ship's training rooms in
  the module info dialog
- Various information about some modules types to the list of the player's
  ship's modules.

### Changed
- Updated look of the module info dialog
- Show the info dialog with available actions instead of menu in the list
  of installed modules in the player's ship

### Fixed
- Typos in changelog
- Crash when browsing modules to install in shipyards
- Saved game doesn't load when training room on the player's ship doesn't
  have set a skill to train
- Recruits in bases change every time instead of every in-game week
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

## [8.0] - 2022-10-30

### Added
- New icons for lack of pilot or engineer for sentient ships. Author:
  Delapouite (https://game-icons.net), license CC BY 3.0

### Changed
- Made the messages list in the last messages sceen wider
- Updated modding guide
- Updated README.md

### Fixed
- Crash on start when there is no the game configuration file
- Crash on trying open link when no program to open it is installed
- Mouse wheel doesn't scroll the player's ship crew member general info in
  the dialog
- Position of Close button in the dialog with information about the selected
  module from the player's ship
- Showing the list of installed modules in shipyards after installing a new
  module
- Showing information about lack of pilot or engineer on sentient ships
  (Drones)
- Crash when selling items after sorting the list of them (GitHub #93)
- Information about tools needed to train Metalsmithing skill
- Information about bases types during selecting it in the new game screen
- Crash on start a new game when selected a base type unavailable for the
  selected faction

## [7.9] - 2022-10-02

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
- Ability to rename the selected crew member in the crew member's info dialog
- Better Tab traversal to the player's ship crew member's info dialog
- Closing the player's ship crew member's info dialog with Escape key when
  various buttons in the dialog are selected

### Changed
- Updated modding guide
- Updated look of general information about the selected player's ship's crew
  member
- Small update to the information about the selected player's ship's module
- Removed the list of orders and option to rename the crew member from the crew
  members' actions' menu

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

## [7.0] - 2022-01-23

### Added
- Ability to set the keyboard shortcuts for resize sections of information
  on the ship info screen, the knowledge info screen, the ship to ship combat
  screen and the boarding combat screen
- Tooltips to the sections' buttons in the game options screen
- Short info to install ship's modules in shipyards why installation is
  impossible
- Information that the selected ship's module is unique to the module's info
  in shipyard
- Tooltips with information why the selected module can't be installed in the
  actions' menu in shipyards

### Changed
- Don't hide enemy ship status after combat
- Show full info about the player's ship's status instead of just damaged
  modules in combat
- Updated modding guide
- Only numbers are allowed as indexes for prototypes of mobiles. Can cause
  problems if your modification was using strings for prototypes' indexes.
- Updated README.md

### Fixed
- Block Install button in shipyard's action menu when installation of the
  module is impossible
- Installing armor modules when the player's ship has installed maximum
  amount of allowed modules
- Block Install button in shipyard's action menu when there is no free turret
  during installation of guns

## [6.9] - 2021-12-26

### Added
- Showing the highest skill or the level of the selected skill on the
  player's ship's crew member list
- Option to sort the player's ship's crew members list by their skills
- Tooltips to the map manipulation buttons, to move to button and to setting
  ship speed box
- Option to minimize or maximize information in the ship to ship and boarding
  combat
- Keyboard shortcuts to minimize or maximize information sections in the ship
  info screen, the knowledge info screen, the ship to ship combat screen and
  the boarding combat screen

### Changed
- Redesigned menu and back to map buttons
- Updated modding guide
- Updated README.md
- Look of map manipulation, the player's ship movement and the move to buttons
- Always show the player's ship damage info in the combat

### Fixed
- Showing some buttons have keyboard focus
- Showing which combo box (like select faction, items type, etc.) has keyboard
  focus
- Crash when canceling moving items from the player's ship crew member
  inventory to the cargo
- Some typos and grammar in modding guide
- Color of message about successfully finished destroy enemy ship mission
- Crash when trying to repair the ship in bases
- Crash when trying to heal wounded crew members in bases
- Crash when trying to sell items which are not available in bases
- Crash in melee combat when the enemy has shield
- Crash on showing the player's ship's crew member menu when more than one the
  ship's module needs repairs
- Showing proper combat screen when the player is in boarding party after back
  from the ship info screen
- Look of an empty progress bar (for example, for destroyed ship modules)

## [6.8] - 2021-11-28

### Added
- Recipe name to the recipe info dialog's header
- Scrolling help's topics lists when automatically showing it, like hiring
  crew members, etc
- Option to assign crew members during setting crafting orders
- Setting crafting order button change its text depending on the type of the
  order in set crafting order and show crafting recipe info dialogs

### Changed
- Redesigned the main in-game, load game, known event, item in cargo, crafting
  recipes, recruits, item in crew inventory, mission in base, ship's module,
  known base, accepted mission, trading item, crew member, buying recipes,
  heal wounded, repair ship in base, installing and removing modules in
  a shipyard, loot item in empty bases menus, should fix (GitHub #88) too
- Updated help
- Updated setting crafting orders dialog
- Pattern of the configuration file for keyboard shortcuts. This mean that
  the old will not work
- Updated look of orders for the whole crew
- Updated modding guide

### Fixed
- Showing the proper event on the map after selecting it from the list
- Selecting available mission in bases after sorting their list
- Crash on removing modules in shipyards
- Delete save game file after resign from game
- Some typos in help
- Tab traversal in the set crafting order dialog
- Closing the set crafting order and show info about crafting order dialogs
  with Escape key
- Resetting keyboard shortcuts for the player's ship movement
- Default settings for keyboard shortcuts for changing the player's ships
  speed

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

## [6.0] - 2021-04-18

### Added
- Pagination to lists of known bases and events, accepted missions, shipyard,
  trade, player's ship cargo, crew and modules, available recruits in bases
  and available items to loot from bases
- Missing info about free space in player ship cargo
- Missing info about free space in crew members inventory

### Changed
- Don't create patrol and explore missions on fields with bases
- Updated interface
- Redesigned crew member's inventory screen
- Updated README.md

### Fixed
- Closing wait menu with Escape key returns to map
- Refresh ship info and knowledge screens after wait some time
- Resetting ugprade progress after finishing upgrading modules
- Crash on generating recruits in unfriendly bases
- Search on trade list doesn't work if item isn't in the ship cargo
- Crash on entering the knowledge screen again
- Possible crash when ending the game
- Clear search entry in trade when refreshing the items list
- Crash when player accepts the last mission in base
- Looking for items in trade when the item's type is selected
- Typos in trade screen
- Autoresizing of tables
- Remembering size of last messages window
- Generate bases at borders of the map
- Showing info about the highest skill of recruits in bases
- Showing list of known bases after reenter knowledge screen
- Reseting search field in install modules in shipyard screen
- Showing info about docking cost in abandoned bases
- Crash on lack of permission to write to directory selected for saves
- Showing the game version in main menu when showing error about the game data
- Crash on set invalid data directory
- Show console window on Windows
- Asking friendly ships for bases and events
- Lack of name of friendly ship in message when asking it for bases or events
- Color of the info about friendly trader in map info
- Starting and completing missions
- Progressing stories
- Delivering medicines to the diseaded bases

## [5.9] - 2021-03-21

### Added
- Missing option to remember combat orders (Github issue #74)

### Changed
- Updated interface
- Finished redesign of the ship info screen

### Fixed
- Hiding enemy ship's info after combat
- Crash on entering the ships combat again
- Crash when trying to load configuration file from older game versions
- Crash when trying to enter the game settings on Windows (Github issue #73)
- Crash on showing messages in main menu
- Showing the game statistics on resign from the game
- Position of Close button on the game statistics screen when resigning from
  the game
- Showing the game after reloading it (quit to main menu and load or start the
  game again)
- Crash on damaged engines during fly (Github issue #74)
- Crash after victory in combat (Github issue #75)
- Crash when player ship crew member die in melee combat
- Setting themes directory via command line arguments
- Hide next turn button after finishing boarding combat
- Crash on finishing deliver item missions
- Showing profit in trade screen
- Counting max sell amount in trade
- Showing sell option when selling is not possible
- Crash when can't accept mission in bases
- Showing info about the selected module when it is upgraded

## [5.8] - 2021-02-21

### Changed
- Redesigned installing and removing module screen in shipyard bases
- Redesigned looting bases screen
- Updated interface
- Updated contributing guide
- Updated README.md

### Fixed
- Showing the selected modules type in shipyard with the current search text
- Showing the selected type of items in loot screen
- Showing longer description for modules in shipyard
- Possible crash on accepting missions in bases
- Cost of the player's ship repairs in Industrial base
- Typos in knowledge screen
- Crash on showing the current player's goal

## [5.7] - 2021-01-24

### Changed
- Updated README.md
- Updated contributing guide
- Updated modding guide
- Updated interface
- Redesigned trade and hire recruits screen

### Fixed
- Showing profit info on trade screen
- Crash on trying to trade with friendly ships
- Crash on showing info about errors
- Updating messages after wait action
- Showing long goals names on button in the game statistics
- Looking for crafting recipes to buy in bases
- Button text for buying recipes or repairs in bases

## [5.6] - 2020-12-27

### Added
- Zooming in and out map with the mouse wheel
- Option to set defenders in combat screen

### Changed
- Updated interface
- Keyboard shortcut for the next turn in combat to Enter
- Updated descriptions for harpoon guns and battering rams
- Updated README.md

### Removed
- Setting for the animations

### Fixed
- Escape key behavior in inventory and crew member dialogs
- Position of messages window after leaving the last messages screen
- Some typos in changelog
- Position of the game statistics
- Showing player's ship damage in combat
- Showing the enemy's ship's modules info when it is destroyed
- Scrolling last messages window when older messages showed as first
- Font on help topics list
- Scrolling the crew member info, assigning crew member to module and crew
  members inventory info
- Possible crash when ending delivery mission (issue #71)
- Scrollbar position in crew member info dialog
- Ending combat turn with space key
- Showing the lists of gunners in combat
- Crash in combat when the player's ship doesn't have installed guns
- Crash on giving order to back to ship in boarding combat
- Setting crew members positions (pilot, engineer, gunner) in combat
- Crash on setting crew members orders priorities

## [5.5] - 2020-11-29

### Changed
- Updated interface

### Fixed
- Showing horizontal scrollbar in known bases info
- Position of destination menu
- Showing base type when its name has more that one word
- Crash when default base type has more than one word in name
- Showing player when he/she is at left or bottom border of the map
- Showing story icon on the map when player don't have any active
- Showing scrollbars for stories on knowledge screen
- Don't show the move map dialog on other than the map screens
- Setting amount of saved messages in saved games
- Don't show the game menu when there is any dialog
- Crash on giving rest order to the crew members
- Setting and remembering size of messages window
- Wait order when docks in base are full
- Showing combat UI
- Typo in info about daily payments
- Game menu in combat
- Crash on show/hide the game help
- Close button in the game option returns to map instead of combat
- Menu shortcuts in combat
- Keyboard shortcuts on information screens in combat back to map

## [5.4] - 2020-11-01

### Changed
- Updated interface
- Updated README.md
- Updated help
- Updated MODDING.md
- Moved cargo info to ship info screen
- Disable menu keyboard shortcuts when entering the text
- Moved known bases list to knowledge screen
- Moved known events list to knowledge screen
- Moved accepted missions list to knowledge screen
- Moved know stories list to knowledge screen

### Removed
- Settings to show bases and items info

### Fixed
- Showing keyboard shortcuts in help
- Crash on the new turn
- Crash during boarding when attacker or defender are too tired
- Crash on selling items when no enough profit to pay crew members
- Showing orders keyboard shortcut in Firsts Steps in help
- Crash in combat when enemy crew member has low morale
- Info about bases symbols on map in help
- Look of setting menu shortcuts
- Resizing of fonts in help tags
- Auto-resizing of help window
- Saving the main game window size
- Unmaximize main menu window when quit the game
- Hide close button in combat
- Look of map info window
- Typos in README.md, CONTRIBUTING.md, MODDING.md and help
- Showing orders for the whole crew
- Showing info about the event on map
- Crash on map with starting a story

## [5.3] - 2020-10-04

### Added
- Ability to cancel crafting orders in the selected module
- Crew members lose morale if they don't get their part of trade profit
- Ability to show the home base from ship info

### Changed
- Updated interface
- Better checking for tools for crafting and training
- Better checking for tools during giving orders
- When tired crew member going on rest, returns tools
- When giving orders always take tools from crew members
- Updated help
- Updated README.md
- Moved crew info to ship info screen

### Fixed
- Code documentation
- Showing the player ship's crew info
- Crash on showing the player ship cabin's info when upgrading it
- Crash on showing the player ship crew info
- Order of columns in inventory info
- Showing messages list
- Crash on giving orders to the crew members
- Background color for debug menu
- Showing repair/clean all buttons
- Showing repair/clean orders for each crew member
- Showing info about assigned skill to training room
- Positioning of orders menu
- Showing clean all button when not needed
- Updated assign crew member list when crew member go to another work
- Showing list of items after selling or buying items
- Showing color messages on messages list
- Auto-resizing of last messages widget
- Update crew info in ship info on assign or unassign crew member to module
- Showing module damage in module info
- Showing main menu when the player tries load invalid savegame
- Crew members don't gain experience in beginning of the game
- Don't show crafting order when any workshop don't have set it
- Can't find fonts on Linux
- Crash on non-existing the game theme
- Redraw map after changed size of map font
- Selecting save game from list, load it
- Showing main menu after deleting last savegame
- Crash on trying to show destination menu on start the game
- Resizing header after again start the game

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

## [5.0] - 2020-05-17

### Added
- New goals: visit 10 undead bases, visit 1 drones base, visit 3 drones bases,
  visit 5 drones bases, visit 10 drones bases, visit 1 inquisition base, vitis
  3 inquisition bases, visit 5 inquisition bases, visit 10 inquisition bases,
  destroy 200 poleis ships, destroy 250 poleis ships, destroy 500 poleis
  ships, destroy 1000 poleis ships, destroy 1500 poleis ships, destroy 2000
  poleis ships, destroy 2500 poleis ships, destroy 200 independent ships,
  destroy 250 independent ships, destroy 500 independent ships, destroy 1000
  independent ships, destroy 1500 independent ships, destroy 2000 independent
  ships, destroy 2500 independent ships, kill 800 poleis members in melee
  combat, kill 1000 poleis members in melee combat, kill 2000 poleis members
  in melee combat, kill 2500 poleis members in melee combat, kill 800
  independent members in melee combat, kill 1000 independent members in melee
  combat, kill 2000 independent members in melee combat, kill 2500 independent
  members in melee combat
- New items: bronze printing press, iron printing press, steel printing press,
  titanium printing press, orichalcum printing press and adamantium printing
  press
- New crafting recipes: bronze printing press, iron printing press, steel
  printing press, titanium printing press, orichalcum printing press and
  adamantium printing press

### Changed
- Reduced amount of lost reputation for escaping from bases
- Updated bases data with new items and recipes
- Updated crafting recipes and skills training with tool quality requirement
- Updated help
- Updated interface
- Bases should no longer created at map borders

### Fixed
- Reputation requirement for recipes for steel, titanium, orichalcum and
  adamantium warhammers
- Added missing fonts
- Setting crew members health in debug menu
- Crash when player was killed in debug menu
- Game header info on player death
- Don't show everything on player death
- Crash on start the game
- Showing orders menu after docking
- Saving current configuration for bases prices bonus

## [4.9] - 2020-04-19

### Added
- New items: orichalcum 80mm explosive ammo, orichalcum 100mm piercing ammo,
  orichalcum 100mm explosive ammo, orichalcum 120mm piercing ammo, orichalcum
  120mm explosive ammo, adamantium 80mm piercing ammo, adamantium 80mm
  explosive ammo, adamantium 100mm piercing ammo, adamantium 100mm explosive
  ammo, adamantium 120mm piercing ammo and adamantium 120mm explosive ammo
- New crafting recipes: orichalcum 80mm explosive ammo, orichalcum 100mm
  piercing ammo, orichalcum 100mm explosive ammo, orichalcum 120mm piercing
  ammo, orichalcum 120mm explosive ammo, adamantium 80mm piercing ammo,
  adamantium 80mm explosive ammo, adamantium 100mm piercing ammo, adamantium
  100mm explosive ammo, adamantium 120mm piercing ammo and adamantium 120mm
  explosive ammo
- Version check to the saved games
- Better info about development/stable version of the game
- Better info about invalid saved games
- Better info about why crew member wasn't healed
- New goals: gain max reputation in 1 undead base, gain max reputation in 3
  undead bases, gain max reputation in 5 undead bases, gain max reputation in
  1 drones base, gain max reputation in 3 drones bases, gain max reputation in
  5 drones bases, gain max reputation in 1 inquisition base, gain max
  reputation in 3 inquisition bases, gain max reputation in 5 inquisition
  bases, visit 1 undead base, visit 3 undead bases and visit 5 undead bases
- Info about docking cost before dock to a base
- Ability to escape from bases without paying
- Ability to set tools quality for crafting recipes
- Ability to set training tools quality for skills

### Changed
- Updated bases data with new items and recipes
- Updated help
- Updated debug interface
- Updated items and crafting recipes with reputation requirements
- Selling items which are unavailable due to reputation requirements in bases
  gives more reputation in bases
- Docking fee is taken during undocking from the bases
- Updated MODDING.md

### Fixed
- Possible crash on trading with a friendly trader (issue #67)
- Crash in shipyard when no module is selected for install or remove
- Charcollum to whatever transformation (issue #68)
- Editing crew members stats and skills in debug menu
- Missing icon and title for help window
- Size and position of help and debug windows

## [4.8] - 2020-03-22

### Added
- New items: iron 100mm explosive ammo, iron 120mm piercing ammo, iron 120mm
  explosive ammo, steel 20mm piercing ammo, steel 20mm explosive ammo, steel
  40mm piercing ammo, steel 40mm explosive ammo, steel 60mm piercing ammo,
  steel 60mm explosive ammo, steel 80mm piercing ammo, steel 80mm explosive
  ammo, steel 100mm piercing ammo, steel 100mm explosive ammo, steel 120mm
  piercing ammo, steel 120mm explosive ammo, titanium 40mm piercing ammo,
  titanium 40mm explosive ammo, titanium 60mm piercing ammo, titanium 60mm
  explosive ammo, titanium 80mm piercing ammo, titanium 80mm explosive ammo,
  titanium 100mm piercing ammo, titanium 100mm explosive ammo, titanium 120mm
  piercing ammo, titanium 120mm explosive ammo, orichalcum 60mm piercing ammo,
  orichalcum 60mm explosive ammo and orichalcum 80mm piercing ammo
- New crafting recipes: iron 100mm explosive ammo, iron 120mm piercing ammo,
  iron 120mm explosive ammo, steel 20mm piercing ammo, steel 20mm explosive
  ammo, steel 40mm piercing ammo, steel 40mm explosive ammo, steel 60mm
  piercing ammo, steel 60mm explosive ammo, steel 80mm piercing ammo, steel
  80mm explosive ammo, steel 100mm piercing ammo, steel 100mm explosive ammo,
  steel 120mm piercing ammo, steel 120mm explosive ammo, titanium 40mm
  piercing ammo, titanium 40mm explosive ammo, titanium 60mm piercing ammo,
  titanium 60mm explosive ammo, titanium 80mm piercing ammo, titanium 80mm
  explosive ammo, titanium 100mm piercing ammo, titanium 100mm explosive ammo,
  titanium 120mm piercing ammo, titanium 120mm explosive ammo, orichalcum 60mm
  piercing ammo, orichalcum 60mm explosive ammo and orichalcum 80mm piercing
  ammo
- Reputation in bases affects available modules in shipyards, items to
  trade and crafting recipes to buy

### Changed
- Updated bases data with new items and recipes
- Fire rate of enemies ships: Disarmers shoot less often than others
- Updated MODDING.md
- Updated modules data
- Updated help
- Hide assign ammo button when no ammo is available
- Hide move item option in inventory when crew member don't have any items
  (issue #59)
- Updated help to clarify how 'Visit bases' type of goals works (issue #65)

### Fixed
- Crash when Drone member is trying to eat
- Move map buttons default look
- Crash on upgrading when setting upgrading cost is very low
- Probably fixed possible crash in selling items in bases
- Crash on showing map with non existing event
- Crash when player don't have enough money for pay crew members
- Possible crash on start ship to ship combat
- Crash when healing last crew member in bases
- Crash when trying to talk in bases with someone with Legendary Rhetoric
  skill (issue #60)
- Probably fixed sudden docking after combat (issue #62)
- Setting icon for error dialog
- Crash when trying to sell medicines to diseased base which don't have
  enough money (issue #63)
- Crash in the game statistics when player visits more than 100 bases
  (issue #65)
- Crash on trying install module after selling another (issue #66)

## [4.7] - 2020-02-23

### Added
- Show current ship speed when option "Show numeric values" is enabled
- Info about speed difference between player and enemy ships in combat
- Types of ammunition: Normal, piercing, explosive
- Guns and harpoon guns have size now and require proper size of turret to be
  installed
- New ship modules: huge bronze turret, huge iron turret, huge steel turret,
  huge titanium turret, huge orichalcum turret and huge adamantium turret
- New items: iron 10mm piercing ammo, iron 20mm piercing ammo, iron 20mm
  explosive ammo, iron 40mm piercing ammo, iron 40mm explosive ammo, iron
  60mm piercing ammo, iron 60mm explosive ammo, iron 80mm piercing ammo,
  iron 80mm explosive ammo, iron 100mm piercing ammo
- New crafting recipes: iron 10mm piercing ammo, iron 20mm piercing ammo, iron
  20mm explosive ammo, iron 40mm piercing ammo, iron 40mm explosive ammo, iron
  60mm piercing ammo, iron 60mm explosive ammo, iron 80mm piercing ammo, iron
  80mm explosive ammo, iron 100mm piercing ammo
- Fire rate for ships guns

### Changed
- Updated mobs data
- Updated some ships crews
- Updated MODDING.md
- Updated help
- Renamed Bronze turret, Iron turret, Steel turret and Titanium turret to
  Medium bronze turret, Medium iron turret, Medium steel turret and Medium
  Titanium turret
- Renamed Heavy turrets to Large turrets
- Updated factions data
- Size of turrets and guns
- Updated bases data with new items and recipes

### Fixed
- Crash on generating recruits (issue #46)
- Grammar and sentence structure by ChrisEdBurt (pull request #52)
- Upgrading player's ship hull (issue #51)
- Crash on missing tools in crafting (issue #51)
- Unable to replace hull (issue #53)
- Removing turret crashes game  (issue #55)
- Additional grammar fixes (pull request #56)
- Possible crash when save directory is not writeable
- Fix misspellings and grammar errors. (pull request #58)

## [4.6] - 2020-01-26

### Added
- Ability to set random equipment for mobs
- Ability to set more generic weapon skill for mobs
- Better search for recipes, wounded crew members and modules to repair in
  bases
- Info about used equipment by both crews members during boarding
- New crafting order - deconstruct - allow recover materials from items

### Changed
- Updated mobs data
- When updating base in debug menu, show only available for selected faction
  bases types
- Updated some ships crews
- Updated MODDING.md
- Updated contributing guide
- Updated help
- Desconstruct crafting order name to Study

### Fixed
- Orders priority for one mob
- Possible generate that same name for savegame as old name
- Deleting events in debug menu
- Crash on updating base in debug menu
- Setting base type in debug menu
- Possible crash after combat
- Showing info about finished Studying order
- Showing info about lack of materials for crafting orders
- Showing info about needed workplace

## [4.5] - 2019-12-29

### Added
- New items: huge book of blacksmithing, medium book of woodworking, large
  book of woodworking, huge book of woodworking, medium book of
  leatherworking, large book of leatherworking, huge book of leatherworking,
  bronze training dummy, iron training dummy, steel training dummy, titanium
  training dummy, orichalcum training dummy, adamantium training dummy, bronze
  small traps, iron small traps, steel small traps, titanium small traps,
  orichalcum small traps, adamantium small traps, medium book of printing,
  large book of printing and huge book of printing
- New crafting recipes: huge book of blacksmithing, medium book of
  woodworking, large book of woodworking, huge book of woodworking, medium
  book of leatherworking, large book of leatherworking, huge book of
  leatherworking, bronze training dummy, iron trainning dummy, steel training
  dummy, titanium training dummy, orichalcum training dummy, adamantium
  training dummy, bronze small traps, iron small traps, steel small traps,
  titanium small traps, orichalcum small traps, adamantium small traps, medium
  book of printing, large book of printing and huge book of printing
- Better check for correctness of bases types data
- Orders menu to bases screens (issue #49)
- Button "Buy max" in trading (issue #49)
- Setting for favorite weapon for factions

### Changed
- Reduced chance to damage for Large book of metalsmithing
- Updated Industrial and Factory bases types with new recipes and items
- Updated contributing guide
- Don't show buttons buy/sell all when unneeded (issue #49)
- Reduced chance for random max skills levels for recruits
- Amount of recruit's skills depends on player reputation in base
- Updated help
- Updated factions with favorite weapon skill
- Updated MODDING.md
- Recruits in bases have weapons of own factions
- Recruits equipment quality depends on player reputation in base
- Reduced attributes of recruits in bases
- Updated README.md

### Fixed
- Info about setting difficulty level of upgrades costs
- Adding non existing ships to the game statistics
- Crash on finishing missions in bases
- Hang on starting a new game
- Removed outdated tooltip for recruits list in bases

## [4.4] - 2019-12-01

### Added
- New items: Adamantium small toys, medium book of alchemy, large book of
  alchemy, huge book of alchemy, medium book of cooking, large book of
  cooking, huge book of cooking, medium book of gunsmithing, large book of
  gunsmithing, huge book of gunsmithing, medium book of metalsmithing, large
  book of metalsmithing, huge book of metalsmithing, medium book of medicine,
  large book of medicine, huge book of medicine, medium book of farming, large
  book of farming, huge book of farming, medium book of woodcutting, large
  book of woodcutting, huge book of woodcutting, medium book of brewing, large
  book of brewing, huge book of brewing, medium book of blacksmithing and
  large book of blacksmithing
- New crafting recipes: Adamantium small toys, medium book of alchemy, large
  book of alchemy, huge book of alchemy, medium book of cooking, large book of
  cooking, huge book of cooking, medium book of gunsmithing, large book of
  gunsmithing, huge book of gunsmithing, medium book of metalsmithing, large
  book of metalsmithing, huge book of metalsmithing, medium book of medicine,
  large book of medicine, huge book of medicine, medium book of farming, large
  book of farming, huge book of farming, medium book of woodcutting, large
  book of woodcutting, huge book of woodcutting, medium book of brewing, large
  book of brewing, huge book of brewing, medium book of blacksmithing and
  large book of blacksmithing
- Enemy escape from the combat after some time
- Added type, size, material info to modules list in shipyard when installing
  new modules or remove old ones (issue #49)
- Presetted the game difficulty levels (issue #49)
- Travelling with the mouse (issue #49)

### Changed
- Reduced base price of orichalcum small toys
- Reduced price of orichalcum small toys in Industrial and Factory bases
- Updated Industrial and Factory bases types with new recipes and items
- Updated help
- Updated MODDING.md
- Updated README.md
- Updated contributing guide
- Reduced influence of items prices on recruits prices
- Better info about needed workshop in crafting
- Exit from any station screen, lead to station menu (issue #49)
- Don't reset category/search term when buying or selling (issue #49)

### Fixed
- Crash during hiring new crew members in bases (issue #49)
- Crash when trying to sell medicines to diseased base with full cargo (issue
  #49)
- Hide orders menu when moving ship (issue #49)
- Not working Close button in combat when you enter any other info screen
  (issue #49)
- Crash on map after selling last engine
- Not working "Auto center map after set destination" configuration option

## [4.3] - 2019-11-03

### Added
- Factory base type to the Drones faction
- New base types: Market, Barracks and Agora
- Market base type to the Independent faction
- New flag for bases types: barracks
- New items: bronze cockpit simulator, iron cockpit simulator, steel cockpit
  simulator, titanium cockpit simulator, orichalcum cockpit simulator,
  adamantium cockpit simulator, medium book of engineering, large book of
  engineering, huge book of engineering, bronze gun simulator, iron gun
  simulator, steel gun simulator, titanium gun simulator, orichalcum gun
  simulator, adamantium gun simulator, medium book of rhetoric, large book of
  rhetoric, huge book of rhetoric, bronze small toys, iron small toys, steel
  small toys, titanium small toys and orichalcum small toys
- New crafting recipes: bronze cockpit simulator, iron cockpit simulator,
  steel cockpit simulator, titanium cockpit simulator, orichalcum cockpit
  simulator, adamantium cockpit simulator, medium book of engineering, large
  book of engineering, huge book of engineering, bronze gun simulator, iron
  gun simulator, steel gun simulator, titanium gun simulator, orichalcum gun
  simulator, adamantium gun simulator, medium book of rhetoric, large book of
  rhetoric, huge book of rhetoric, bronze small toys, iron small toys, steel
  small toys, titanium small toys and orichalcum small toys
- Barracks base type to the Undead faction
- Agora base type to the Poleis faction

### Changed
- Pirates, Inquisition, Poleis and Independent factions ships
- Factory base type description
- Updated help
- Updated MODDING.md
- Updated Industrial and Factory bases types with new recipes
- Updated README.md to avoid confusion with `run.sh` script (issue #45)
- Updated README.md
- Updated contributing guide
- Updated factions description with information about unique bases

### Removed
- Industrial and Shipyard bases types from the Drones faction

### Fixed
- Crash when viewing available recruits in bases
- Possible crash during generating new missions in bases
- Crash during crafting (issue #46)
- Showing all bases when Undead faction is selected as default
- Crash on showing battering ram info in Shipyard (issue #47)
- Inventory window doesn't show properly, most of the interface is not drawn,
  flickering on and off (issue #48)

## [4.2] - 2019-10-06

### Added
- New ship modules: huge orichalcum workshop, huge adamantium workshop, medium
  bronze greenhouse, medium iron greenhouse, medium steel greenhouse, medium
  titanium greenhouse, medium orichalcum greenhouse, medium adamantium
  greenhouse, large bronze greenhouse, large iron greenhouse, large steel
  greenhouse, large titanium greenhouse, large orichalcum greenhouse, large
  adamantium greenhouse, huge bronze greenhouse, huge iron greenhouse, huge
  steel greenhouse, huge titanium greenhouse, huge orichalcum greenhouse, huge
  adamantium greenhouse, medium bronze training room, medium iron training
  room, medium steel training room, medium titanium training room, medium
  orichalcum training room, medium adamantium training room, large bronze
  training room, large iron training room, large steel training room, large
  titanium training room, large orichalcum training room, large adamantium
  training room, huge bronze training room, huge iron training room, huge
  steel training room, huge titanium training room, huge orichalcum training
  room, huge adamantium training room, medium bronze medical room, medium
  iron medical room, medium steel medical room, medium titanium medical
  room, medium orichalcum medical room, medium adamantium medical room,
  large bronze medical room, large iron medical room, large steel medical
  room, large titanium medical room, large orichalcum medical room, large
  adamantium medical room, huge bronze medical room, huge iron medical
  room, huge steel medical room, huge titanium medical room, huge orichalcum
  medical room and huge adamantium medical room
- Option to set which bases types are available for each faction
- Option to set chance for spawn for each base type for each faction
- New flags for bases types: temple and blackmarket
- New base types: Temple, Black market and Factory
- Added temple base type to the Inquisition faction
- Added flags shipyard, temple and blackmarket to the help
- Added black market base type to the Pirates faction

### Changed
- Updated MODDING.md
- Changed prices for items in bases on one price in data files
- Updated items, recipes and factions data
- Updated README.md
- Updated contributing guide
- Updated help

### Removed
- Info about which base type sells recipes from recipes data file

### Fixed
- No base type selected in new game setting on first run
- Crash on editing not visited base in debug menu
- Color of agricultural bases on the map
- Parsing factions names for selected flag in help

## [4.1] - 2019-09-08

### Added
- New ship modules: huge steel alchemy lab, huge titanium alchemy lab, huge
  orichalcum alchemy lab, huge adamantium alchemy lab, medium bronze furnace,
  medium iron furnace, medium steel furnace, medium titanium furnace, medium
  orichalcum furnace, medium adamantium furnace, large bronze furnace, large
  iron furnace, large steel furnace, large titanium furnace, large orichalcum
  furnace, large adamantium furnace, huge bronze furnace, huge iron furnace,
  huge steel furnace, huge titanium furnace, huge orichalcum furnace, huge
  adamantium furnace, medium bronze water collector, medium iron water
  collector, medium steel water collector, medium titanium water collector,
  medium orichalcum water collector, medium adamantium water collector, large
  bronze water collector, large iron water collector, large steel water
  collector, large titanium water collector, large orichalcum water collector,
  large adamantium water collector, huge bronze water collector, huge iron
  water collector, huge steel water collector, huge titanium water collector,
  huge orichalcum water collector, huge adamantium water collector, medium
  bronze workshop, medium iron workshop, medium steel workshop, medium
  titanium workshop, medium orichalcum workshop, medium adamantium workshop,
  large bronze workshop, large iron workshop, large steel workshop, large
  titanium workshop, large orichalcum workshop, large adamantium workshop,
  huge bronze workshop, huge iron workshop, huge steel workshop and huge
  titanium workshop
- More random cargo to the enemies ships in combat

### Changed
- Updated README.md
- Updated contributing guide
- Added cargo to traders ships so they will be slower in combat
- Updated interface
- Moved bases types to separated data file

### Fixed
- Crash on returning mission with max reputation as reward
- Patrol missions should be generated only on visited map cells
- Lack of fonts on Windows
- Possible crash on generating missions
- Moved cheapest harpoons buying option to Military bases instead Industrial

## [4.0] - 2019-08-11

### Added
- New ship modules: medium double extended adamantium cabin, medium double
  luxury adamantium cabin, large double basic bronze cabin, large double
  advanced bronze cabin, large double extended bronze cabin, large double
  luxury bronze cabin, large double basic bronze cabin, large double advanced
  iron cabin, large double extended iron cabin, large double luxury iron
  cabin, large double basic steel cabin, large double advanced steel cabin,
  large double extended steel cabin, large double luxury steel cabin, large
  double advanced titanium cabin, large double extended titanium cabin, large
  double luxury titanium cabin, large double advanced orichalcum cabin, large
  double extended orichalcum cabin, large double luxury orichalcum cabin,
  large double extended adamanitum cabin, large double luxury adamantium
  cabin, huge double basic bronze cabin, huge double advanced bronze cabin,
  huge double extended bronze cabin, huge double luxury bronze cabin, huge
  double basic iron cabin, huge double advanced iron cabin, huge double
  extended iron cabin, huge double luxury iron cabin, huge double basic steel
  cabin, huge double advanced steel cabin, huge double extended steel cabin,
  huge double luxury steel cabin, huge double advanced titanium cabin, huge
  double extended titanium cabin, huge double luxury titanium cabin, huge
  double advanced orichalcum cabin, huge double extended orichalcum cabin,
  huge double luxury orichalcum cabin, huge double extended adamantium cabin,
  huge double luxury adamanitum cabin, medium bronze alchemy lab, medium iron
  alchemy lab, medium steel alchemy lab, medium titanium alchemy lab, medium
  orichalcum alchemy lab, medium adamantium alchemy lab, large bronze alchemy
  lab, large iron alchemy lab, large steel alchemy lab, large titanium alchemy
  lab, large orichalcum alchemy lab, large adamantium alchemy lab, huge bronze
  alchemy lab and huge iron alchemy lab
- Info about chance to damage for items
- Check if the game have permission to write to save directory
- Info about strength of ship's weapons, guns ammunitions, harpoons, melee
  weapons and crew members armors
- Option to show numeric values of crew members attributes, skills, etc

### Changed
- Updated help
- Starting ships for player from Undead and Drones factions
- Updated UI
- Updated README.md
- Updated contributing guide
- Default installation directory on Windows
- Updated crafter career with bonus to Printing skill

### Fixed
- Crash after showing info dialog in main menu
- Fonts size after first run of the game
- Buying items when they price is lower than weight
- Not adding training tools to tools category
- Showing ship info icons on Windows
- Crash after asking for the events in bases
- Showing movement buttons on Windows
- Don't show overload information when engineer is resting

## [3.9] - 2019-07-14

### Added
- New ship modules: huge extended bronze cabin, huge luxury bronze cabin, huge
  basic iron cabin, huge advanced iron cabin, huge extended iron cabin, huge
  luxury iron cabin, huge basic steel cabin, huge advanced steel cabin, huge
  extended steel cabin, huge luxury steel cabin, huge advanced titanium cabin,
  huge extended titanium cabin, huge luxury titanium cabin, huge advanced
  orichalcum cabin, huge extended orichalcum cabin, huge luxury orichalcum
  cabin, huge extended adamantium cabin, huge luxury adamantium cabin, small
  double basic bronze cabin, small double advanced bronze cabin, small double
  extended bronze cabin, small double luxury bronze cabin, small double basic
  iron cabin, small double advanced iron cabin, small double extended iron
  cabin, small double luxury iron cabin, small double basic steel cabin, small
  double advanced steel cabin, small double extended steel cabin, small double
  luxury steel cabin, small double advanced titanium cabin, small double
  extented titanium cabin, small double luxury titanium cabin, small double
  advanced orichalcum cabin, small double extended orichalcum cabin, small
  double luxury orichalcum cabin, small double extended adamantium cabin,
  small double luxury adamantiu cabin, medium double basic bronze cabin,
  medium double advanced bronze cabin, medium double extended bronze cabin,
  medium double luxury bronze cabin, medium double iron basic cabin, medium
  double advanced iron cabin, medium double extended iron cabin, medium double
  luxury iron cabin, medium double basic steel cabin, medium double advanced
  steel cabin, medium double extended steel cabin, medium double luxury steel
  cabin, medium double advanced titanium cabin, medium double extended
  titanium cabin, medium double luxury titanium cabin, medium double advanced
  orichalcum cabin, medium double extended orichalcum cabin and medium double
  luxury orichalcum cabin
- Order priority for training
- Option to edit player ship modules in debug menu
- Option to edit amount of money owned by bases in debug menu
- Info about crew to ship info
- Option to see crew, ship and cargo info after death

### Changed
- Updated help
- Updated UI
- Updated README.md

### Fixed
- Completing lists for bases and items names in debug menu
- Some typos in changelog
- Setting size of last messages window
- Debug argument not working with AppImage version
- Running the game after extracting AppImage
- Crash when starting the game in debug mode with not existing save directory
- Crash when base don't have more money to buying something
- Editing bases population and reputation in debug menu
- Setting size of ship info icon
- Infinite resizing of game window on enter crafting info
- Hide other windows on show more move map options
- Info about abandoned bases
- Resizing of the main game window
- Double opening help
- Crew members are going on rest during combat

## [3.8] - 2019-06-16

### Added
- New player's ships for inquisition faction
- Self repair ships during combat for pirates, poleis and independent factions
- Ability to have many owners to cabins and many workers to workshops, training
  rooms and medical rooms
- New ship modules: medium basic bronze cabin, medium advanced bronze cabin,
  medium extended bronze cabin, medium luxury bronze cabin, medium basic iron
  cabin, medium advanced iron cabin, medium extended iron cabin, medium luxury
  iron cabin, medium basic steel cabin, medium advanced steel cabin, medium
  extended steel cabin, medium luxury steel cabin, medium advanced titanium
  cabin, medium extended titanium cabin, medium luxury titanium cabin, medium
  advanced orichalcum cabin, medium extended orichalcum cabin, medium luxury
  orichalcum cabin, medium extended adamanium cabin, medium luxury adamantium
  cabin, large basic bronze cabin, large advanced bronze cabin, large extended
  bronze cabin, large luxury bronze cabin, large basic iron cabin, large
  advanced iron cabin, large extended iron cabin, large luxury iron cabin,
  large basic steel cabin, large advanced steel cabin, large extended steel
  cabin, large luxury steel cabin, large advanced titaniu cabin, large
  extended titanium cabin, large luxury titanium cabin, large advanced
  orichalcum cabin, large extended orichalcum cabin, large luxury orichalcum
  cabin, large extended adamantium cabin, large luxury adamantium cabin, huge
  basic bronze cabin and huge advanced bronze cabin
- Ability to set random career for new game
- Ability to set random faction for new game
- Ability to set random difficulty level for new game
- Ability to make autosaves
- Index to help entries

### Changed
- Updated crews on inquisition ships
- Updated player's character and starting crew for inquisition faction
- Contributing guide
- Maximum allowed owners for cabins and workers for all workshops
- Updated MODDING.md
- Updated interface
- Updated help
- Updated README.md
- Player ship upgrades

### Fixed
- Crews for Advanced poleis patrol ship mk VI, advanced poleis trader mk VI,
  advanced poleis patrol ship mk V, advanced poleis trader mk V, huge poleis
  patrol ship mk V, huge poleis trader mk V, advanced independent patrol ship
  mk VI, advanced independent trader mk VI, advaced independent patrol ship mk
  V, advanced independent trader mk V, huge independent patrol ship mk V, huge
  independent trader mk V, advanced pirates ship mk VI, advanced pirates
  trader mk VI, advanced pirates ship mk V, advanced pirates trader mk V, huge
  pirates ship mk V, huge pirates trader mk V, armored poleis patrol ship,
  armored poleis patrol ship mk II, armored poleis patrol ship mk III, armored
  poleis patrol ship mk IV, armored independent patrol ship, armored
  independent patrol ship mk II, armored independent patrol ship mk III,
  armored independent patrol ship mk IV, armored pirates ship, armored pirates
  ship mk II, armored pirates ship mk III and armored pirates ship mk IV
- Name for small extended adamantium cabin
- Crash on showing passenger info in ship info screen
- Giving orders to passengers
- Send previous trainee on break when new is assigned
- Return training tools back on going on break
- Assigning crew members to medical rooms
- Durability for small advanced steel cabin, small extended steel cabin and
  small luxury steel cabin
- Hide other menus on map when new appear
- Info about upgrading strength of harpoon guns in ship info
- Counting max fuel reduction upgrade for engines
- Help window resizes outside screen
- Crash on repair player ship in bases

## [3.7] - 2019-05-19

### Added
- New items: orichalcum maul, adamantium maul, bronze rapier, iron rapier,
  steel rapier, titanium rapier, orichalcum rapier, adamantium rapier, bronze
  spear, iron spear, steel spear, titanium spear, orichalcum spear and
  adamantium spear
- New crafting recipes: orichalcum maul, adamantium maul, bronze rapier, iron
  rapier, steel rapier, titanium rapier, orichalcum rapier, adamantium rapier,
  bronze spear, iron spear, steel spear, titanium spear, orichalcum spear and
  adamantium spear
- Option to delete events in debug menu
- Option to save game in debug menu
- New skills: rapiers and spears
- Ability to set difficulty levels for new game
- Added few new mobs
- Check for correctness of value ranges in data files
- Warning when fuel/food/drinks can drop below low level on player actions
  (trade, drop, give)
- Stop automovement when fuel/food/drinks drops below low level
- Remembering orders from last combat
- New starting ships for player from pirates faction
- Wake up crew on start combat and don't allow them to go sleep during it

### Changed
- Updated interface
- Always show help window maximized
- Reduced cost of recipes in bases
- Updated poleis, independent, pirates, drones and inquisition factions
  descriptions
- Updated MODDING.md (author: Michael Ax)
- Updated README.md (author: Michael Ax)
- Updated help UI
- Updated player's characters and player's ships for independent faction
- Updated player's characters and starting crew for pirates and drones faction
- Updated help
- Updated crews on poleis, independent, pirates and inquisition ships

### Fixed
- Typos in changelog
- Crash on trying to writing to the closed debug log file
- Checking availability of crafting recipes
- Check for tools for deconstructing items
- Start searching for modules on editing module name
- Missing keyboard shortcuts in wait menu
- Crash on counting max items to trade in bases
- Minimal level of skill after maluses from health, hunger, thirst and fatigue
- Possible crash on talking in bases
- Skills for one of Inquisition pilots
- Setting command line arguments in AppImage running script
- A lot of in-game messages (author: Michael Ax)
- Starting script for Linux (author: Michael Ax)
- Name of Huge pirates trader ship mk II
- Removed unneeded menu from messages and help
- Refresh game header information on drop and give items from ship cargo
- Closing windows with Escape key
- Info about low level of fuel/food/drinks
- Refresh game header information on trading items

## [3.6] - 2019-04-21

### Added
- New items: steel mace, titanium mace, orichalcum mace, adamantium mace,
  bronze dagger, iron dagger, steel dagger, titanium dagger, orichalcum
  dagger, adamantium dagger, bronze greatsword, iron greatsword, steel
  greatsword, titanium greatsword, orichalcum greatsword, adamantium
  greatsword, bronze warhammer, iron warhammer, steel hammer, titanium
  warhammer, orichalcum warhammer, adamantium hammer, bronze maul, iron maul,
  steel maul and titanium maul
- New crafting recipes: steel mace, titanium mace, orichalcum mace, adamantium
  mace, bronze dagger, iron dagger, steel dagger, titanium dagger, orichalcum
  dagger, adamantium dagger, bronze greatsword, iron greatsword, steel
  greatsword, titanium greatsword, orichalcum greatsword, adamantium
  greatsword, bronze warhammer, iron warhammer, steel warhammer, titanium
  warhammer, orichalcum warhammer, adamantium hammer, bronze maul, iron maul,
  steel maul and titanium maul
- Bigger chance to lost reputation in base after destroying friendly ship
- Better counting for info about time and fuel needed for reach destination on
  map
- Better counting amount of items for sale and buy
- New skills: Daggers, Two-handed swords and Two-handed maces
- Crew members lose morale with high fatigue
- Option to negotiate reward during accepting missions (money or reputation)
- Debug menu for modifying the game data

### Changed
- Default icon for unvisited bases
- Updated help
- Crash on healing wounded crew members in bases
- Items stats: bronze mace and iron mace
- Crafting recipes: bronze mace and iron mace
- Updated interface
- Faster gaining reputation in bases
- Even slower losing morale for factions with fanaticism
- Reduced size of save game files
- Transport passenger missions depends on cabins quality not a cabin type
- Reduced experience needed for gaining levels in skills and attributes
- Updated MODDING.md

### Fixed
- Clearing search field in trade after buy or sell items
- Don't show Sell All option when possible amount to sell is smaller than
  owned amount
- Don't show that crafting recipe is available if there are not enough
  crafting materials
- Check for having needed materials during crafting
- Show invalid path to error.log file in error report
- Crash on accepting transport passenger missions
- Crash on lack of passenger's attributes
- Crash on very low starting morale of transported passenger
- Crash on showing info about passenger in crew view
- Crash on finishing transport passenger missions
- Possible crash on counting current skill level for crew members
- Crash on dismissing passengers
- Waiting X minutes option don't use fuel
- Crash on putting on items in inventory

## [3.5] - 2019-03-24

### Added
- New ships: Advanced huge poleis trader mk II, advanced huge independent
  trader mk II, advanced huge pirates trader mk II, advanced huge poleis
  patrol ship mk II, advanced huge independent patrol ship mk II, advanced
  huge poleis trader mk III, advanced huge independent trader mk III, advanced
  huge pirates trader mk III, advanced huge poleis patrol ship mk III,
  advanved huge independent patrol ship mk III, advanced huge poleis trader mk
  IV, advanced huge independent trader mk IV, advanced huge pirate trader mk
  IV, advanced huge poleis patrol ship mk IV, advanced huge independent patrol
  ship mk IV, advanced huge poleis trader mk V, advanced huge independent
  trader mk V, advanced huge pirate trader mk V, advanced huge poleis patrol
  ship mk V, advanced huge independent patrol ship mk V, advanced huge poleis
  trader mk VI, advanced huge independent trader mk VI, advanced huge pirates
  trader mk VI, advanced huge poleis patrol ship mk VI and advanced huge
  independent patrol ship mk VI
- Option to show or hide last messages window
- Option to set the game in full screen mode
- Self-closing message boxes and option to set delay for self-closing
- Option to set check boxes looks in themes
- Each battering ram attack cause unarmed enemy to stop for 1 combat round
- New factions flags: Naturalarmor, toxicattack, sentientships, fanaticism and
  loner
- Natural armor ability for Undead and Drones faction members
- Toxic attack ability for Undead
- Sentient ships ability for Drones
- Fanaticism ability for Inquisition
- Bonus to damage from unarmed skill
- New skill: Maces
- New items: bronze mace and iron mace
- New crafting recipes: bronze mace and iron mace
- Loner ability to Independent and Pirates
- Better and worse food and drinks affect crew members morale

### Changed
- Updated interface
- Damage of battering rams depends on ship mass and speed too
- Updated help
- Battering rams attack once per 2 combat rounds
- Updated MODDING.md
- Raised base strength and lowered other stats for Undead player and crew.
- Group sky bases by factions during generating world

### Removed
- Configuration to show or hide last message

### Fixed
- Typos in changelog
- Default keyboard shortcut for show more move map options
- Missing AppImage .desktop file entries by probonopd #43
- Typo in README.md
- Showing new and load game buttons when data files are not loaded
- Showing info about overloading when pilot is on rest
- Enabling or disabling tooltips
- Using/taking off items in crew member inventory
- Loading inventories and cargo from data files
- Crash in combat when no gunner in player ship
- Crash on adding corpse to ships
- Harpoon gun work when enemy have an armor
- Crash on trying to trade with friendly ship
- Selling items to friendly ships
- Selling items resets item's profit info
- Crash on dismissing crew members
- Crash on delivering medicines to diseased base
- Crash on repair ship in bases
- Weapons on some ships

## [3.4] - 2019-02-24

### Added
- New ships: Armored independent patrol ship mk II, armored poleis patrol
  ship mk III, armored independent patrol ship mk III, armored poleis patrol
  ship mk IV, armored independent patrol ship mk IV, advanced poleis patrol
  ship mk II, advanced independent patrol ship mk II, advanced poleis patrol
  ship mk III, advanced independent patrol ship mk III, advanced poleis patrol
  ship mk IV, advanced independent patrol ship mk IV, large poleis trader mk
  II, large independent trader mk II, large pirates trader mk II, large poleis
  patrol ship mk II, large independent patrol ship mk II, large poleis trader
  mk III, large independent trader mk III, large pirates trader ship mk III,
  large poleis patrol ship mk III, large independent patrol ship mk III, large
  poleis trader mk IV, large independent trader mk IV, large pirates trader mk
  IV, large poleis patrol ship mk IV, large independent patrol ship mk IV,
  large poleis trader mk V, large independent trader mk V, large pirates
  trader mk V, large poleis patrol ship mk V, large independent patrol ship
  mk V, huge poleis trader mk II, huge independent trader mk II, huge pirates
  trader mk II, huge poleis patrol ship mk II, huge independent patrol ship mk
  II, huge poleis trader mk III, huge independent trader mk III, huge pirates
  trader mk III, huge poleis patrol ship mk III, huge independent patrol ship
  mk III, huge poleis trader mk IV, huge independent trader mk IV, huge pirates
  trader ship mk IV, huge poleis patrol ship mk IV, huge independent patrol
  ship mk IV, huge poleis trader mk V, huge independent trader mk V, huge
  pirates trader mk V, huge poleis patrol ship mk V, huge independent patrol
  ship mk V, huge poleis trader mk VI, huge independent trader mk VI, huge
  pirates trader mk VI, huge poleis patrol ship mk VI and huge independent
  patrol ship mk VI
- Show README.md file in game about menu
- Added default icon for unvisited bases to themes
- Added default icon for player ship to themes
- Added default icon for empty space on map to themes
- Added default icon for player ship destination on map to themes
- Added default icon for story event location on map to themes
- Added default icon for showing ship overload warning to themes
- More keyboard shortcuts to move map and option to set them
- Don't allow to undock from bases if player ship is overloaded
- Keyboard shortcuts to change player ship speed

### Changed
- Made starting ships for Undead player a bit stronger (better battering ram
  and armor)
- Default font for map
- Updated README.md
- Updated MODDING.md
- Reduced fuel usage during waiting in place outside bases
- Updated interface
- Raised limit of minimal ship speed
- Updated help

### Removed
- Configuration to show or hide move map buttons

### Fixed
- Crash on selling ship modules in shipyard
- Crash when waiting in place
- Modules in ships: Poleis trader mk IV, pirates trader mk IV, independent
  trader mk IV, poleis patrol ship mk IV and independent patrol ship mk IV
- Crash when checking cleanliness of cabins
- Crash on cleaning ship
- Crash on crew member back to work after rest
- Checking did player ship have enabled engines during changing ship speed
- Ship overloading
- Assigning crew member to harpoon gun
- Generating ship names
- Possible crash when showing the game map
- Crash when undocking speed is set to full stop
- Setting custom location for themes directory

## [3.3] 2019-01-27

### Added
- Option to enable or disable tooltips
- New ships: tiny pirates trader, small pirates trader, pirates trader, large
  pirates trader, huge pirates trader, advanced huge pirates trader, tiny
  poleis trader mk II, tiny independent trader mk II, tiny pirates trader mk
  II, tiny poleis patrol ship mk II, tiny independent patrol ship mk II, small
  poleis trader mk II, small independent trader mk II, small pirates trader mk
  II, small poleis patrol ship mk II, small independent patrol ship mk II,
  small poleis trader mk III, small independent trader mk III, small pirates
  trader mk III, small poleis patrol ship mk III, small independent patrol
  ship mk III, poleis trader mk II, independent trader mk II, pirates trader
  mk II, poleis patrol ship mk II, independent patrol ship mk II, poleis
  trader mk III, independent trader mk III, pirates trader mk III, poleis
  patrol ship mk III, indepdenent patrol ship mk III, poleis trader mk IV,
  independent trader mk IV, pirates trader mk IV, poleis patrol ship mk IV,
  independent patrol ship mk IV and armored poleis patrol ship mk II
- Missing message when enemy ship intercept player
- Lack of food or drinks reduce morale of crew members
- Separated icon on map for bases for each faction
- Separated icon on map for each event type
- Separated icon on map for each mission type
- Configuration files for custom themes

### Changed
- Updated cargo bays for some ships
- Updated interface
- Trader talking skill and reputation affects award for completed missions
- Updated help
- Don't add enemy traders to attack on base and enemy patrol events
- Updated README.md
- Factions data with new bases icons
- Updated MODDING.md
- Each custom theme must be in separated directory inside themes directory

### Fixed
- Hide close button when showing stories
- Color of message when player can't dock to base because its docks are full
- Missing menu entries after selecting destination for ship
- Crash on failing mission
- Missing info about lack of tools when giving orders to whole crew
- Setting fonts size
- UI/UX: Selecting character goal behaves different than other menuses
  (issue #35)
- Auto-resizing window makes it hard to exit character creation screen in
  specific curcumstances (issue #36)
- Name of faction in destroy ship goals
- Deleting mob by modification files
- Typo in character creation screen - "Carrer" instead of "Career" (issue #40)
- RFE: Allow to scroll box info (during character creation) using keyboard
  (issue #41)

## [3.2] - 2018-12-30

### Added
- New ship modules: medium steel cargo bay, medium titanium cargo bay, medium
  orichalcum cargo bay, medium adamantium cargo bay, large bronze cargo bay,
  large iron cargo bay, large steel cargo bay, large titanium cargo bay, large
  orichalcum cargo bay, large adamantium cargo bay, huge bronze cargo bay, huge
  iron cargo bay, huge steel cargo bay, huge titanium cargo bay, huge
  orichalcum cargo bay and huge adamantium cargo bay
- Option to center map on home base
- Keyboard shortcut (and option to set it) for center map on home base
- Maximum allowed size of modules to ship hulls
- Missing close button to goals list
- Missing tooltips for directories info in options
- Last bought price to items to show profit in trade
- Option to enable or disable last message info

### Changed
- Updated README.md
- Updated interface
- Updated ships hulls data with maximum allowed size of installed modules
- Updated help
- Updated MODDING.md
- Updated new game message
- Updated cargo bays for some ships
- Reduced chance to meet strongest enemies on weaker ships
- More dynamic changes of morale

### Fixed
- Info about item damage
- Unnecessary changes of name of save game file
- Crash on item damage when there is more than one that same item
- Saving default settings for player faction and career for new game
- Size of website button
- Showing info about available ammo for guns in combat
- Showing only visited bases of selected faction on bases list
- Crash after recruit new crew member in bases
- Auto back to map when all recruits are hired
- Resetting hire negotiations
- Info about available missions after accepting mission
- Player ship for general career in pirates faction
- Losing morale when crew member can't go sleep
- Warning in console when giving items to crew members
- Crash when player don't have enough money for paying crew members
- Color of message when player don't have moneys for pay for docking
- Show orders menu outside map

## [3.1] - 2018-12-02

### Added
- New ship modules: Advanced huge adamantium hull, huge adamantium engine,
  advanced huge adamantium engine, adamantium harpoon gun, adamantium 80mm gun,
  adamantium 100mm gun, adamantium 120mm gun, small adamantium workshop, medium
  bronze cargo bay and medium iron cargo bay
- Options to update game data (ships, mobs, factions, etc) by modification files
- New ships: huge pirate ship mk VI, advanced huge pirate ship mk VI, huge
  undead ship mk VI, advanced huge undead ship mk VI, huge attacking drone mk
  VI, advanced huge attacking drone mk VI, huge inquisition ship mk VI,
  advanced huge inquisition ship mk VI
- Few new mobs for ships crews
- Option to set starting base for new game

### Changed
- Raised maximum amount of modules for huge adamantium hull
- Raised default durability of huge orichalcum engine and orichalcum harpoon
  gun
- Updated README.md
- Removing game data
- Updated MODDING.md
- Final enemy for story with undead
- Updated interface
- Crews for advanced huge inquisition ship mk V, huge inquisition ship mk V,
  advanced huge undead ship mk V, huge undead ship mk V, advanced huge pirate
  ship mk V and huge pirate ship mk V

### Fixed
- Some typos in old changelog
- Removing mobs from game data
- Chance for damage for items during usage
- Going on break after finishing crafting
- Log entry about removed mob
- Scrolling messages lists to last messsage
- Even more autoresize GUI elements on mouse hover (issue #31)
- Dying in combat from lack of fuel
- Index for small steel training room
- Crash on showing know events list
- Crash on asking friendly ships for bases and events
- Saving crew members faction and homebase
- Crash in combat on destroying module in player ship
- Repair material for titanium armor
- Setting player ship speed after combat
- Entering text cause activation of menu
- Entering text cause activation of menu in numeric fields
- Close windows when entering texts
- Entering text cause activation of shortcuts in options
- Setting key for move ship left
- Showing info about player reputation in bases
- Close wait orders and move map windows when entering text
- Crash during combat on showing messages

## [3.0] - 2018-11-04

### Added
- New crafting recipes: small book of alchemy, small book of cooking, small
  book of gunsmithing, small book of metalsmithing, small book of medicine,
  small book of farming, small book of woodcutting, small book of brewing,
  small book of blacksmithing, small book of woodworking, small book of
  leatherworking, wooden training dummy, small wooden traps, small book of
  printing and wooden printing set
- Welcoming message to each new game
- Option to set order of displayed messages
- Option to zoom in and zoom out map with mouse wheel and keyboard shortcuts
- Variables for keyboard shortcuts for zooming map
- Option to auto ask for bases and events when ship dock to base
- New ship modules: small orichalcum workshop, small adamantium training room,
  adamantium cockpit, small adamantium alchemy lab, small adamantium cargo bay,
  small adamantium furnace, heavy adamantium armor, small adamantium
  greenhouse, small adamantium water collector, small adamantium medical room,
  small extended adamantium cabin, small luxury adamantium cabin, heavy
  adamantium turret, heavy adamantium battering ram and huge adamantium hull
- Option to show or hide buttons for map movement

### Changed
- Updated help
- Updated interface
- Redesigned combat UI (issue #30)
- Updated README.md
- Updated MODDING.md
- Don't allow to disable last engine even when player ship is docked to base
- Updated CONTRIBUTING.md

### Fixed
- Showing information about reputation in base in bases list
- Reading default player gender from new game configuration
- Autoresize GUI elements on mouse hover (issue #31)
- Typo in finished crafting message
- Return to old speed after finished combat
- Assigning guns to turrets during creating ships
- Starting morale and loyalty for recruits
- Generating enemy ships for missions
- Don't show new game and load game buttons when there is error in loading game
  data
- Removing available careers
- Index for bronze sickle recipe
- Updating crew inventory
- Possibility of disease event in bases
- Reading bases population from saves

## [2.9] - 2018-10-07

### Added
- New crafting recipes: Adamantium short sword, adamantium sword, adamantium
  shield, adamantium chestplate, adamantium helmet, adamantium armsguard,
  adamantium legsguard, wooden cockpit simulator, sheet of paper, empty book,
  small book of engineering, wooden gun simulator, small book of rethoric and
  wooden small toys
- New items: Adamantium sword, adamantium shield, adamantium chestplate,
  wooden cockpit simulator, adamantium helmet, small book of engineering,
  wooden gun simulator, adamantium armsguard, small book of rhetoric, wooden
  small toys, adamantium legsguard, small book of alchemy, small book of
  cooking, small book of gunsmithing, small book of metalsmithing, small book
  of medicine, small book of farming, small books of woodcutting, small book
  of brewing, small book of blacksmithing, small book of woodworking, small
  book of leatherworking, wooden training dummy, small wooden traps, small
  book of printing, wooden printing set, sheet of paper and empty book
- Ability to show current game directories in options
- Optional names for careers in factions
- New items types: cockpitsimulator, engineersbook, gunsimulator,
  rhetoricbook, smalltoys, alchemybook, cookingbook, gunsmithbook,
  metalsmithbook, medicinebook, farmingbook, woodcuttingbook, brewingbook,
  blacksmithbook, woodworkingbook, leatherworkingbook, trainingdummy,
  smalltraps, printingbook, printingset, paper and emptybook
- Option to set fonts size in game
- User Interface themes
- New skill: Printing
- Recruits skills and attributes depends on player reputation in base
- Size to bases
- Ability to sort bases in bases list by population
- Waiting outside skybases uses fuel too

### Changed
- Help factions flags variables now return list of factions which have that
  flag enabled
- Update help
- Updated MODDING.md
- Updated interface
- Moved player's careers to separated data file
- Training skills on ship require tools
- How chance for base owner is calculated
- Updated README.md

### Fixed
- Reclaiming turrets and workshops by crew members after back to work after
  rest
- Info about returning mission in bases info
- Armor stat of orichalcum shield
- Typos in careers descriptions
- Possible memory leaks during loading game data
- Keyboard shortcuts for moving map

## [2.8] - 2018-09-09

### Added
- New crafting recipes: Adamantium mold, adamantium gunsmith set, adamantium
  cooking set, adamantium sickle, adamantium saw, adamantium bucket, adamantium
  blacksmith set, adamantium woodworker set, adamantium repair tools,
  adamantium sewing kit, adamantium 80mm ammo, adamantium 100mm ammo,
  adamantium 120mm ammo and adamantium harpoon
- New items: Adamantium gunsmith set, adamantium cooking set, adamantium
  sickle, adamantium saw, adamantium bucket, adamantium blacksmith set,
  adamantium woodworker set, adamantium repair tools, adamantium sewing kit,
  adamantium 80mm ammo, adamantium 100mm ammo, adamantium 120mm ammo,
  adamantium harpoon and adamantium short sword
- Separated healing tools for each faction
- Variable with healing tool name to help
- Separated healing skill for each faction
- Ability to disable genders for selected factions
- Ability to made selected factions immune to diseases
- Ability to disable tiredness for selected factions
- Ability to disable morale for selected factions
- Separated directory for game modifications and ability to set it by starting
  arguments
- Option to show help depending on player faction
- Option to read game main data from different files than game.dat
- High morale adds small bonus to skills
- Option to remove existing recipes, factions, goals, help entries, items,
  ship modules, ships and stories from modifications files
- Ability to select player career which affect player starting ship and
  equipment
- New ships: Player starting ships for trader, hunter, explorer and crafter
  careers
- Bonus to gained experience based on career selected by player
- Each crew members, recruit, passenger have home skybase
- Hide from player factions without careers
- Separated faction for each mobile

### Changed
- Updated interface
- Updated MODDING.md
- Updated help
- Drones faction don't have morale, tiredness, genders and is immune to
  diseases
- Undead faction don't have morale, tiredness and is immune to diseases
- Moved all data files to one directory
- Raised chance for sky bases from factions other than Poleis
- Updated README.md
- Reduced weight of blacksmith sets
- How morale is gained and lost
- Blocked gaining experience by pilot, engineer and gunner when ship is docked
  to base
- Only resting crew members can be wounded in bar brawl event

### Fixed
- Recipe for adamantium plates
- Crash when opening game stats with finished goal
- Searching for food and drinks in crew members inventory too
- Showing info about thirst/hunger/tiredness of crew members
- Showing info about lack of workshop in crafting menu
- Finding fonts on Windows
- Don't show Menu button in game statistics after player death
- Waiting 1 minute button
- Possible crash when reporting error
- Lack of keyboard shortcut for back button in license window

### Removed
- HealingSkill variable from help

## [2.7] - 2018-08-12

### Added
- New ship modules: huge orichalcum hull, advanced huge orichalcum hull, huge
  orichalcum engine, advanced huge orichalcum engine, orichalcum harpoon gun,
  orichalcum 60mm gun, orichalcum 80mm gun, orichalcum 100mm gun and orichalcum
  120mm gun
- Finishing missions raise or lower (if failed) player character morale
- New enemy ships: large clockwork drone mk V, large pirate ship mk V, large
  undead ship mk V, large inquisition ship mk V, large attacking drone mk V,
  huge pirate ship mk V, advanced huge pirate ship mk V, huge undead ship mk V,
  advanced huge undead ship mk V, huge attacking drone mk V, advanced huge
  attacking drone mk V, huge inquisition ship mk V and advanced huge
  inquisition ship mk V
- More text formatting options to help
- Option to stories to set which factions are not allowed to start selected
  story
- Option to enable or disable ship engines
- Showing current unarmed skill name in help
- Separated player starting character for each faction
- Starting player character data for undead and drones factions
- Starting crew data for undead, drones and inquisition factions
- Separated player starting ship for each faction
- New ship: Player starting ship for pirates, undead, drones and inquisition
  factions
- Description to factions
- New item types: Adamantiumore and adamantium
- Option to select player faction at starting new game
- New items: Adamantium ore, adamantium plates and adamantium mold
- Separated food and drinks types for each faction
- New crafting recipe: Adamantium plates

### Changed
- Updated interface
- Updated README.md
- Base name of savegame files
- Updated help
- Each faction have separated reputation with each other
- Updated MODDING.md
- Separated friendly/enemy relations between factions
- Updated game data
- Final enemy for story with undead
- Raised chance for sky bases from factions other than Poleis

### Fixed
- Keyboard shortcuts for some orders
- Updating crew orders after combat
- Info about module damage on ship info screen
- Refreshing combat UI after change crew orders on crew screen
- Showing info about events in bases
- Crash on start game
- Renaming ship in game not change save game file name
- Showing upgrade button when upgrade is unavailable for that ship module
- Showing operate harpoon gun in crew members orders list
- Running game on Linux

### Removed
- HealingTool variable from help

## [2.6] - 2018-07-15

### Added
- New goals: kill 800 inquisitors in melee combat, kill 1000 inquisitors in
  melee combat, kill 2000 inquisitors in melee combat, kill 4000 inquisitors
  in melee combat, kill 6000 inquisitors in melee combat, kill 8000 inquisitors
  in melee combat and kill 10000 inquisitors in melee combat
- Keyboard shortcut (and option to set it) to center map on player ship
- Morale to crew members
- Loyalty to crew members
- New type of ship modules: training room
- New ship modules: small bronze training room, small iron training room, small
  steel training room, small titanium training room, small orichalcum training
  room, orichalcum cockpit, small orichalcum alchemy lab, small orichalcum
  cargo bay, large orichalcum engine, large advanced orichalcum engine, small
  orichalcum furnace, large orichalcum hull, advanced large orichalcum hull,
  heavy orichalcum armor, small orichalcum greenhouse, small orichalcum water
  collector, small orichalcum medical room, small advanced orichalcum cabin,
  small extended orichalcum cabin, small luxury orichalcum cabin, heavy
  orichalcum turret and heavy orichalcum battering ram
- Ability to train skills on ship
- Keyboard shortcuts (and option to set them) to move map
- New item type: storyitem
- New items: strange medallion and strange metal scrap
- Keyboard shortcuts (and option to set them) to move cursor around map
- Keyboard shortcut (and option to set it) for selecting map field
- Randomly generated stories
- Option to set GTK share directory via console parameter (Linux only)
- Ability to have few saved games at once

### Changed
- Updated help
- Updated interface
- Updated MODDING.md
- Updated README.md

### Fixed
- Resting during automovement
- Adding keyboards shortcuts with special keys
- Loading missions data from saved game
- Mouse click on map field with player ship should bring orders menu
- Docking to bases where player have very low reputation
- Saving reputation in bases
- Crash on giving invalid order to crew member
- Showing buy option when trader don't have that item
- Moving map near borders
- Crash during selecting event as destination for player ship
- Showing complete mission button on wrong bases
- Keyboard shortcuts for some orders
- Don't show item amount in base/trader ship if player can't buy them
- Possible crash on deleting messages

## [2.5] - 2018-06-17

### Added
- New items: orichalcum chestplate, orichalcum helmet, orichalcum armsguard and
  orichalcum legsguard
- New crafting recipes: orichalcum chestplate, orichalcum helmet, orichalcum
  armsguard and orichalcum legsguard
- Option to set GTK directories via console parameters (Linux only)
- New goals: kill 1000 enemies in melee combat, kill 2000 enemies in melee
  combat, kill 4000 enemies in melee combat, kill 6000 enemies in melee combat,
  kill 8000 enemies in melee combat, kill 10000 enemies in melee combat, kill
  800 pirates in melee combat, kill 1000 pirates in melee combat, kill 2000
  pirates in melee combat, kill 4000 pirates in melee combat, kill 6000 pirates
  in melee combat, kill 8000 pirates in melee combat, kill 10000 pirates in
  melee combat, kill 800 undead in melee combat, kill 1000 undead in melee
  combat, kill 2000 undead in melee combat, kill 4000 undead in melee combat,
  kill 6000 undead in melee combat, kill 8000 undead in melee combat and kill
  10000 undead in melee combat
- Option to set player faction
- Random equipment to recruits in bases
- Payment for crew members
- Negotiating hiring of recruits in bases
- Time-based contracts with crew members

### Changed
- Raised gained experience in combat
- Updated interface
- Updated README.md
- New format of game data file which made old incompatible
- Updated MODDING.md
- New format of hall of fame data file which made old incompatible
- New format of items data file which made old incompatible
- New format of ships modules data file which made old incompatible
- New format of recipes data file which made old incompatible
- New format of mobiles data file which made old incompatible
- New format of ships data file which made old incompatible
- New format of goals data file which made old incompatible
- New format of help data file which made old incompatible
- Moved NPC factions data to data file
- Updated help

### Fixed
- Crash on showing map cell info
- Some typos in changelog
- Loading player ship cargo from savegame file
- Default keyboard shortcuts for ship movement
- Crash on moving map
- Crash on player death
- Hide map after player death
- Showing death screen after lost ship fight
- Deleting savegame when it is saved in different directory than default
- Possible crash on saving hall of fame data
- Updating crew orders in combat
- Showing buying option for unavailable items
- Loading saved game statistics
- Loading player ship data
- Setting set mission as destination for ship button
- Crash when trying to remove destroyed ship module
- Keyboard shortcut for shipyard in orders menu
- Again, lots of grammar/spelling errors by LJNIC (pull request #25)
- Ability to dock when base docks are full
- Showing info about unknown bases on map
- Showing help button in crew member inventory
- Counting hire price for recruits
- Lots of grammar/spelling errors in README.md by MagiOC (pull request #26)
- Crash on repair whole ship in bases
- Showing Wait order when there are full docks in base
- Refreshing map after stop automove due to lack of crew members on position

### Removed
- Abandoned faction

## [2.4] - 2018-05-20

### Added
- New items: titanium woodworker set, orichalcum woodworker set, titanium
  repair tools, orichalcum repair tools, titanium sewing kit, orichalcum
  sewing kit, orichalcum 60mm ammo, orichalcum 80mm ammo, orichalcum 100mm
  ammo, orichalcum 120mm ammo, orichalcum harpoon, orichalcum short sword,
  orichalcum sword and orichalcum shield
- New crafting recipes: titanium woodworker set, orichalcum woodworker set,
  titanium repair tools, orichalcum repair tools, titanium sewing kit,
  orichalcum sewing kit, orichalcum 60mm ammo, orichalcum 80mm ammo,
  orichalcum 100mm ammo, orichalcum 120mm ammo, orichalcum harpoon, orichalcum
  short sword, orichalcum sword and orichalcum shield
- Showing license full text in info about game
- New sky base type: military
- Damage type for personal weapons
- Option to search through messages
- More info about player death in melee combat
- Option to set max amount of stored messages in game
- Option to set max amount of saved messages
- Statistics for killed enemies in melee combat
- New type of goal: kill X enemies in melee combat
- New goal: kill 800 enemies in melee combat

### Changed
- Updated interface
- Updated README.md
- Updated items data
- Updated MODDING.md
- Updated recipes data
- Updated help
- New format of save games which made saves from previous versions incompatible

### Fixed
- Double attack in melee combat after kill
- Generating cargo in first time visited bases
- Crash in repairing ship in bases
- Crash in buying items in bases
- Crash in combat on death of crew member
- Assigning gunners to harpoon guns in enemies ships
- Typo in old changelog file
- Shooting by enemy when no gunner at gun
- Showing game statistics after player death
- Showing last message after buying recipes/ship repairs/crew healing in bases
- Don't show empty know events list
- Auto back to sky map after buying all recipes in base
- Refreshing combat view after player back on ship
- Melee combat stops when no harpoons attached to ships
- Showing empty goals category in goals select menu
- Showing sky map after starting/loading game
- Block ability to back on ship during combat if no harpoon active
- Checking for needed materials before start crafting
- Don't allow to go on rests crew members in boarding party when ships are not
  connected by harpoons
- Issue #17 - Crash when boarding party member don't have weapon
- Showing information about crew member order when he/she boarding enemy ship
- Probably fixed issue #18 - Crash when crew member have more than maximum health
- Gaining experience by crew members during boarding enemy ship
- Issue #20 - Crash when attacking with ram
- Issue #19 - Crash when crew member have non-existent item equipped

## [2.3] - 2018-04-22

### Added
- New items: titanium mold, orichalcum mold, titanium gunsmith set, orichalcum
  gunsmith set, titanium cooking set, orichalcum cooking set, titanium sickle,
  orichalcum sickle, titanium saw, orichalcum saw, titanium bucket, orichalcum
  bucket, titanium blacksmith set and orichalcum blacksmith set
- New crafting recipes: titanium mold, orichalcum mold, titanium gunsmith set,
  orichalcum gunsmith set, titanium cooking set, orichalcum cooking set,
  titanium sickle, orichalcum sickle, titanium saw, orichalcum saw, titanium
  bucket, orichalcum bucket, titanium blacksmith set and orichalcum blacksmith
  set
- Ability to enable/disable interface animations
- Ability to set type of interface animations
- Descriptions for skills and stats
- Option to set how much hands weapons needs to use (one-handed, two-handed)
- New orders priorities: for defending ship and boarding enemy ship which made
  saves from previous version incompatible
- Ability to board player ship by enemies
- Option to give orders to boarding party (if player is in this party)

### Changed
- Updated interface
- Updated game data
- Updated MODDING.md
- Changed item type MeleeWeapon to Weapon
- Updated data for weapons
- Updated data for mobs
- Updated help

### Fixed
- Lots of grammar/spelling errors by LJNIC (pull request #16)
- Clearing combat orders after combat

## [2.2] - 2018-03-25

### Added
- New enemy ships: Armored pirate ship mk IV, attacking clockwork drone mk IV,
  armored attacking drone mk IV, inquisition ship mk IV, armored inquisition
  ship mk IV, large clockwork drone mk IV, large pirate ship mk IV, undead ship
  mk IV, large undead ship mk IV, large inquisition ship mk IV, large attacking
  drone mk IV, advanced attacking drone mk IV, advanced pirate ship mk IV,
  advanced undead ship mk IV, advanced inquisition ship mk IV, huge pirate ship
  mk IV, advanced huge pirate ship mk IV, huge undead ship mk IV, huge
  attacking drone mk IV, advanced huge undead ship mk IV, advanced huge
  attacking drone mk IV, huge inquisition ship mk IV and advanced huge
  inquisition ship mk IV
- Keyboard shortcuts for ship movement and menu
- Option to set keyboard shortcuts for ship movement and menu
- New item types: orichalcumore and orichalcum
- New items: orichalcum ore and orichalcum plates
- New crafting recipe: orichalcum plates

### Changed
- Updated MODDING.md
- Updated help
- Updated interface
- Updated README.md

### Fixed
- Movement buttons after set destination for ship
- Moving ship to destination point
- Refreshing move buttons after finish mission
- Crash on showing available missions
- Crash when generate bases cargo
- Showing repair ship option in small and medium bases
- Showing combat after show info windows
- Crash on setting module upgrade when no upgrading materials are available

## [2.1] - 2018-02-25

### Added
- New ship modules: titanium armor, heavy titanium armor, titanium turret,
  small titanium greenhouse, small titanium water collector, small titanium
  medical room, advanced titanium cabin, extended titanium cabin, luxury
  titanium cabin, heavy titanium turret, heavy titanium battering ram, small
  titanium workshop, huge titanium hull, advanced huge titanium hull, huge
  titanium engine, advanced huge titanium engine, titanium harpoon gun,
  titanium 40mm gun, titanium 60mm gun, titanium 80mm gun, titanium 100mm gun
  and titanium 120mm gun
- New items: titanium 40mm ammo, titanium 60mm ammo, titanium 80mm ammo,
  titanium 100mm ammo, titanium 120mm ammo, titanium harpoon, titanium short
  sword, titanium sword, titanium shield, titanium chestplate, titanium helmet,
  titanium armsguard and titanium legsguard
- New crafting recipes: titanium 40mm ammo, titanium 60mm ammo, titanium 80mm
  ammo, titanium 100mm ammo, titanium 120mm ammo, titanium harpoon, titanium
  short sword, titanium sword, titanium shield, titanium chestplate, titanium
  helmet, titanium armsguard and titanium legsguard
- New enemy ship: Pirate ship mk IV

### Changed
- Updated README.md
- Renamed all cabins to small cabins
- Updated help
- User Interface from console to graphical

### Fixed
- Typos in old changelog files
- Generating cargo in abandoned bases
- Crew member equipment after moving item to cargo

## [2.0] - 2018-01-07

### Added
- New items: steel saw, bronze bucket, iron bucket, steel bucket, iron
  blacksmith set, steel blacksmith set, iron woodworker set, steel woodworker
  set, iron repair tools, steel repair tools, iron sewing kit, steel sewing
  kit, titanium ore and titanium plates
- New crafting recipes: steel saw, bronze bucket, iron bucket, steel bucket,
  iron blacksmith set, steel blacksmith set, iron woodworker set, steel
  woodworker set, iron repair tools, steel repair tools, iron sewing kit, steel
  sewing kit and titanium plates
- New item types: titaniumore, titanium
- New ship modules: titanium cockpit, small titanium alchemy lab, small
  titanium cargo bay, titanium battering ram, medium titanium engine, medium
  titanium hull, medium advanced titanium engine, large titanium engine, large
  advanced titanium engine, small titanium furnace, advanced medium titanium
  hull, large titanium hull and advanced large titanium hull

### Changed
- Raised gained or lost reputation for missions
- Updated help
- Updated README.md

### Fixed
- Showing very long messages
- Showing messages after screen resize
- Crash in orders menu when nobody is assigned to talk in bases
- Crash in many forms when trying to left them with invalid value
- Showing full weight of items in ship cargo
- Buying items when you have that same item but damaged
- Finding empty cabin when crew member without it going on break
- Scrolling bases list by pages
- Showing item status
- Showing info about accepted missions
- Crash when training trader in base
- Description for small package, package, large package and huge package
- Don't allow gave mission items from ship cargo to crew members
- Giving orders for gunners during combat when they are on break
- Crash in crafting when there is more than one needed material type in ship
- Crafting items when there is more than one available material in ship

## [1.9] - 2017-12-10

### Added
- New items: Iron mold, iron sword, iron short sword, bronze chestplate,
  bronze helmet, bronze armsguard, bronze legsguard, iron shield, iron
  chestplate, iron helmet, iron armsguard, iron legsguard, steel short sword,
  steel sword, steel shield, steel chestplate, steel helmet, steel armsguard,
  steel legsguard, steel mold, iron gunsmith set, steel gunsmith set, bronze
  cooking set, iron cooking set, steel cooking set, iron sickle, steel sickle
  and iron saw
- New crafting recipes: Iron mold, iron sword, iron short sword, bronze
  chestplate, bronze helmet, bronze armsguard, bronze legsguard, iron shield,
  iron chestplate, iron helmet, iron armsguard, iron legsguard, steel short
  sword, steel sword, steel shield, steel chestplate, steel helmet, steel
  armsguard, steel legsguard, steel mold, iron gunsmith set, steel gunsmith
  set, bronze cooking set, iron cooking set, steel cooking set, iron sickle,
  steel sickle and iron saw
- Option to set starting equipment for mobs
- Inventory to mobs
- Equipment to mobs
- New skills: swords, dodge and brawling
- Boarding enemy ships in combat
- Combat between characters
- Showing current dodge skill name in help
- Few new mobiles
- Option to set random amount of crew for ships
- Option to grouping ships modules in ship data

### Changed
- Updated MODDING.md
- Added used skill info to weapons
- Updated interface
- Issue #9 - changed default keys for close main menu and quit from game
- Issue #11 - use this same key for help everywhere, and ability to set help
  for key
- Default ship speed after undocking as suggested in issue #14
- Starting skills for mobs
- Updated help
- Increased crews in some ships

### Fixed
- Don't stop giving order to whole crew when one crew member can't do it
- Auto changing crew members orders
- Reset selected item in crew member inventory after close
- Issue #8 - Cant delete backward with BACKSPACE in text fields
- Crash during combat when enemy ship change speed
- Showing game menu after screen resize
- Assign gunner to other gun

## [1.8] - 2017-11-12

### Added
- New goals: finish 200 destroy ship missions, finish 250 destroy ship
  missions, finish 500 destroy ship missions, finish 1000 destroy ship
  missions, finish 1500 destroy ship missions, finish 2000 destroy ship
  missions, finish 2500 destroy ship missions, finish 200 patrol area missions,
  finish 250 patrol area missions, finish 500 patrol area missions, finish 1000
  patrol area missions, finish 1500 patrol area missions, finish 2000 patrol
  area missions, finish 2500 patrol area missions, finish 200 explore area
  missions, finish 250 explore area missions, finish 500 explore area missions,
  finish 1000 explore area missions, finish 1500 explore area missions, finish
  2000 explore area missions, finish 2500 explore area missions, finish 200
  transport passenger missions, finish 250 transport passenger missions, finish
  500 transport passenger missions, finish 1000 transport passenger missions,
  finish 1500 transport passenger missions, finish 2000 transport passenger
  missions and finish 2500 transport passenger missions
- New ship module: steel harpoon gun
- New items: steel harpoon, leather sheet, bronze sword, leather jacket,
  leather helmet, leather armsguard, bronze sewing kit, leather legsguard,
  bronze shield, wooden shield and bronze short sword
- Harpoon guns to pirates ships
- New item types: leather, meleeweapon, chestarmor, headarmor, armsarmor,
  sewingkit, legsarmor and shield
- New crafting recipes: bronze harpoon, iron harpoon, steel harpoon, bronze
  sword, leather jacket, leather helmet, leather armsguard, bronze sewing kit,
  leather legsguard, bronze shield, wooden shield and bronze short sword
- New skill: leatherworking
- Character's equipment which made saves from previous version incompatible
- Option to set multipliers for points for selected goals
- Option to set starting inventory for mobs

### Changed
- Better counting ships combat value (for selection of enemies for player)
- Added info about harpoon gun to enemy info in combat screen
- Updated MODDING.md
- Value field for items from single value to list of values
- Updated help
- Better game crash reporting
- Updated CONTRIBUTING.md
- Raised max carry weight for characters
- Updated amount of points for completing some goals

### Fixed
- Stopping player ship in combat after hit by enemy harpoon
- Finding ammunition to guns during combat
- Shooting with harpoon gun by enemies ships
- Crash in trade screen after added new item to game
- Crash on showing goals list
- Don't check for amount when canceling giving items to crew member
- Search for cleaning tools in ship cargo too
- Search for healing tools in character inventory too
- Starting priorities for crew members
- Crash when crew member have full inventory and starts repairing ship (issue
  #4)
- Moving items between crew members inventory and ship cargo
- Crash when giving order to clean ship for crew member with full inventory
- Moving item from ship cargo to crew members inventory
- Crash when starting upgrading ship without needed materials
- Crash when repairing ships in bases (issue #6)

## [1.7] - 2017-10-15

### Added
- New goals: destroy 200 drones ships, destroy 250 drones ships, destroy 500
  drones ships, destroy 1000 drones ships, destroy 1500 drones ships, destroy
  2000 drones ships, destroy 2500 drones ships, destroy 200 undead ships,
  destroy 250 undead ships, destroy 500 undead ships, destroy 1000 undead
  ships, destroy 1500 undead ships, destroy 2000 undead ships, destroy 2500
  undead ships, destroy 200 inquisition ships, destroy 250 inquisition ships,
  destroy 500 inquisition ships, destroy 1000 inquisition ships, destroy 1500
  inquisition ships, destroy 2000 inquisition ships, destroy 2500 inquisition
  ships, finish 200 delivery missions, finish 250 delivery missions, finish
  500 delivery missions, finish 1000 delivery missions, finish 1500 delivery
  missions, finish 2000 delivery missions and finish 2500 delivery missions
- Option to set any amount of items types as food types
- Moving items between ship cargo and crew members inventory
- Medical supplies to starting cargo of player ship
- Option to set which skill is used for healing wounded crew members
- Option to set which skill is used for piloting ship
- Option to set which skill is used by ship engineer
- Option to set which skill is used to operate ships guns
- Option to set which skill is used to talk in bases or with other ships
- Option to set which skill is used to spotting things
- Showing current healing tool name in help
- Showing current healing skill name in help
- Showing current piloting skill name in help
- Showing current engineering skill name in help
- Showing current gunnery skill name in help
- Showing current name of skill used to talk in bases or with other ships in
  help
- Showing current perception skill name in help
- Ability to train skills in sky bases
- Showing current condition attribute name in help
- New ship weapon type: harpoon gun
- New ship modules: bronze harpoon gun and iron harpoon gun
- New item type: harpoon
- New items: bronze harpoon and iron harpoon

### Changed
- Updated MODDING.md
- Updated help
- Allow heal wounded crew members without medical room
- Moved starting player character data to separated text file
- Moved informations about ships crews to separated text file
- Show real price of healing in bases
- Show real price for buying or selling ship modules in bases
- Show real price for buying recipes in bases
- Show real price for recruit new crew members
- Show real price for repair ship in bases
- Updated interface
- Better count max amount items to sell or buy

### Fixed
- Showing various forms after screen resize
- Healing wounded crew members
- Recipe for fresh vegetables
- Crash after buying all recipes in base
- Counting attack range for enemy ships

## [1.6] - 2017-09-17

### Added
- New goals: gain max reputation in 10 independent bases, gain max reputation
  in 1 pirates base, gain max reputation in 3 pirates bases, gain max
  reputation in 5 pirates bases, visit 50 poleis bases, visit 75 poleis bases,
  visit 100 poleis bases, visit 125 poleis bases, visit 150 poleis bases,
  visit 175 poleis bases, visit 200 poleis bases, visit 250 poleis bases,
  visit 10 independent bases, visit 15 independent bases, visit 20 independent
  bases, visit 25 independent bases, visit 50 independent bases, visit 1
  pirates base, visit 3 pirates bases, visit 5 pirates bases, visit 10 pirates
  bases, destroy 200 pirates ships, destroy 250 pirates ships, destroy 500
  pirates ships, destroy 1000 pirates ships, destroy 1500 pirates ships,
  destroy 2000 pirates ships and destroy 2500 pirates ships
- New friendly ships: tiny independent patrol ship, small poleis patrol ship,
  small independent patrol ship, poleis patrol ship, independent patrol ship,
  armored poleis patrol ship, armored independent patrol ship, advanced poleis
  patrol ship, advanced independent patrol ship, large poleis patrol ship,
  large independent patrol ship, huge poleis patrol ship, huge independent
  patrol ship, advanced huge poleis patrol ship and advanced huge independent
  patrol ship
- Ability to set how many times craft selected item, which made saves from
  previous version incompatible
- Check for minimal size of terminal during game
- Option to set owners of ships for destroy ships goals targets
- Option to set types of items for crafting goals targets
- New random events: stealing ship cargo in base and retake abandoned base
- Assigned all ships (player's ship too) to sky bases
- Chance to reduce player reputation in sky base after destroying ship
- Faster gaining/losing reputation in player home sky base
- Ability to attack friendly ships
- Ability to show whole Changelog.md in News
- Ability to set condition to stop ship auto movement
- Attributes to characters
- Raise character endurance with Condition attribute
- Crew members inventory
- Showing current strength attribute name in help

### Changed
- Updated interface
- Auto assign crew member to talk in bases order when meet friendly ship
- Updated help
- Updated MODDING.md
- Updated README.md
- Amount of repair tools in starting cargo

### Fixed
- Info about time for new recruits/missions in bases list
- Adding player ship to list of friendly ships
- Crash on showing info about workshop with crafting order
- Ability to set worker in ship screen for deconstructing orders
- Showing info about low level of food/drinks/fuel
- Crash in help on resizing window
- Crafting skill for Wooden cooking set and Wooden bucket recipes
- Healing crew members

## [1.5] - 2017-08-20

### Added
- New goals: craft 1500 items, craft 2000 items, craft 2500 items, craft 3000
  items, craft 3500 items, craft 4000 items, craft 5000 items, craft 7500
  items, craft 10000 items, finish 250 missions, finish 500 missions, finish
  1000 missions, finish 1500 missions, finish 2000 missions, finish 2500
  missions, gain max reputation in 1 poleis base, gain max reputation in 3
  poleis bases, gain max reputation in 5 poleis bases, gain max reputation in
  7 poleis bases, gain max reputation in 10 poleis bases, gain max reputation
  in 15 poleis bases, gain max reputation in 20 poleis bases, gain max
  reputation in 25 poleis bases, gain max reputation in 50 poleis bases, gain
  max reputation in 1 independent base, gain max reputation in 3 independent
  bases, gain max reputation in 5 independent bases and gain max reputation in
  7 independent bases
- Ability to loot abandoned bases
- Remove items from abandoned bases over time
- Death screen
- Random prices changes in bases which made saves from previous version
  incompatible
- New friendly ships: tiny poleis trader, tiny independent trader, small poleis
  trader, small independent trader, poleis trader, independent trader, advanced
  poleis trader, advanced independent trader, large poleis trader, large
  independent trader, huge poleis trader, huge independent trader, advanced
  huge poleis trader, advanced huge independent trader and tiny poleis patrol
  ship
- New random events: friendly trader ship and friendly ship

### Changed
- Updated interface
- Auto finish missions now finish all which are ready
- Updated help
- Updated MODDING.md

### Fixed
- Redrawing screens after resize terminal
- Manual assigning ammo to guns
- Grouping damaged items in cargo
- Trading with bases when money item was changed in game settings
- Gaining loot when money index is different than default
- Crash on viewing Hall of Fame
- Crash during buying items in bases
- Damaging tools during cleaning ship
- Possible crash during repair ship

## [1.4] - 2017-07-23

### Added
- New enemy ships: huge inquisition ship mk III and advanced huge inquisition
  ship mk III
- New goals: gain max reputation in 3 bases, gain max reputation in 5 bases,
  gain max reputation in 7 bases, gain max reputation in 10 bases, gain max
  reputation in 15 bases, gain max reputation in 20 bases, gain max reputation
  in 25 bases, gain max reputation in 50 bases, destroy 250 ships, destroy 500
  ships, destroy 1000 ships, destroy 1500 ships, destroy 2000 ships, destroy
  2500 ships, discover 1500 fields of map, discover 2000 fields of map,
  discover 2500 fields of map, discover 5000 fields of map, discover 7500
  fields of map, discover 10000 fields of map, visit 75 bases, visit 100 bases,
  visit 125 bases, visit 150 bases, visit 175 bases, visit 200 bases and visit
  250 bases
- Hall of fame which made saves from previous version incompatible
- Charges for docking
- Info about amount of owned materials in crafting screen
- Option to set ship movement keys
- Option to set map manipulation keys
- Option to set menu shortcut keys
- Showing current keys in help
- Warnings about lack of fuel/food/drinks
- Warnings about low level of fuel/food/drinks
- Option to set when show warnings about low level fuel/food/drinks
- Limited and randomly changing amount of money in bases
- Limited and randomly changing amount of items in bases
- Showing current money name in help
- Showing current fuel name in help

### Changed
- Updated README.md
- Updated interface
- Updated help
- Updated MODDING.md
- Name of ship modules from cargo bay to small cargo bay

### Fixed
- Not working 'Wait Orders' entry in main menu
- Crafting interface
- Crash on redrawing main menu after resize screen
- Info about having materials/tools in crafting screen
- Typo in info about tools in crafting screen
- Crash in crafting screen when more than one tools is used in recipe
- Possible crash when showing help text
- Few misspellings in help
- Crash on showing ship cargo
- Redrawing screens after resize terminal

## [1.3] - 2017-06-25

### Added
- New ship modules: huge steel engine and advanced huge steel engine
- New enemy ships: small pirates ship mk III, small undead ship mk III, small
  clockwork drone mk III, pirate ship mk III, armored pirate ship mk III, small
  attacking drone mk III, attacking clockwork drone mk III, armored attacking
  drone mk III, small inquisition ship mk III, inquisition ship mk III, armored
  inquisition ship mk III, large clockwork drone mk III, large pirate ship mk
  III, undead ship mk III, large undead ship mk III, large inquisition ship mk
  III, large attacking drone mk III, advanced attacking drone mk III, advanced
  pirate ship mk III, advanced undead ship mk III, advanced inquisition ship mk
  III, huge pirate ship mk III, advanced huge pirate ship mk III, huge undead
  ship mk III, huge attacking drone mk III, advanced huge undead ship mk III
  and advanced huge attacking drone mk III
- Info about amount of destroyed ships to game statistics
- Auto center map after set destination for player ship (and option to enable
  or disable it)
- Auto set skybase as player ship destination after finished mission (and
  option to enable or disable it)
- Auto finish missions when player ship is near corresponding skybase (and
  option to enable or disable it)
- End game after losing all fuel during fly
- Option to set location of game directories via console parameters
- Coloring messages which made saves from previous versions incompatible
- New type of missions: transport of passengers
- New random event: brawl in base
- Player goals (achievements)
- Option to resign from game
- More detailed info about finished missions in game statistics
- More detailed info about finished crafting orders in game statistics
- Random modules upgrades to enemy ships

### Changed
- Updated interface
- Fuel usage during bad weather event depends on ship engines fuel usage
- Updated help
- Amount of gained/lost reputation from finished missions
- Updated MODDING.md
- Ship require fuel to undock from base
- Updated README.md

### Fixed
- Crash in empty list of missions
- Typo in advanced huge iron engine description
- Don't finish mission if ship can't dock to base
- Showing info about event and mission on this same map cell
- Crash when asking for events in bases
- Count max allowed amount when selling items
- Changing workplace when manufacturing
- Searching for ammunition during combat for enemy ship
- User interface for buying recipes in bases
- Selling items in bases when more than one of that item type is in cargo
- Stop crafting orders when workplace module is destroyed
- Stop upgrading module when it is destroyed
- Crash when can't load game data
- Info about minimal screen size
- Gun for advanced inquisition ship mk II
- Gaining reputation with bases

## [1.2] - 2017-05-28

### Added
- New ship modules: small steel turret, steel battering ram, small steel
  battering ram, small advanced steel engine, medium steel engine, small
  advanced steel hull, medium steel hull, medium advanced steel engine,
  large steel engine, large advanced steel engine, small steel furnace,
  advanced medium steel hull, large steel hull, advanced large steel hull,
  steel armor, heavy steel armor, steel turret, small steel greenhouse,
  small steel water collector, small steel medical room, advanced steel cabin,
  extended steel cabin, luxury steel cabin, heavy steel turret, heavy steel
  battering ram, small steel workshop, huge steel hull and advanced huge steel
  hull
- Option to set which item type is used for delivery missions items
- Option to set which item type is used as drinks
- Option to set which item type is used as corpses
- Option to set which ship is used as player ship
- Option to set which item type is used as tools for upgrade/repair modules
- Option to set which item type is used as tools for cleaning ship
- Option to set which item type is used as tools for healing crew members or
  medicines delivery for diseased bases
- Option to set which item type is used as tools for for deconstructing items
- Option to set which items types are used as food by crew members
- Option to set which item type is used as fuel
- Option to set which item is used as moneys
- Ask for rest if needed after ship move
- Support for many help files
- Option to auto rest when pilot/engineer is too tired to work
- Ability to set game options in game
- Option to set default ship speed after undock from base
- Read starting recipes from ships data file
- Option to heal wounded crew members in bases
- Last 5 messages to sky map

### Changed
- Updated MODDING.md
- Faster gaining reputation in bases
- Gain more reputation from finished missions
- Updated interface
- Updated help
- Updated recipes data
- How ships speed is calculated to prevent some bugs
- Amount of gained/lost reputation from deliver medicines to diseased bases
  depends on amount of delivered medicines
- Updated README.md

### Fixed
- Counting enemy evasion during combat
- Crash in combat when chance to hit is very small
- Typo in small advanced iron engine description
- Don't start upgrades if no upgrading material in cargo
- Crashes on delivering medical supplies to bases
- Info about abandoned bases on map
- Showing others/missions messages
- Crash on removing damaged items
- Info about lack of food/drinks in ship cargo
- Showing this same deconstruct option few times
- Sending crew member on break on selling cabin
- Crash on damaging tools during ship upgrade
- Info about free/taken guns
- Gunner back to work from rest when more than one gun is used
- Crash on overloaded ship
- Crash when recipe don't have set difficulty
- Repair selected module in bases
- Crash on repair whole ship in bases
- Showing dialog with long messages
- Crash on updating population in bases
- Showing bases coordinates on bases list

## [1.1] - 2017-04-30

### Added
- New enemy ships: tiny inquisition ship mk II, small inquisition ship mk II,
  inquisition ship mk II, armored inquisition ship mk II, large clockwork drone
  mk II, large pirate ship mk II, undead ship mk II, large undead ship mk II,
  large inquisition ship mk II, large attacking drone mk II, advanced attacking
  drone mk II, advanced pirate ship mk II, advanced undead ship mk II,
  advanced inquisition ship mk II, huge pirate ship mk II, advanced huge pirate
  ship mk II, huge undead ship mk II, huge attacking drone mk II, advanced huge
  undead ship mk II, advanced huge attacking drone mk II, huge inquisition ship
  mk II and advanced huge inquisition ship mk II
- Support for many data files of this same objects types which made saves from
  1.0 incompatible
- Starting priorities of orders for player ship crew
- New ship modules: small steel hull, light steel armor, small steel engine,
  basic steel cabin, steel cockpit, small steel alchemy lab and steel cargo bay
- Fast auto travel option
- Option to wait selected by player minutes

### Changed
- Merged fields lootmin and lootmax in ships data
- Updated MODDING.md
- Reduced needed experience for next skill level
- Updated README.md
- Moved documentation to separated directory
- Impact of randomness in combat
- Updated help
- Updated interface

### Fixed
- Crash on buying recipes of items with zero price in bases
- Cursor mark on map
- Ship orders entry in main menu
- Read default player/ship name from configuration when none entered in new
  game form
- Some ships data
- Merging damaged items
- Recipes for Andrae and Illandru logs
- Killing gunner on direct hit in gun
- Removing gun on destroying turret
- Crash on read changelog file
- Counting player accuracy during combat
- Crash on player death from starvation/dehydration

## [1.0] - 2017-04-02

### Added
- New ship modules: small iron engine, small advanced iron engine, medium
  iron engine, medium advanced iron engine, large iron engine, large advanced
  iron engine, huge iron engine, advanced huge iron engine, basic iron cabin,
  advanced iron cabin, extended iron cabin, luxury iron cabin, small iron
  turret, iron turret, heavy iron turret, iron battering ram, small iron
  battering ram and heavy iron battering ram
- Modding guide: MODDING.md
- Contributing guide: CONTRIBUTING.md
- New enemy ships: tiny pirates ship mk II, tiny undead ship mk II, small
  pirates ship mk II, small undead ship mk II, small clockwork drone mk II,
  pirate ship mk II, armored pirate ship mk II, small attacking drone mk II and
  attacking clockwork drone mk II and armored attacking drone mk II

### Changed
- Updated README.md
- Better handling orders priorities
- Updated interface
- Reduced chance to encounter stronger enemies
- Faster gaining reputation in bases

### Fixed
- Healing crew members
- Counting used modules space on ships
- Sending crew members on break on stop upgrading ship module
- Saving bases reputation which made saves from 0.9 incompatible
- Showing orders for all crew
- Crash on lack of upgrading tools during upgrade ship
- Crash on giving orders to crew members
- Orders menu when player returns with finished mission
- Sending worker on break after finished crafting
- Sending gunner on break when selling guns
- Crash on finishing crafting
- Dropping damaged items from cargo
- Return crew member to crafting items from other orders
- Buying new hull in shipyards

## [0.9] - 2017-03-05

### Added
- New enemy ships: undead ship, large undead ship, large attacking
  drone, advanced attacking drone, advanced pirate ship, advanced
  undead ship, advanced inquisition ship, huge pirate ship, advanced
  huge pirate ship, huge undead ship, huge attacking drone, advanced
  huge undead ship, advanced huge attacking drone, huge inquisition
  ship, advanced huge inquisition ship and large inquisition ship
- Descriptions to ship modules
- Descriptions to items
- Orders for all crew members
- New combat AI type: disarmer
- More info about enemy to combat screen
- Descriptions to ships
- New random events: engine damage during fly, enemy patrol and double
  price for items in bases
- New ship modules: huge bronze hull, advanced huge bronze hull, huge
  bronze engine, advanced huge bronze engine, small iron hull, small
  advanced iron hull, medium iron hull, advanced medium iron hull, large
  iron hull, advanced large iron hull, huge iron hull and advanced huge
  iron hull
- Option to set orders priorities for crew members which made saves
  from 0.8 incompatible
- Send on break crew member which have set order to talk in bases when
  outside base
- Owners of sky bases
- Owners of ships
- New item type: repairtools
- New item: bronze repair tools
- New crafting recipe: bronze repair tools
- Bronze repair tools and wooden bucket to starting cargo
- Separated names for drones ships
- Docking fees when docking to bases
- Population changes to bases
- Crew in NPC ships
- Random skills levels to NPC ships

### Changed
- Updated interface
- Updated help
- Combat AI for pirates ships
- Combat order 'Aim for weapon' hits turrets instead directly guns
- Minimum required durability of engine to fly on almost destroyed
- Better generation of event when asking in bases
- Repairing, upgrading and cleaning player ship modules needs tools
- Better destroy ship missions generation
- Updated ships data to newest version of code

### Fixed
- Attacking different ship than selected
- Attacking selected part of enemy ship
- Fuel usage for medium advanced bronze engine, large bronze engine and
  large advanced bronze engine
- Crash on loading ships data
- Generating random cargo for ships
- Centering map on new game or load old game

## [0.8] - 2017-02-05

### Added
- New ship module: Iron armor and heavy iron armor
- New item types: body, alchemyset, mold, gunsmithset, cookingset,
  sickle, saw, bucket, barleyseeds, barley, grape, grapevine, sand,
  blacksmithset and woodworkerset
- New items: corpse, package, large package, huge package, beer, wine,
  lavish ration, alchemy set, bronze mold, bronze gunsmith set, wooden
  cooking set, bronze sickle, bronze saw, wooden bucket, barley seeds,
  barley, grapes, grape vine, quartz sand, bronze blacksmith set and
  bronze woodworker set
- Corpses of dead crew members which made saves from 0.7 incompatible
- Option to set random amount of cargo to ships
- Eat raw food by hungry crew members (when no other food available)
- Tools to manufacturing items
- Alchemy set, bronze gunsmith set and wooden cooking set to starting
  cargo
- New crafting recipes: barley seeds, barley, beer, grapes, grape vine,
  wine, lavish ration, alchemy set, bronze mold, bronze gunsmith set,
  bronze sickle, bronze blacksmith set, bronze saw, bronze woodworker
  set, wooden cooking set and wooden bucket
- New skills: brewery, blacksmith and woodworking
- New type of ship modules: Workshop
- New ship modules: small bronze workshop and small iron workshop
- New type of missions in bases: explore
- New enemy ships: large clockwork drone and large pirate ship
- Option to sell all selected items in bases

### Changed
- Updated interface
- Updated help
- All NPC ships have now random amount of cargo
- Amount of reduced hunger/thirst depends on consumables
- Updated README.md
- Item type from FoodMaterial to RawFood
- Better random cargo in npc ships
- Prices of: Iron 80mm ammo, steel 80mm ammo, iron 100mm ammo, steel
  100mm ammo, iron 120mm ammo and steel 120mm ammo
- Moved starting player ship crew to separated text file

### Fixed
- Crash on selling ship modules
- Repair material for Steel 120mm gun
- Upgrade module when selling ship modules
- Check for free cargo space when accepting mission from base
- Item types for Vegetables seeds, Andrae seeds and Illandru seeds

## [0.7] - 2017-01-08

### Added
- Option to set module as priority to repair
- New enemy ships: tiny inquisition ship, small inquisition ship,
  inquisition ship and armored inquisition ship
- Option to select types of modules to show in shipyard
- Save new game settings to file
- Option to generate random player name in new game settings
- Ships size have impact on spot another ship during fly
- Game statistics
- New ship modules: Advanced bronze cabin, extended bronze cabin, luxury
  bronze cabin, heavy bronze turret, heavy bronze battering ram, iron cockpit,
  iron cargo bay, small iron alchemy lab, small iron furnace, small iron
  water collector, small iron greenhouse, small iron medical room, steel
  10mm gun, steel 20mm gun, steel 40mm gun, steel 60mm gun, steel 80mm gun,
  steel 100mm gun, steel 120mm gun and light iron armor
- Option to show game statistics after player death
- Option to generate random ship name in new game settings
- Random names for all ships
- Deconstruction of items for discover new crafting recipes
- Option to buy crafting recipes in bases
- New items types: Andraeseeds, illandruseeds and missionitem
- New items: Andrae seeds, illandru seeds and small package
- New skill: Woodcutting
- New crafting recipes: Andrae logs and illandru logs
- Changed name of ship module small alchemy lab to small bronze alchemy lab
- Random missions in bases
- New type of messages: missions messages

### Changed
- Updated interface
- Updated help
- Start game only with few known crafting recipes
- Weight of bronze cargo bay
- Weight gain during ship module upgrade depends on module durability
- Amount of crafted water in recipe

### Fixed
- Crash on giving crew orders when more than 2 medic room installed
- Deleting letters in new game window
- Crash on loading invalid save game data
- Losing reputation in bases
- Crash in repair menu when no modules to repair
- Crash on stop upgrading module
- Crash on deleting old events
- Crash on repair ship in bases
- Don't count guns to amount of modules when buying
- Assigned ammo when deleting items from cargo
- Repair priority when selling ship modules
- Crash on removing crew member

## [0.6] - 2016-12-11

### Added
- Weapon damage, crafted amount of items and speed of rest depends on how much
  ship module is damaged
- New random events: bad weather, full docks in bases, attacks on bases and
  disease in bases
- Info about events on sky map
- List of know events
- New game logo
- Option to ask for events in bases
- Cargo to all ships
- Need and using ammo in combat by npc ships
- Player reputation in bases and it impact on some player actions
- More detailed informations about skills levels
- Check for minimal console size
- More possible ammo for this same gun types
- Option to assign ammunition to selected gun
- Damage in combat depends on ammo too
- New items: steel 10mm ammo, steel 20mm ammo, steel 40mm ammo, iron 60mm ammo,
  steel 60mm ammo, medical herbs, medical herbs seeds, vegetables seeds,
  medical supplies, iron 80mm ammo, steel 80mm ammo, iron 100mm ammo, steel
  100mm ammo, iron 120mm ammo and steel 120mm ammo
- New crafting recipes: steel 10mm ammo, steel 20mm ammo, steel 40mm ammo, iron
  60mm ammo, steel 60mm ammo, medical supplies, medical herbs, fresh vegetables,
  water, iron 80mm ammo, steel 80mm ammo, iron 100mm ammo, steel 100mm ammo,
  iron 120mm ammo and steel 120mm ammo
- New ship modules: bronze turret, iron 60mm gun, small bronze greenhouse, small
  bronze water collector, small bronze medical room, iron 80mm gun, iron 100mm
  gun and iron 120mm gun
- New enemy ships: Pirate ship, armored pirate ship, small attacking drone,
  attacking clockwork drone and armored attacking drone
- New types of items: Ammo60, herbs, herbseeds, vegetableseeds, medicines, ammo80,
  ammo100 and ammo120
- Each crafting recipe have own crafting time
- New skills: medicine and farming
- New types of ship modules: greenhouse, water collector and medical room
- Healing wounded crew members
- Dirt/cleaning ship

### Changed
- Updated help
- Most of random events have duration time, they are saved to savegame which
  made saves from version 0.5 incompatible
- Starting order for gunner from On break to Operate gun
- Don't change ship speed in fight when no engineer on duty
- When change ship speed outside combat, check only for engine and engineer
- Updated interface
- Read game changes in News screen from CHANGELOG.md
- Ammo name from size ammo to Iron size ammo
- Updated enemies selection
- Better generating characters (crew, npc) names
- Updated README.md

### Fixed
- Don't craft items when worker is on break
- Crash in bases list on small screens
- Item type for steel plates
- Crash when terminal not support changing colors
- Crash in combat when crew member was killed or gun was destroyed
- Allow send tired crew member on break
- Crash in crafting with experienced crafter

## [0.5] - 2016-11-13

### Added
- New type of ship modules - furnaces
- New skills: metalsmith and perception
- New ship modules: Small bronze furnace, Advanced medium bronze hull, Large
  bronze hull, Advanced large bronze hull, Bronze armor, Heavy bronze armor and
  Iron 40mm gun
- Minimum fuel usage during fly
- New types of items: copper, tin, copperore, tinore, ironore and steel
- New items: copper plates, tin plates, copper ore, tin ore, iron ore, 40mm
  ammo and steel plates
- New crafting recipes: copper plates, tin plates, bronze plates, iron plates,
  40mm ammo and steel plates
- Option to rename player ship
- Impact of hunger/thirst/wounds on crew members skills
- Option to assign crew members to selected module
- Option to assign crew members to trade with bases
- Discovering bases
- Time passing to recruit new crew members in bases
- Option to move sky map to selected coordinates
- Show informations about selected map cell
- List of known bases
- Option to set travel destination and auto travel to it
- Ability to be auto attacked by enemy ship
- Option to stop current module update
- Restore ship speed after fight
- Gaining experience in fight for pilot and engineer

### Changed
- Each crew member have separate work timer which made saves from 0.4
  incompatible
- Each crafting module have own separate crafting order
- Updated help
- Updated interface
- Price of bronze plates
- Name of ship module Bronze armor to Light bronze armor
- Always start new game in large base
- Skill name from Bartering to Rhetoric
- Starting order for player from On break to Talking in bases
- Moved items types to separated text file
- Starting combat with other ships
- Item type ammo on ammo10, ammo20, ammo40
- Updated in-game news

### Fixed
- Crash on updating modules
- Crash on check for new recruits in bases
- Clearing module owner on change crew member order
- Time needed for rest in wait command
- Check for random events if there was no ship move
- Gaining experience for trading
- Ammo for Iron 10mm gun
- Messed screen on deleting messages
- Info about lack of crew on selected position
- Gaining experience for gunner for last round of combat
- Crash on assign crew members to positions during combat
- Assign new pilot when older is on break
- Crash on repair ship modules

## [0.4] - 2016-10-16

### Added
- Auto-resize game screen on resize console
- Population to bases which made saves from 0.3 incompatible
- Option to repair ship in bases
- New type of bases - shipyards
- Separated repair skill for each module
- Maximum amount of messages - 500
- Option to delete all messages
- Upgrading ship modules
- Cabin quality to speed of rest
- New item to game: 10mm ammo
- New crafting recipe: 10mm ammo
- Installing/removing modules from ship
- Fuel usage during movement depends on amount of engines installed on ship
- New ship modules: small advanced bronze engine, medium bronze engine, small
  advanced bronze hull and medium bronze hull, medium advanced bronze engine,
  large bronze engine, large advanced bronze engine
- Info about unique ship modules
- Recruitment new crew members in bases
- Dismissing crew members
- New enemy ship - small clockwork drone
- Support for many weapons for ships in combat
- Option to install battering ram on player ship
- Attack with battering ram by player
- Ship modules size

### Changed
- Updated help
- Updated interface
- Showing more messages on combat screen
- Add only one crafting message with summary of manufacturing
- Names of some ship modules
- Moved skills names to separated text file
- Fuel usage during fly depends on engine
- Enemy attack range depends on weapon type not on ship
- Limit player ship attacks distance
- Amount of enemies attacks depends on weapon type
- Updated in-game news

### Fixed
- Crash on invalid data in save game file
- Adding destroyed cargo bay to free cargo space
- Crash in combat on small screens
- Coloring missing materials on recipes screen
- Showing negative free cargo space
- Some memory leaks
- Crash on updating cargo during manufacturing
- Crash on show info about empty cabin
- Crash on show info about empty turret
- Informations about crew members skills levels

## [0.3] - 2016-09-18

### Added
- New enemy ships - small pirate ship and small undead ship
- Support for enemy armor in combat
- Types of messages (combat/crafting/etc.)
- Option to sort messages by types on messages screen
- New items to game: Illandru logs, fine rations, gunpowder, iron plates
- New types of items: gunpowder, iron
- New crafting recipes: 20mm ammo and fine rations
- New skills: cooking and gunsmith
- Some iron to starting cargo
- Info which ships are enemies
- Very simple enemy AI in combat
- Option to rename player ship modules
- Option to drop selected item from ship cargo
- Show information about ship/cargo/crew on combat screen
- Better handling crew members deaths
- Owners for ship modules
- Assign ship guns to turrets
- Destruction of guns after destroying turrets
- Killing crew member when selected ship module is destroyed
- Evasion bonus for enemies
- Check for modules when giving orders to crew members
- Crew member will search for empty cabin when going on break
- Pilot skill to evade chance in combat
- Separated loot amount for each enemy

### Changed
- Renamed old enemies from small to tiny
- Saving new messages format to savegame file which made saves from 0.2 incompatible
- Show last messages as first on last messages screen
- Item type FuelMaterial on Wood and RepairMaterial on Bronze
- Crafting recipes now can have more than one material needed to craft
- All ship modules have own repair materials
- Each crafting recipe use separated skill
- Updated interface
- Split help on topics
- Updated help
- Raised ships speed
- Counting ships speed on sky map
- Raised durability of cockpit and turret
- Raised amount of loot from combat
- Speed of rest depends on did crew member have cabin
- Don't recover health if crew member don't have cabin
- Name of ship module from cargo space to cargo bay
- Bonus to hit/evade in combat depends on ships real speed not on engineer orders
- Removed need for dock to base to discover its type
- Moved syllables for bases and crew names to separated text file
- Better bases names generator
- Updated in-game news

### Fixed
- On combat screen showing messages from previous combat
- Moving ship with arrows keys
- Starting random events on map moving or centering map
- Giving orders to gunner during combat
- Healing dead crew members
- Information about no weapon on ship in combat
- Crash on giving manufacturing order when ship manufacturing order isn't set

## [0.2] - 2016-08-21

### Added
- Wait 1 game minute command
- Fatigue, hunger, thirst, health to crew members
- Show last message on screen
- Informations why action failed (trade/ship moving/orders)
- Check for free cargo space during trade items
- End game on player death
- Confirmation dialog for quit to main menu
- New game window with ability to set player and ship names and player gender
- Version to savegame file which made saves from 0.1 incompatible
- Window with informations about current location
- Gaining experience in skill by crew members
- Pilot/Engineer skills influence on flying time
- Player Bartering skill influence on item prices in bases
- Random names for bases and crew members
- Armor module for ships
- Combat between ships
- Random event during fly (combat)
- Items manufacturing
- New items: bronze plates, fresh fruits, fresh vegetables, fresh meat, andrae logs
- Save to file last 10 messages
- Ability to move map without moving ship
- Informations about license to main menu
- Menu with commands to wait certain amount of time
- Informations about in game changes to main menu
- Random gender to crew members
- New ship weapon type - battering ram

### Changed
- Updated README.md
- Updated in-game menu
- Moved ship orders to game menu
- Updated help
- Updated interface
- Player and gunner starts game with on break command
- Show ship name instead speed in ship info screen
- Moved more items informations to prototypes
- Raised starting skills levels
- Moved help text to separated text file
- Raised price of basic rations
- Moved items data to separated text file
- Better world generation
- Moved ship modules data to separated text file
- Moved ships data to separated text file
- Moved ship cargo informations to separate screen

### Fixed
- Crash in save game when game was started from other directory
- Giving orders to dead/starving/dehydrated/too tired crew members
- Crash on too long list of messages to show

### Removed
- Duty order for player

## [0.1] - 2016-07-24
Initial release
