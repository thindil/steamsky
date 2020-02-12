# Changelog
All notable changes to this project will be documented in this file.

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
- Moved information about ships crews to separated text file
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

