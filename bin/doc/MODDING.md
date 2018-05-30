Some general informations about adding/editing some game elements (skills,
ships, ships modules, etc.)

## Items types

### General informations
- Open file *game.dat* in *data* directory. Each item type is one line entry
  with tag `itemtype`.

### Changing existing items types
- To change name of existing item type, edit `value` attribute for selected 
  type.
- After change of Item Type don't forget to do proper changes in other data
  files (like items or recipes) and in this same file if item type was used in
  one of below settings.

### Adding new items types
- To add new item type, just append new line with tag `itemtype` and it name
  as `value` attribute.
- Each new ammunition type must starts with `Ammo` (example: *Ammo150*).
- Ammunition for harpoon guns must be named `Harpoon`.

### Removing item types
- Just remove line with tag `itemtype` and selected item type name as `value`
  attribute.
- After delete of Item Type don't forget to do proper changes in other data
  files (like items or recipes) and in this same file if item type was used in
  one of below settings.

## Repair tools type
To change which item type is used for repair/upgrading tools, open file 
*game.dat* in *data* directory and edit `value` attribue of tag `repairtools`.
Value must be existing item type.

## Cleaning tools type
To change which item type is used for cleaning ship, open file *game.dat* in 
*data* directory and edit `value` attribute of tag `cleaningrools`. Value must
be existing item type.

## Healing tools type
To change which item type is used for healing crew members and for deliver
medicines to bases, open file *game.dat* in *data* directory and edit `value`
attribute of tag `healingtools`. Value must be existing item type.

## Player Ship index
To change which ship is used as a player ship, open file *game.dat* in *data*
directory and edit `value` attribute of tag `playershipindex`. Value must be 
existing ship index from any ships file from *data/ships* directory.

## Alchemy tools type
To change which item type is used for deconstruct items, open file *game.dat* 
in *data* directory and edit `value` attribute of tag `alchemytools`. Value 
must be existing item type.

## Drinks type
To change which item type is used for reduce thirst, open file *game.dat* 
in *data* directory and edit `value` attribute of tag `drinkstype`. Value must
be existing item type.

## Corpse index
To change which item is used as a body for dead, open file *game.dat* in *data*
directory and edit `value` attribute of tag `corpseindex`. Value must be 
existing item index from any items file from *data/items* directory.

## Mission items type
To change which item type is used for delivery missions, open file *game.dat* 
in *data* directory and edit `value` attribute of tag `missionitemstype`. Value
must be existing item type.

## Food items types
Open file *game.dat* in *data* directory. Each food type is one line entry
with tag `foodtype`. To change name of existing food type, edit `value`
attribute for selected type. To add new food type, just append new line 
with tag `itemtype` and it name as `value` attribute. Value must be existing
item type. To remove food type, just remove line with tag `foodtype` and 
selected food type name as `value` attribute.

## Fuel item type
To change which item type is used as a fuel for ship, open file *game.dat* in 
*data* directory and edit `value` attribute of tag `fueltype`. Value must be 
existing item type.

## Money index
To change which item is used as a money, open file *game.dat* in *data* 
directory and edit `value` attribute of tag `moneyindex`. Value must be 
existing item index from any items file from *data/items* directory.

## Traders ships name
To change which word in ship names is used to determine trader ship (needed for
friendly trader random event), open file *game.dat* in *data* directory and
edit `value` attribute for tag `tradersname`. Value can be any single word
(but this word must be in trader ships names).

## Characters attributes

### General informations
- To add/remove/change characters attributes, open file *game.dat* in
  *data* directory.
- Each attribute starts with tag `attribute`. Attribute `name` is name of
  selected attribute. Value between `attribute` tags is description of
  attribute. Example *<attribute name="Dexterity">Nimbleness of character,
  used in many crafts</attrbute>*.

### Changing existing attribute
- After change attribute name, don't forget to made proper changes in this
  same data file (if needed).
- To edit attribute name, just edit value of attribute `name`.
- To change attribute description, just edit text between `attribute` tags.

### Adding new attribute
- To add new attribute, just append new line with tag `attribute` with
  it name as attribute `name` and description between tags `attribute`.

## Skills

### General informations
- Open file *game.dat* in *data* directory to add/remove/change skills.
- Each skill starts with tag `skill`. Attribute `name` is name of selected 
  skill, attribute `attribute` is name of character attribute (must be defined
  ealier in this same file). Value between `skills` tags is description of
  skill. Example: *<skill name="Gunnery" attribute="Dexterity">Used by 
  gunners, determine chance to hit enemy in ship combat.</skill>*.

### Changing existing skills
- After change skill name, you been need to made proper changes in other game
  data files (recipes, items and this same file) if needed.
- To change skill name, just edit value of `name` attribute of selected skill.
- To change assigned attribute to selected skill enter new name in attribute
  `attribute`. Name must be existing attribute name, defined ealier in this
  same file.
- To change skill description, just edit text between `skill` tags.

### Adding new skills
- To add new skill, just append new line with tag `skill` with it name as
  attribute `name`, assigned attribute to skill as attribute `attribute` and
  description between tags `skill`.

## Condition attribute name
To change which attribute is used to raise character condition, open file 
*game.dat* in *data* directory and edit attribute `value` of tag 
`conditionname`. Value must be existing attribute name.

## Strength attribute name
To change which attribute is used to count character maximum encumbrance, open
file *game.dat* in *data* directory and edit attribute `value` of tag 
`strenghtname`. Value must be existing attribute name.

## Healing skill name
To change which skill is used for healing wounded crew members, open file 
*game.dat* in *data* directory and edit `value` attribute of tag 
`healingskill`. Value must be existing skill name.

## Piloting skill name
To change which skill is used for piloting ship, open file *game.dat* in 
*data* directory and edit `value` attribute of tag `pilotingskill`. Value must 
be existing skill name.

## Engineering skill name
To change which skill is used for engineering duty, open file *game.dat* in 
*data* directory and edit `value` attribute of tag `engineeringskill`. Value
must be existing skill name.

## Gunnery skill name
To change which skill is used for operating guns on ship, open file *game.dat*
in *data* directory and edit `value` attribute of tag `gunneryskill`. Value 
must be existing skill name.

## Talking skill name
To change which skill is used for talking in bases or with other ships (trades,
repairs, recruit, etc), open file *game.dat* in *data* directory and edit 
`value` attribute of tag `talkingskill`. Value must be existing skill name.

## Spotting skill name
To change which skill is used for spotting things (other ships, etc), open 
file *game.dat* in *data* directory and edit `value` attribute of tag 
`perceptionskill`. Value must be existing skill name.

## Player character index
To change which mobile data is used for starting player character data 
(attributes, skills, etc), open file *game.dat* in *data* directory and edit
`value` attribute of tag `playerindex`. Value must be existing mobile index 
from any mobiles file from *data/mobs* directory.

## Head armor type
To change which item type is used as a head armor for characters, open file 
*game.dat* in *data* directory and edit `value` attribute of tag `headarmor`.
Value must be existing item type.

## Torso armor type
To change which item type is used as a torso armor for characters, open file 
*game.dat* in *data* directory and edit `value` attribute of tag `chestarmor`.
Value must be existing item type.

## Arms armor type
To change which item type is used as an arms armor for characters, open file 
*game.dat* in *data* directory and edit `value` attribute of tag `armsarmor`.
Value must be existing item type.

## Legs armor type
To change which item type is used as a legs armor for characters, open file 
*game.dat* in *data* directory and edit `value` attribute of tag `legsarmor`.
Value must be existing item type.

## Shield type
To change which item type is used as a shield for characters, open file 
*game.dat* in *data* directory and edit `value` attribute of tag `shieldtype`.
Value must be existing item type.

## Weapon type
To change which item type is used as a weapon by characters, open file 
*game.dat* in *data* directory and edit `value` attribute of tag `weapontype`.
Value must be existing item type.

## Dodging skill name
To change which skill is used for dodges in character's combat, open 
file *game.dat* in *data* directory and edit `value` attribute of tag 
`dodgeskill`. Value must be existing skill name.

## Unarmed skill name
To change which skill is used for chance to hit enemy in character's combat
when character don't have weapon, open file *game.dat* in *data* directory and
edit `value` attribute of tag `unarmedskill`. Value must be existing skill name.

## Items

### General informations
- Open file *items.dat* in *data/items* directory or create new file with *dat*
  extension in this directory (example: *myitems.dat*)

### Item data structure
- Each item starts with tag `item`.
- Attribute `index` is a item index (it can be number or text) and must be 
  unique. This value is used in ships and recipes data entries.
- `name` attribute: name of item displayed in various places (cargo info, 
  crafting, etc.)
- Attribute `weight`: weight of one item in kilograms
- Attribute `type`: item type of item (from *game.dat* file, entry 
  *ItemsTypes*)
- Each `trade` tag is for selected type of bases: Industrial, Agricultural, 
  Refinery, Shipyard, Military. Attribute `price` is price of item in selected
  base type. If you want that item will be not possible to buy/sellable in 
  bases (or only selected base type) set it price to 0. Attribute `buyable`
  said did item can be bough in selected type of base. "Y" mean Yes, "N" means
  No.
- Attribute `showtype`: optional attribute. If you want to show item type in 
  game (for example in cargo or in trade screen) different than item type 
  from *game.dat* file, you can set this parameter to any text value.
- Data: optional tags. Each tag is one value. For 'Food' or 'Raw Food' it is
  value of hunger reduced by one portion. For 'Drink', value of thirst reduced
  by one portion. For ammunition it is damage done by that ammunition. For 
  working tools it is chance for item to be damaged during work. For harpoon 
  guns ammunition it is how long (in combat turns) item will be stuck in enemy
  ship. For weapons and armor pieces first value is change for item to be 
  damaged during combat, second entry for weapons is damage done by weapon 
  and for armor piece it is amount of damage reduced by this armor.
  Third entry for weapons is number of skill used by this weapon (from 
  *game.dat* file, entry *Skills*) and for armor is amount of levels of dodge 
  skill which this armor reduce when weared. Forth entry for weapon is amount
  of hands used (1 for one-handed, 2 for two-handed weapons). Fifth entry for
  weapon is damage type (1 - cutting damage, 2 - impaling damage, 3 - blunt
  damage).
- Tag `description`: In game description of item. Can have any value.

## Recipes

### General informations
- Open file *recipes.dat* in *data/recipes* directory or create new file with
  *dat* extension in this directory (example: *myrecipes.dat*).
- Each value in recipe data structure must be one line length.
- File must end with `[]`.

### Recipe data structure
- Each recipe start from `[` symbol. Any value between `[` and `]` is a recipe 
  index (it can be number or text) and must be unique. This value is used at
  this moment for set starting recipes and in Craft types of goals.
- Material: Item type of materials need for recipe. Each material must be
  separated with `, ` (comma and space)
- Amount: Amount of crafting materials needed for recipe. If you set more than
  one material for recipe, you must set amount for each of them. Each material
  amount must be separated with `, ` (comma and space)
- Result: Item index which will be produced by recipe (you can check this index
  in *items* directory)
- Crafted: Amount of items crafted from one recipe
- Workplace: Type of ship module in which recipe is made. Available options
  are: Alchemy\_Lab, Furnace, Water\_Collector, Workshop, Greenhouse
- Skill: Name of skill used during crafting selected recipe (skills names are
  in *game.dat* file)
- Tool: Type of item used as tool in crafting selected recipe (items types are
  in *game.dat* file)
- Difficulty: optional parameter. How complicated recipe is. Should be between 
  1 and 100.
- BaseType: In which bases type recipe can be bought. If not set, recipe will be
  know from beginning of game. Values: 1 for Industrial, 2 for Agricultural, 3
  for Refinery, 4 for Shipyard and 5 for Military.
- Time: optional parameter. How long in minutes take crafting selected recipe.
  If not set, it take 15 minutes.

## Ship modules

### General informations
- Open file *shipmodules.dat* in *data/shipmodules* directory or create new
  file with *dat* extension in that directory (example: *myshipmodules.dat*).

### Ship module data structure
- Each ship module starts with tag `module`.
- Attribute `index` is a module index (it can be number or text) and must be 
  unique. This value is used in ships data entries.
- Attribute `name`: Standard name of module. Will be visible in ship info screen
  and in shipyards.
- Attribute `type`: Type of module. Available options are: Engine, Cabin, Cockpit,
  Turret, Gun, Cargo, Hull, Armor, Battering\_ram, Alchemy\_Lab, Furnace,
  Water\_Collector, Workshop, Greenhouse, Medical\_room, Harpoon\_Gun
- Attribute `weight`: Weight of module in kilograms.
- Attribute `value`: Depends on type of module. For 'Engine' it is fuel usage 
  for travel by one map field. For 'Cabin' it is value of reduced tiredness of 
  owner who rest there. For 'Gun' or 'Harpoon\_Gun' it is index of item type 
  used as ammunition (item types are in *game.dat* file). For any other type 
  of modules should be 0 (zero).
- Attribute `maxvalue`: Depends on type of module. For 'Hull' it is max free 
  module space. For 'Engine' it is engine power. For 'Cabin' should be that 
  same like attribuge `value` value. For 'Cargo' it is maximum capacity in 
  kilograms for cargo for that module. For 'Gun', 'Battering\_ram' it is amount
  of damage done by selected weapon. For 'Harpoon\_Gun' it is amount of combat
  rounds by how long harpoon is stuck in enemy ship. For any other type of 
  modules should be 0 (zero).
- Attribute `durability`: Base durability of module. How many damage module can
  take before will be destroyed.
- Attribute `material`: Type of item which will be used to repair or upgrade 
  module.
- Attribute `skill`: Name of skill which will be used during repair or upgrading 
  module.
- Attribute `price`: Base buy or sell price of module in shipyard.
- Attribute `installtime`: How long in minutes take install/remove selected 
  module from ship.
- Optional attribute `unique`: If set to 'Y' then each ship can have only one 
  module of that type.
- Optional attribute `size`: Amount of hull module space used by module.
- Text between `module` tags is module description.

## Ships

### General informations
- Open file *ships.dat* in *data/ships* directory or create new file with *dat*
  extension in that directory (example: *myships.dat*).
- Each value in ship data structure must be one line length.
- File must end with `[]`.

### Ship data structure
- Each ship start from `[` symbol. Any value between `[` and `]` is a index 
  (it can be number or text) and must be unique. This value is used at this
  moment to set player ship and in Destroy types of goals.
- Name: Type of ship. Will be visible during combat information. If you want
  that ship will be used in friendly trader random event, you must have word
  which you set in *game.dat* as *TraderNames* in ship name. Example: if you
  use *trader* word, ship name can be *small poleis trader*.
- Modules: List of modules indexes (from *shipmodules* directory), separated 
  by `, ` (comma and space). Each module entry can be only module index or
  Amount`x`Module index.
- Accuracy: Bonus to accuracy for ship. Can be constant value (example: 1) or 
  range from minimum value to max value, separated by `..` (double dots)
  (example: 1..5).
- CombatAI: Behavior of ship in combat (NPC ships only). Possible values are:
  Berserker - attack to end, no matter how heavy damage take.
  Attacker - aggressive but will be run away from combat when lost all
  ammunition or weapons.
  Coward - try run from combat, attack only in self-defense.
  Disarmer - same as Attacker but first aim for player ship weapons before
  start destroying ship.
- Evasion: Bonus to evasion for ship. Can be constant value or range from 
  minimum value to max value, separated by `..` (double dots).
- Loot: Amount of moneys earned for destroying that ship. Can be constant value
  or range from minimum value to max value, separated by `..` (double dots).
- Perception: Bonus to perception for ship. Can be constant value or range 
  from minimum value to max value, separated by `..` (double dots).
- Cargo: List of cargo of ship. Each item in cargo is separated by `, ` (comma
  and space). Each item entry is Amount`x`Item index (from *items* directory).
  Amount can be constant value or range from minimum value to max value, 
  separated by `..` (double dots).
- Description: Any length description of ship.
- Owner: Which fraction own ship. Possible values are: Poleis, Independent,
  Pirates, Undead, Drones, Inquisition.
- Recipes: List of know recipes indexes (from *recipes* directory, player ship
  only), separated by `, ` (comma and space).
- Crew: List of crew members indexes (from *mobs* directory), separated by 
  `, ` (comma and space). Each crew entry can be only mob index or
  Amount`x`Mob index. Amount can be constant value or range from minimum
  value to max value, separated by `..` (double dots).

## Help

### General informations
- Open file *help.dat* in *data/help* directory or create new file with *dat* 
  extension in that directory (example: *myhelp.dat*).
- File must end with `[]`.

### Help data structure
- Each help entry starts from `[` symbol. Any value between `[` and `]` is a
  menu entry in main help menu. It can be number or text.
- Whole text below (until next new line which starts from `[`) is help entry
  text visible when player select this option from help menu.
- Inside help text you can use special variables which later will be replaced
  by proper keys names. All  that variables have name `{GameKey [number]}`
  where `[number]` is between 1 and 27 (example: `{GameKey 9}`). Proper game 
  keys in order: move ship up/left, move ship up, move ship up/right, move
  ship left, move ship one field or wait 1 minute, move ship right, move ship
  down/left, move ship down, move ship down/right, move ship to destination, 
  show ship info, show cargo info, show crew info, show ship orders, show 
  crafting menu, show last messages, show know bases, show known events, show 
  accepted missions, move map position, show game statistics, show help, show 
  game options, quit from game, resign from game, show menu, show wait orders.
  (example: `{GameKey 10}` will be translated to key used for auto move ship).
- Inside help text you can use special variables: `{MoneyName}` which later 
  will be replaced with name of game money, `{FuelName}` which later will be
  replaced with name of fuel for ship, `{StrengthName}` which later will be
  replaced with name of attribute used to count max character encumbrance,
  `{HealingTools}` which later will be replaced with name of tool used for 
  healing, `{HealingSkill}` which later will be replaced with name of skill
  used for healing, `{PilotingSkill}` which later will be replaced with name
  of skill used for piloting ship, `{EngineeringSkill}` which later will be
  replaced with name of skill used by ship engineer, `{GunnerySkill}` which
  later will be replaced with name of skill used to operate guns on ship,
  `{TalkingSkill}` which later will be replaced with name of skill used to
  talk in bases or with other ships, `{PerceptionSkill}` which later will be
  replaced with name of skill used to notice other ships, `{ConditionName}`
  which later will be replaced with name of attribute used to raise crew
  member condition, `{DodgeSkill}` which later will be replaced with name of
  skill used to dodge enemy attacks in character's combat.

## Goals

### General informations
- Open file *goals.dat* in *data/goals* directory or create new file with *dat*
  extension in that directory (example: *mygoals.dat*).
- Each value in goal data structure must be one line length.
- File must end with `[]`.

### Goal data structure
- Each ship start from `[` symbol. Any value between `[` and `]` is a index 
  (it can be number or text) and must be unique. At this moment this value is
  used to set/update goal in game.
- Type: Type/category of goal which define what is needed to do for finish
  selected goal. Possible values: Reputation - gain max reputation in X bases,
  Destroy - destroy X ships, Discover - discover X fields of map, Visit - visit
  (discover) X bases, Craft - craft X items, Mission - Finish X missions, 
  Kill - Kill X enemies in melee combat
- Amount: Amount of target for selected goal to do for finish it. For example
  100 ships to destroy, etc.
- Target: Optional parameter, ignored for Discover type, exact target for 
  goal, depends on type of goal. Possible values: for Reputation and Visit, 
  bases owners names: Poleis, Independent, Pirates, Undead, Drones, 
  Inquisition. For Destroy, any enemy ship index (from *ships* directory) or
  owners names: Poleis, Independent, Pirates, Undead, Drones, Inquisition. For
  Craft, recipe index (from *recipes* directory), Type or ShowType (from
  *items* directory). For Mission, missions types: Deliver for deliver item to 
  bases, Destroy for destroy enemy ship, Patrol for patrol missions, Explore 
  for explore missions, Passenger for transport passengers missions. For Kill,
  enemy fraction name: Poleis, Independent, Pirates, Undead, Drones, 
  Inquisition.
- Multiplier: Optional parameter, multiplier for amount of game points earned
  by finishing this goal. Default value is 1, which mean goal give Amount of
  points for finish it.

## Mobiles

### General informations
- Open file *mobs.dat* in *data/mobs* directory or create new file with *dat*
  extension in that directory (example: *mymobs.dat*).
- Each value in goal data structure must be one line length.
- File must end with `[]`.
- First entry in file *mobs.dat* is player character.

### Mob data structure
- Each ship start from `[` symbol. Any value between `[` and `]` is a index 
  (it can be number or text) and must be unique.
- Skills: List of separated by `, ` (comma and space) names of skills (from
  *game.dat* from *data* directory) which selected mob known.
- SkillsLevels: List of levels of skills which selected mob known, separated
  by `, ` (comma and space). Skill level can be constant value or range from
  minimum value to max value, separated by `..` (double dots). Must be that
  same amount as for Skills entry.
- Attributes: List of levels of mobile attributes, separated by `, ` (comma
  and space). Can be constant value or range from minimum value to max value,
  separated by `..` (double dots).
- Order: Current ship order of selected mob. Possible values are: Pilot,
  Engineer, Gunner, Repair, Craft, Upgrading, Talk, Heal, Clean, Rest, Defend,
  Boarding.
- Priorities: Orders priorities for selected mob, separated by `, ` (comma and
  space). Each priority entry is Order`:`Priority where Order is: Piloting,
  Engineering, Operating guns, Repair ship, Manufacturing, Upgrading ship,
  Talking in bases, Healing wounded, Cleaning ship, Defend ship, Board enemy
  ship and Priority is: Normal or High.
- Inventory: List of inventory of selected mob. Each item in inventory is 
  separated by `, ` (comma and space). Each item entry is Amount`x`Item index
  (from *items* directory). Amount can be constant value or range from minimum
  value to max value, separated by `..` (double dots).
- Equipment: List of items from inventory used by selected mob. Each item in
  equipment is separated by `, ` (comma and space). Each item entry is Slot
  name`:`Item index from inventory, where Slot name is: Weapon, Shield, Head,
  Torso, Arms, Legs, Tools. Item index always starts from 1.

## Debugging
If you want test your changes, you may run game in debug mode. In this mode
game create file *debug.log* in *data* directory. To start game in debug mode
run it with parameter --debug=[debugtype]. Example:

`./steamsky --debug=everything`

At this moment available are two types of debug:

- Option 'everything' mean to log each debug message to file. At this moment
  this mean informations about loading game data (ships/recipes/items/modules)
  and some combat data. This option may be useful to check correctness of data
  of added new items, etc.
- Option 'combat' mean to log debug messages only from combat.

