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

## Alchemy tools type
To change which item type is used for deconstruct items, open file *game.dat* 
in *data* directory and edit `value` attribute of tag `alchemytools`. Value 
must be existing item type.

## Corpse index
To change which item is used as a body for dead, open file *game.dat* in *data*
directory and edit `value` attribute of tag `corpseindex`. Value must be 
existing item index from any items file from *data/items* directory.

## Mission items type
To change which item type is used for delivery missions, open file *game.dat* 
in *data* directory and edit `value` attribute of tag `missionitemstype`. Value
must be existing item type.

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

## Player faction
To change which faction is player faction, edit `value` attribute of tag
`playerfaction`. Value must be existing faction index from factions files from
*data/factions* directory.

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
- Data: optional tags. Each tag is one value. For items used as food it is
  value of hunger reduced by one portion. For item used as drinks, value of 
  thirst reduced by one portion. For ammunition it is damage done by that
  ammunition. For working tools it is chance for item to be damaged during 
  work. For harpoon guns ammunition it is how long (in combat turns) item will
  be stuck in enemy ship. For weapons and armor pieces first value is change 
  for item to be damaged during combat, second entry for weapons is damage 
  done by weapon and for armor piece it is amount of damage reduced by this armor.
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

### Recipe data structure
- Each recipe starts with tag `recipe`.
- Attribute `index` is a recipe index (it can be number or text) and must be
  unique. This value is used at this moment for set starting recipes and in 
  Craft types of goals.
- Tag `material` contains data about material used to craft recipe. Attribute
  `type` is item type of material need for recipe. Attribute `amount` is 
  amount of crafting materials needed for recipe. If you want to add more 
  materials to recipe, just add new tag `material` with proper data.
- Attribute `result`: Item index which will be produced by recipe (you can 
  check this index in *items* directory).
- Attribute `crafted`: Amount of items crafted from one recipe.
- Attribute `workplace`: Type of ship module in which recipe is made. Available
  options are: Alchemy\_Lab, Furnace, Water\_Collector, Workshop, Greenhouse
- Attribute `skill`: Name of skill used during crafting selected recipe (skills
  names are in *game.dat* file).
- Attribute `tool`: Type of item used as tool in crafting selected recipe 
  (items types are in *game.dat* file).
- Attribute `difficulty`: optional attribute. How complicated recipe is. Should
  be between 1 and 100.
- Attribute `basetype`: In which bases type recipe can be bought. If not set, 
  recipe will be know from beginning of game. Values: 1 for Industrial, 2 for 
  Agricultural, 3 for Refinery, 4 for Shipyard and 5 for Military.
- Attribute `time`: optional attribute. How long in minutes take crafting selected
  recipe. If not set, it take 15 minutes.

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
  Water\_Collector, Workshop, Greenhouse, Medical\_room, Harpoon\_Gun, 
  Training\_Room
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

### Ship data structure
- Each ship is between `ship` tags. Attribute `index` is a index (it can be 
  number or text) and must be unique. This value is used at this moment to 
  set player ship and in Destroy types of goals.
- Attribute `name`: Type of ship. Will be visible during combat information. 
  If you want that ship will be used in friendly trader random event, you must
  have word which you set in *game.dat* as *TraderNames* in ship name. Example:
  if you use *trader* word, ship name can be *small poleis trader*.
- Tags `module`: List of ship modules installed on selected ship. Attribute
  `index` is module index from files from *shipmodules* directory. Attribute
  `amount` is optional: if ship should have more than one that module, just
  add attribute `amount` with number of modules.
- Attribute `accuracy`: Bonus to accuracy for ship.
- Attributes `minaccuracy` and `maxaccuracy`: If bonus to accuracy for ship
  should be random, add attribute `minaccuracy` for minimum value and 
  `maxaccuracy` for maximum value.
- Attribute `combatai`: Behavior of ship in combat (NPC ships only). Possible 
  values are: Berserker - attack to end, no matter how heavy damage take. 
  Attacker - aggressive but will be run away from combat when lost all 
  ammunition or weapons. Coward - try run from combat, attack only in 
  self-defense. Disarmer - same as Attacker but first aim for player ship 
  weapons before start destroying ship.
- Attribute `evasion`: Bonus to evasion for ship.
- Attributes `minevasion` and `maxevasion`: If bonus to evasion for ship should
  be random, add attribute `minevasion` for minimum value and `maxevasion` for 
  maximum value.
- Attribute `loot`: Amount of money earned for destroying that ship.
- Attributes `minloot` and `maxloot`: If amount of earned money for destroying 
  that ship should be random, add attribute `minloot` for minimum value and 
  `maxloot` for maximum value.
- Attribute `perception`: Bonus to perception for ship.
- Attributes `minperception` and `maxperception`: If bonus to perception for 
  ship should be random, add attribute `minperception` for minimum value and 
  `maxperception` for maximum value.
- Tags `cargo`: List of items in cargo of ship. Attribute `index` is index of
  item from files from *items* directory. If amount of that item should be 
  constant, add attribute `amount` with proper value. If amount of that item
  should be random, add attributes `minamount` with minimum amount and attribute
  `maxamount` with maximum amount of that item.
- Tag `description`: Description of ship (NPC ships only). Will be displayed
  during combat.
- Attribute `owner`: Which fraction own ship. Possible values are: Poleis, 
  Independent, Pirates, Undead, Drones, Inquisition.
- Tags `recipes`: List of know recipes. Attribute `index` is recipe index from
  files from *recipes* directory (player ship only).
- Tags `member`: List of crew members. Attribute `index` is mobile index from
  files form *mobs* directory. If ship should have more than one that same 
  mobile if crew, add attribute `amount`. If ship should have more than one 
  that same mobile and amount should be random, add attributes `minamount` for 
  minimum amount of that mobile and attribute `maxamount` for maximum amount of 
  that mobile.

## Help

### General informations
- Open file *help.dat* in *data/help* directory or create new file with *dat* 
  extension in that directory (example: *myhelp.dat*).

### Help data structure
- Each help entry is between `entry` tags. 
- Attribute `title` is help menu entry in main help menu. It can be number or
  text.
- Text between tags `entry` is help entry text text visible when player select
  this option from help menu.
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
  `{HealingSkill}` which later will be replaced with name of skill
  used for healing, `{PilotingSkill}` which later will be replaced with name
  of skill used for piloting ship, `{EngineeringSkill}` which later will be
  replaced with name of skill used by ship engineer, `{GunnerySkill}` which
  later will be replaced with name of skill used to operate guns on ship,
  `{TalkingSkill}` which later will be replaced with name of skill used to
  talk in bases or with other ships, `{PerceptionSkill}` which later will be
  replaced with name of skill used to notice other ships, `{ConditionName}`
  which later will be replaced with name of attribute used to raise crew
  member condition, `{DodgeSkill}` which later will be replaced with name of
  skill used to dodge enemy attacks in character's combat, `{UnarmedSkill}`
  which later will be replaced with name of skill used when character fight
  without weapon, `{HealingTools}` which later will be replaced by name
  of item used to heal members of player faction.
- Inside help text you can use some tags for formatting text: 
  `{u}some text{/u}` for add underline for text, `{b}some text{/b}` to made
  bold characters and `{i}some text{/i}` for italic font.

## Goals

### General informations
- Open file *goals.dat* in *data/goals* directory or create new file with *dat*
  extension in that directory (example: *mygoals.dat*).

### Goal data structure
- Each goal starts with tag `goal`.
- Attribute `index` is index of goal (it can be number or text) and must be 
  unique. At this moment this value is used to set/update goal in game.
- Attribute `type`: Type/category of goal which define what is needed to do for
  finish selected goal. Possible values: Reputation - gain max reputation in X 
  bases, Destroy - destroy X ships, Discover - discover X fields of map, 
  Visit - visit (discover) X bases, Craft - craft X items, Mission - Finish X
  missions, Kill - Kill X enemies in melee combat.
- Attribute `amount`: Amount of target for selected goal to do for finish it.
  For example 100 ships to destroy, etc.
- Attribute `target`: Optional attribute, ignored for Discover type, exact 
  target for goal, depends on type of goal. Possible values: for Reputation 
  and Visit, bases owners names: Poleis, Independent, Pirates, Undead, Drones, 
  Inquisition. For Destroy, any enemy ship index (from *ships* directory) or
  owners names: Poleis, Independent, Pirates, Undead, Drones, Inquisition. For
  Craft, recipe index (from *recipes* directory), Type or ShowType (from
  *items* directory). For Mission, missions types: Deliver for deliver item to 
  bases, Destroy for destroy enemy ship, Patrol for patrol missions, Explore 
  for explore missions, Passenger for transport passengers missions. For Kill,
  enemy fraction name: Poleis, Independent, Pirates, Undead, Drones, 
  Inquisition.
- Attribute `multiplier`: Optional attribute, multiplier for amount of game 
  points earned by finishing this goal. Default value is 1, which mean goal
  give attribute `amount` of points for finish it.

## Mobiles

### General informations
- Open file *mobs.dat* in *data/mobs* directory or create new file with *dat*
  extension in that directory (example: *mymobs.dat*).

### Mob data structure
- Each mobile starts with tag `mobile`.
- Attribute `index` is is a mobile index (it can be number or text) and must be
  unique. At this moment this value is used to set crew on ships.
- Attribute `order`: current ship order of selected mob. Possible values are:
  Pilot, Engineer, Gunner, Repair, Craft, Upgrading, Talk, Heal, Clean, Rest,
  Defend, Boarding.
- Tag `skill` define skill of mobile. Attribute `name` is name of skill (from
  *game.dat* from *data* directory). If mobile should have constant level of 
  skill, add attribute `level` with level of selected skill. If mobile should 
  have random level of skill, add attribute `minlevel` with minimum level of 
  skill and attribute `maxlevel` with maximum level of skill.
- Tag `attribute` define attribute of mobile. If mobile should have constant
  level of attribute, add attribute `level` with level of selected attribute.
  If mobile should have random level of attribute, add attributes `minlevel`
  with minimum level of attribute and attribute `maxlevel` with maximum level
  of attribute.
- Tag `priority` define orders priorities of mobile. Attribute `name` can have
  value: Piloting, Engineering, Operating guns, Repair ship, Manufacturing,
  Upgrading ship, Talking in bases, Healing wounded, Cleaning ship, Defend ship,
  Board enemy ship. Attribute `value` can have value Normal or High (only
  one High per mobile).
- Tag `item` define item in mobile inventory. Attribute `index` is index of 
  item from files from *items* directory. If mobile should have constant amount
  of item, add attribute `amount` with amount of item. If mobile should have 
  random amount of item, add attribute `minamount` with minimum amount of item
  and attribute `maxamount` with maximum amount of item.
- Tag `equipment` define which items are used by mobile. Attribute `index` is
  item index from inventory. Item index always starts with 1. Attribute `slot`
  is name of equipment slot in which selected item is set. Possible values for
  `slot`: Weapon, Shield, Head, Torso, Arms, Legs, Tools.

## Factions

### General informations
- Open file *factions.dat* in *data/factions* directory or create new file 
  with *dat* extension in that directory (example: *myfactions.dat*).

### Faction data structure
- Each faction is between `faction` tags.
- Attribute `index`: index of faction (it can be number or text) and must be
  unique. At this moment this value is used to create bases during starting new
  game and to determine which ships are enemies or friends.
- Attribute `name`: name of factions displayed in game. Can be any text and
  unique.
- Attribute `membername`: name of single mobile from this faction. Can be any
  text.
- Attribute `pluralmembername`: plural of name of mobiles from this faction.
  Can be any text.
- Attributes `minspawn`, `maxspawn` and `spawn`: chance to that newly created 
  sky base will be owned by that faction. Value of this attributes is roll on 
  dice 100 and must be unique for each faction. If it should be only one roll, 
  use attribute `spawn`. If it should be range, use `minspawn` for minimum roll 
  and `maxspawn` for maximum roll.
- Attributes `population`, `minpopulation` and `maxpopulation`: starting 
  population of base owned by that faction. If it should be constant value, use
  attribute `population`. If it should be random value, use attribute 
  `minpopulation` for minimum population and `maxpopulation` for maximum 
  population. Minumum value is 0.
- Optional attribute `namestype`: Used in generating ship names of that faction
  and names of all bases. Can be `standard` (default value) or `robotic`.
- Attribute `playerindex`: index of mobile used for starting player character.
  Value must be existing mobile index from any mobiles file from *data/mobs*
  directory.
- Attribute `playershipindex`: index of ship used for starting player ship.
  Value must be existing ship index from any ships file form *data/ships*
  directory.
- Attribute `healingtools`: name of item type used to healing members of that
  faction and in diseased bases events. Must be valid item type from 
  *data/game.dat* file.
- Tags `relation`: Relation of this faction with other faction. All factions
  must have this tags for each faction (even for self).
- Attribute `faction` is faction index to which relation will be set. 
- Attributes `minreputation`, `maxreputation`, `reputation`: starting 
  reputation with selected faction. If it should have random reputation, use 
  attribute `minreputation` for minimum level of reputation and 
  `maxreputation` for maximum level of reputation. If it should be constant
  reputation, use attribute `reputation`. Minumum value is -100 and maximum
  is 100.
- Attribute `friendly`: did selected faction is friendly to this faction.
  Value `Y` means `Yes`, value `N` means `No`. Used mostly to generate 
  enemy ships.
- Tag `description`: In game description of item. Can have any value.
- Tags `foodtype`: Types of items used as food by this faction members. If
  no `foodtype` tags inside faction, that faction members can't be hungry.
- Attribute `name`: name of item type used as food. Must be valid item type
  from *data/game.dat* file.
- Tags `drinktype`: Types of items used as drink by this faction members. If
  no `drinktype` tags inside faction, that faction members can't be hungry.
- Attribute `name`: name of item type used as food. Must be valid item type
  from *data/game.dat* file.

## Stories

### General informations
- Open file *stories.dat* in *data/stories* directory or create new file 
  with *dat* extension in that directory (example: *mystories.dat*).

### Story data structure
-- Each story is between "story" tags.
- Attribute `index`: index of story (it can be number or text) and must be
  unique. At this moment this value is used to manage current story in which
  player is involved.
- Attribute `start`: condition which must be met to start that story. Possible
  values are: dropitem - story starts on drop selected item from enemies from
  selected faction.
- Attribute `minsteps`: minumum amount of steps in that story.
- Attribute `maxsteps`: maxiumum amount of steps in that story.
- Attribute `startstep`: index of step which will be used as first step in
story.
- Attribute `finalstep`: index of step which will be used as final step in
  story.
- Tags `startdata`: contains data needed for story starting condition. For
  "dropitem" it will be index of item which should drop, mob faction from which
  item will be dropped, chance (1 to that number) for drop.
- Tag `endtext`: text which will be show to player when he/she finish story.
- Tags `forbiddenfaction`: if player is in that faction, he can't start this
  story.
- Tag `step` contains data for step of story.
- Attribute `finish`: condition which must be met to finish this step. Possible
  values are: `askinbase` - go to next step when player ask about something in
  any or selected base, `destroyship` - go to next step when player destroy
  selected ship, `explore` - go to next step when player search selected map
  field.
- Tags `finishdata`: contains data needed for finish selected step. Attribute
  `name` is name of data. Possible values: `item` - item index (for `askinbase`
  and `loot` steps), `base` - ask in any base (value `any`) or randomly 
  selected (value `selected`) needed for `askinbase` steps. Names `faction` - 
  index of faction to which ship belongs, `ship` - index of ship which must be 
  destroyed (for `destroyship` and `loot` steps), `random` value if enemy ship
  should be selected randomly or `any` for any enemy ship (for `loot` step only).
  Names `x` and `y` are location on map where player must go to progress in 
  story. Value `random` mean randomly selected place on map or numeric 
  coordinates of map field. Both used by `destroyship` and `explore` steps. 
  Name `condition` is used by all steps and mean which skill should be used
  for check did step will progress to next, or value `random` for random
  chance. Name `chance` is used by all steps and mean chance (1 to that number
  for `random` condition or Skill + roll from 1 to 100) that step will 
  progress to next.
- Tag `text`: text which will be show to player when step starts. Attribute
  `condition`: finish condition of previous step which was lead to this one.
  Possible values: `any`, `askinbase` and `destroyship`.
- Tag `failtext`: text which will be show to player if step not progress to 
  next.

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

