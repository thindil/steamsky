Some general information about adding/editing game elements (skills,
ships, ships modules, etc.)

## General information about modifications
To add new things (items, ships, modules, etc) to the game, you must create
a new
directory inside the game's mod directory which is *data/mods* by default.

For example, create the directory *mymod* as *data/mods/mymod*
and place your work in a file or files with the extension *dat*
patterned similarly to the structure of the shipped files there.

For example, to add a new item to game, create a file which looks like
*data/items.dat*.

Below you will find more detailed information about the
XML data structures used by the program.

You may can edit the shipped files as well of course, but modification
you make there will be overwritten when you update the game.

## Items types

### General information
- Open file *game.dat* in *data* directory or even better, create a new file in
  modifications directory.
- A file providing new items must start with tag `data`. Each item type is one
  line entry with tag `itemtype`. Structure your work on what you see in
  the *data/game.dat* file.

### Changing an existing items types
- By editing the *data/game.dat* file you can change the name of an existing item type
  by changing the `value` attribute.
- When editing your custom file: you need to remove first selected item type, then add new.
- Item types are referenced in the specifications of items and recipes. Don't
  forget to change any references to your type in these files as well.

### Adding new items types
- Add a new item type, by appending a new line with tag `itemtype`. Store the
  Name of your new type in the `value` attribute.
- All Gun ammunition must have a name beginning with `Ammo`, for example:
  *Ammo150*.
- All Harpoon Gun Ammunition types must have a name beginning with `Harpoon`.

### Removing item types
- You can remove item types from *data/game.dat* by removing their tag.
- Alternatively, your mod may have a `remove` tag with the attributes `name`
  set to `itemtype` and `value` set to the name of type to be removed.
- After deleting item types, don't forget remove any references to it in other
  data files.

## Characters attributes

### General information
- To add/remove/change characters attributes, open the file *game.dat* in
  *data* directory or better, create a new file in modifications directory. New
  file must start with `data` tag.
- Each attribute starts with the tag `attribute`. Attribute `name` is the name of
  the selected attribute. Value between `attribute` tags is a description of
  the attribute. Example *<attribute name="Dexterity">Nimbleness of character,
  used in many crafts</attrbute>*.
- After changing/removing attribute/s, don't forget to make the proper changes in the game
  data.

### Changing an existing attribute
- If editing *game.dat* file: just change the selected value of the attribute.
- If editing your own file: you need to remove first selected attribute then add new.
- To edit the attribute name, just edit value of the attribute `name`.
- To change the attributes description, just edit the text between `attribute` tags.

### Adding a new attribute
- To add a new attribute, just append a new line with tag `attribute` with
  its name as attribute `name` and description between tags `attribute`. Same
  for *game.dat* file and your own file.

### Removing an existing attribute
- In *game.dat* file: Just delete the selected attribute entry.
- In own file: Append new line with tag `remove` which have attribute `name`
  set to `attribute` and value with name of attribute which will be removed.

## Skills

### General information
- Open the *game.dat* file in the *data* directory to add/remove/change skills, or
  even better, create a new file in the modifications directory.
- Each skill starts with tag `skill`.
- Attribute `name` is the name of the selected skill.
- Attribute `attribute` is the name of the character's attribute (must be defined
  earlier in this same file).
- Optional attribute `tool` is item type used as tool during training (must be
  defined earlier in this same file).
- After changing/removing the skill, you need to make the proper changes in other game
  data files (recipes, items and this same file) if needed.

### Changing an existing skills
- If editing *game.dat* file: just change the selected value of the skill/s.
- If editing a custom file: you need to remove the first selected skill and then add new.
- To change a skill's name, edit the value of `name` attribute of the selected skill.
- To change assigned attribute to selected skill enter the new name in attribute
  `attribute`. The Name must be an existing attribute name, defined earlier in
  *game.dat* file.
- To change skill description, just edit the text between `skill` tags.
- To change assigned tool, edit value of `tool` attribute of the selected skill.
  Tool must be an existing item type, defined earlier in *game.dat* file.

### Adding new skills
- To add a new skill, just append a new line with tag `skill` with its name as
  attribute `name`, assigned attribute's skill as attribute `attribute`,
  assigned item type's training tool as attribute `tool` and description between
  tags `skill`.

### Removing an existing skill
- In *game.dat* file: Delete the selected skill entry.
- In custom file: Append new line with tag `remove` which has attribute's `name`
  set to `skill` and value with name of skill which will be removed.

All changes below (to **Skills**) are made in *game.dat* file or custom file inside
modification directory. Custom file must start with `data` tag. If you want
overwrite default value in own file, just add new line with proper tag and
attributes.

## Repair tools type
To change which item type is used for repair/upgrading tools,  edit `value`
attribute of tag `repairtools`. Value must be an existing item type.

## Cleaning tools type
To change which item type is used for the cleaning ship, edit `value` attribute of
tag `cleaningrools`. Value must be an existing item type.

## Alchemy tools type
To change which item type is used for the deconstructing of items, edit `value` attribute
of tag `alchemytools`. Value must be an existing item type.

## Corpse index
To change which item is used as a body for dead, edit `value` attribute of tag
`corpseindex`. Value must be existing item index from any items file.

## Mission items type
To change which item type is used for delivery missions, edit `value` attribute
of tag `missionitemstype`. Value must be an existing item type.

## Fuel item type
To change which item type is used as a fuel for ship, edit `value` attribute of
tag `fueltype`. Value must be an existing item type.

## Money index
To change which item is used as currency in the game, edit `value` attribute of tag
`moneyindex`. Value must be an existing item index from any items file.

## Traders ships name
To change which word in ship names is used to determine a trader ship (needed for
friendly trader random event), edit `value` attribute for tag `tradersname`.
Value can be any single word (but this word must be in trader ships names).

## Condition attribute name
To change which attribute is used to raise a character's condition, edit attribute
`value` of tag `conditionname`. Value must be an existing attribute name.

## Strength attribute name
To change which attribute is used to count a character's maximum encumbrance, edit
attribute `value` of tag `strenghtname`. Value must be an existing attribute name.

## Piloting skill name
To change which skill is used for the piloting of ships, edit `value` attribute of tag
`pilotingskill`. Value must be an existing skill name.

## Engineering skill name
To change which skill is used for engineering duty, edit `value` attribute of
tag `engineeringskill`. Value must be an existing skill name.

## Gunnery skill name
To change which skill is used for the operation of guns on ships, edit `value`
attribute of tag `gunneryskill`. Value must be an existing skill name.

## Talking skill name
To change which skill is used for talking in bases or with other ships (trades,
repairs, recruit, etc), edit `value` attribute of tag `talkingskill`. Value
must be an existing skill name.

## Spotting skill name
To change which skill is used for spotting things (other ships, etc), edit
`value` attribute of tag `perceptionskill`. Value must be an existing skill name.

## Head armor type
To change which item type is used as a head armor for characters, edit `value`
attribute of tag `headarmor`. Value must be an existing item type.

## Torso armor type
To change which item type is used as a torso armor for characters, edit `value`
attribute of tag `chestarmor`. Value must be an existing item type.

## Arms armor type
To change which item type is used as arm armor for characters, edit `value`
attribute of tag `armsarmor`. Value must be an existing item type.

## Legs armor type
To change which item type is used as leg armor for characters, edit `value`
attribute of tag `legsarmor`. Value must be an existing item type.

## Shield type
To change which item type is used as a shield for characters, edit `value`
attribute of tag `shieldtype`. Value must be an existing item type.

## Weapon type
To change which item type is used as a weapon by characters, edit `value`
attribute of tag `weapontype`. Value must be an existing item type.

## Dodging skill name
To change which skill is used for dodging in combat, edit
`value` attribute of tag `dodgeskill`. Value must be an existing skill name.

## Unarmed skill name
To change which skill is used for chance to hit enemy in combat
when a character doesn't have a weapon, edit `value` attribute of tag `unarmedskill`.
Value must be an existing skill name.

## Items

### General information
- Default game items are in *items.dat* file which is in *data* directory.
- If you want remove or update any existing item, you can do it in *items.dat*
  file in *data* directory or in modification file (better option) add tag
  `item` with attribute `index` which value will be index of selected item and
  attribute `action`. Then if you modify an existing item, add changed values.

### Item data structure
- Each item starts with tag `item`.
- Attribute `index` is the item index (it can be a number or text) and must be
  unique. This value is used in ships and recipes data entries.
- Optional attribute `action`: What to do with this item. Possible values
  are: "add" (add this item, default option), "remove" (remove this item)
  or "update" (update selected item).
- `name` attribute: Name of item displayed in various places (cargo info,
  crafting, etc.)
- Attribute `weight`: Weight of one item in kilograms
- Attribute `type`: Item type of item (from *game.dat* file, entry
  *ItemsTypes*)
- Attribute `showtype`: optional attribute. If you want to show item type in
  game (for example in cargo or in trade screen) different than item type
  from *game.dat* file, you can set this parameter to any text value.
- Attribute `price` is price of the item. If you want that item will be not
  possible to buy or sell in bases, set it price to 0.
- Optional attribute `reputation`: Minimal reputation in bases needed to
  buy that item from them. Default value is -100 (available in all bases).
- Tag `data`: optional tag. Each tag is one value. For items used as food for example, it
  is the value of hunger reduced by one portion. For items used as drinks, it's the value of
  thirst reduced by one portion. For ammunition, the first value is damage done by
  that ammunition, second is ammunition type (1 - normal, 2 - piercing,
  3 - exploding). For working tools, it is the chance for the item to be damaged during
  work. For harpoon guns ammunition, it is how long (in combat turns) item will
  be stuck in the enemy ship. For weapons and armor pieces, the first value is the chance
  of the item being damaged during combat, second entry for weapons is damage
  done by weapon and for armor, it is the amount of damage reduced by this armor.
  Third entry for weapons is the number of skill used by this weapon (from
  *game.dat* file, entry *Skills*) and for armor is amount of levels of dodge
  skill which this armor reduces when worn. Forth entry for weapon is the amount
  of hands used (1 for one-handed, 2 for two-handed weapons). Fifth entry for
  weapon is damage type (1 - cutting damage, 2 - impaling damage, 3 - blunt
  damage).
- Tag `description`: In game description of the item. Can have any value.

## Recipes

### General information
- Default game crafting recipes are in *recipes.dat* file which is in *data*
  directory.
- If you want remove or update any existing recipe, you can do it in
  *recipes.dat* file in *data* directory or in modification file (better
  option) add tag `recipe` with attribute `index` which value will be index
  of selected recipe and attribute `action`. Then if you modify an existing
  recipe, add changed values.

### Recipe data structure
- Each recipe starts with tag `recipe`.
- Attribute `index` is a recipe index (it can be a number or text) and must be
  unique. This value is used at this moment for set starting recipes and in
  Craft types of goals.
- Optional attribute `action`: What to do with this recipe. Possible values
  are: "add" (add this recipe, default option), "remove" (remove this recipe)
  or "update" (update selected recipe).
- Tag `material` contains data about material used to craft recipe. If you want
  to add more materials to recipe, just add new tag `material` with proper
  data.
    - Attribute `type` is item type of material need for recipe.
    - Attribute `amount` is amount of crafting materials needed for recipe. Zero
      value for updating recipe means that this material should be removed.
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
- Attribute `time`: optional attribute. How long in minutes take crafting selected
  recipe. If not set, it take 15 minutes.

## Ship modules

### General information
- Default game ship modules are in *shipmodules.dat* file which is in *data*
  directory.
- If you want remove or update any existing ship module, you can do it in
  *shipmodules.dat* file in *data* directory or in modification file (better
  option) add tag `module` with attribute `index` which value will be index of
  selected ship module and attribute `action`. Then if you modify an existing ship
  module, add changed values.

### Ship module data structure
- Each ship module starts with tag `module`.
- Attribute `index` is a module index (it can be a number or text) and must be
  unique. This value is used in ships data entries.
- Optional attribute `action`: What to do with this module. Possible values
  are: "add" (add this module, default option), "remove" (remove this module)
  or "update" (update selected module).
- Attribute `name`: Standard name of module. Will be visible in ship info screen
  and in shipyards.
- Attribute `type`: Type of module. Available options are: Engine, Cabin, Cockpit,
  Turret, Gun, Cargo, Hull, Armor, Battering\_ram, Alchemy\_Lab, Furnace,
  Water\_Collector, Workshop, Greenhouse, Medical\_room, Harpoon\_Gun,
  Training\_Room
- Attribute `weight`: Weight of module in kilograms.
- Attribute `value`: Depends on type of module. For 'Engine' it is fuel usage
  for travel by one map field. For 'Cabin' it is the value of reduced tiredness of
  the owner who rests there. For 'Gun' or 'Harpoon\_Gun' it is the index of the item type
  used as ammunition (item types are in *game.dat* file). For 'Hull' it is
  maximum allowed size of installed modules. For any other type of modules
  should be 0.
- Attribute `maxvalue`: Depends on type of module. For 'Hull' it is max
  free module space. For 'Engine' it is engine power. For 'Cargo' it is maximum
  capacity in kilograms of cargo for that module. For 'Gun', 'Battering\_ram' it
  is the amount of damage done by selected weapon. For 'Harpoon\_Gun' it is amount of
  combat rounds of how long the harpoon is stuck in enemy ship. For `Cabin` it must
  be that same as "value" attribute.For any other type of modules should be 0
  (zero).
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
- Optional attribute `size`: Amount of hull module space used by module. For
  guns and harpoon guns it is minimum size of turret needed for that gun or
  harpoon gun.
- Optional attribute `maxowners`: How many owners or workers (for workshop's
  types of modules) the module can have. Default value is 1.
- Optional attribute `speed`: For guns, how many times in one combat round
  this gun shoots. Default value is 4. Values below 0 means that the gun shoot
  shoot once in few rounds. For example, -3 mean the gun shoot once per 3
  rounds.
- Optional attribute `reputation`: Minimal reputation in bases needed to
  buy that module from them. Default value is -100 (available in all bases).
- Text between `module` tags is module description.

## Ships

### General information
- Default game ships are in *ships.dat* file which is in *data* directory.
- If you want remove or update any existing ship, you can do it in *ships.dat*
  file in *data* directory or in modification file (better option) add tag
  `ship` with attribute `index` which value will be index of selected ship and
  attribute `action`. Then if you modify an existing ship, add changed values.

### Ship data structure
- Each ship is between `ship` tags.
- Attribute `index` is a index (it can be a number or text) and must be unique.
  This value is used at this moment to set player ship and in Destroy types of
  goals.
- Optional attribute `action`: What to do with this ship. Possible values
  are: "add" (add this ship, default option), "remove" (remove this ship)
  or "update" (update selected ship).
- Attribute `name`: Type of ship. Will be visible during combat information.
  If you want that ship will be used in friendly trader random event, you must
  have word which you set in *game.dat* as *TraderNames* in ship name. Example:
  if you use *trader* word, ship name can be *small poleis trader*.
- Tags `module`: List of ship modules installed on selected ship.
    - Attribute`index` is module index from files from *shipmodules* directory.
    - Attribute `amount` is optional: If ship should have more than one that
      module, just add attribute `amount` with number of modules.
    - Optional attribute `action`: What to do with this module. Possible values
      are: "add" (add this module, default option) or "remove" (remove this
      module).
- Attribute `accuracy`: Bonus to accuracy for ship.
- Attributes `minaccuracy` and `maxaccuracy`: If bonus to accuracy for ship
  should be random, add attribute `minaccuracy` for minimum value and
  `maxaccuracy` for maximum value.
- Attribute `combatai`: Behavior of ship in combat (NPC ships only). Possible
  values are: Berserker - attacks all time, no matter how heavy damage it take.
  Attacker - aggressive but will be run away from combat when lost all
  ammunition or weapons. Coward - try run from combat, attacks only in
  self-defense. Disarmer - same as Attacker but first aim for player ship
  weapons before start destroying ship and it shoot with lower fire rate than
  others AI's. Additionally, each behavior determine when the enemy will start
  escaping from the combat, from fastest to longest: Disarmer, Attacker,
  Berserker.
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
- Tags `cargo`: List of items in cargo of ship.
    - Attribute `index` is index of item from files from *items* directory. If
      amount of that item should be constant, add attribute `amount` with
      proper value. If amount of that item should be random, add attributes
      `minamount` with minimum amount and attribute `maxamount` with maximum
      amount of that item.
    - Optional attribute `action`: What to do with this item. Possible values
      are: "add" (add this item, default option), "remove" (remove this item)
      or "update" (update selected item).
- Tag `description`: Description of ship (NPC ships only). Will be displayed
  during combat.
- Attribute `owner`: Which fraction own ship. Possible values are: Poleis,
  Independent, Pirates, Undead, Drones, Inquisition.
- Tags `recipes`: List of know recipes. (player ships only).
    - Attribute `index` is recipe index from files from *recipes* directory.
    - Optional attribute `action`: What to do with this recipe. Possible values
      are: "add" (add this recipe, default option) or "remove" (remove this
      recipe).
- Tags `member`: List of crew members.
    - Attribute `index` is mobile index from files form *mobs* directory. If
      ship should have more than one that same mobile if crew, add attribute
      `amount`. If ship should have more than one that same mobile and amount
      should be random, add attributes `minamount` for minimum amount of that
      mobile and attribute `maxamount` for maximum amount of that mobile.
    - Optional attribute `action`: What to do with this memeber. Possible
      values are: "add" (add this member, default option), "remove" (remove
      this member) or "update" (update selected member).

## Help

### General information
- Default game help entries are in *help.dat* file which is in *data* directory.
- If you want remove or update any existing help topic, you can do it in
  *help.dat* file in *data* directory or in modification file (better option)
  add tag `entry` with attribute `index` which value will be index of selected
  help topic and attribute `action`. Then if you modify an existing help topic,
  add changed values.

### Help data structure
- Each help entry is between `entry` tags.
- Attribute `index` is the help index, used mostly in showing help for
  selected part of the game. It can be a number or text.
- Attribute `title` is help menu entry in main help menu. It can be number or
  text.
- Optional attribute `action`: What to do with this entry. Possible values
  are: "add" (add this entry, default option), "remove" (remove this entry)
  or "update" (update selected entry).
- Text between tags `entry` is help entry text text visible when player select
  this option from help menu.
- Inside help text you can use special variables which later will be replaced
  by proper keys names. All  that variables have name `{GameKey [number]}`
  where `[number]` is between 1 and 29 (example: `{GameKey 9}`). Proper game
  keys in order: move ship up/left, move ship up, move ship up/right, move
  ship left, move ship one field or wait 1 minute, move ship right, move ship
  down/left, move ship down, move ship down/right, move ship to destination,
  show ship info, show cargo info, show crew info, show ship orders, show
  crafting menu, show last messages, show know bases, show known events, show
  accepted missions, move map position, show game statistics, show help, show
  game options, quit from game, resign from game, show menu, show wait orders,
  zoom out, zoom in. Example: `{GameKey 10}` will be translated to key used
  for auto move ship.
- Inside help text you can use special variables: `{MoneyName}` which later
  will be replaced with name of game money, `{FuelName}` which later will be
  replaced with name of fuel for ship, `{StrengthName}` which later will be
  replaced with name of attribute used to count max character encumbrance,
   `{PilotingSkill}` which later will be replaced with name
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
  without weapon.
- Inside help text you can use some tags for formatting text:
  `{u}some text{/u}` for add underline for text, `{b}some text{/b}` to made
  bold characters and `{i}some text{/i}` for italic font.
- Inside help text you can use special variables `diseaseimmune`, `nofatigue`,
  `nomorale`, `naturalarmor`,`toxicattack`, `sentientships`, `fanaticism`,
  `loner` which later will be replaced by list of factions which have set
  selected flag.
- Inside help text you can use special variables `shipyard`, `temple`,
  `blackmarket` which later will be replaced by list of bases types which
  have set selected flag.

## Goals

### General information
- Default game goals are in *goals.dat* file which is in *data* directory.
- If you want remove or update any existing goal, you can do it in *goals.dat*
  file in *data* directory or in modification file (better option) add tag
  `goal` with attribute `index` which value will be index of selected goal and
  attribute `action`. Then if you modify an existing goal, add changed values.

### Goal data structure
- Each goal starts with tag `goal`.
- Attribute `index` is index of goal (it can be a number or text) and must be
  unique. At this moment this value is used to set/update goal in game.
- Optional attribute `action`: What to do with this goal. Possible values
  are: "add" (add this goal, default option), "remove" (remove this goal)
  or "update" (update selected goal).
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

### General information
- Default game mobiles are in *mobs.dat* file which is in *data* directory.
- If you want remove or update any existing mobile, you can do it in *mobs.dat*
  file in *data* directory or in modification file (better option) add tag
  `mobile` with attribute `index` which value will be index of selected mobile
  and attribute `action`. Then if you modify an existing mobile, add changed
  values.

### Mob data structure
- Each mobile starts with tag `mobile`.
- Attribute `index` is is a mobile index (it can be a number or text) and must be
  unique. At this moment this value is used to set crew on ships.
- Optional attribute `action`: What to do with this mobile. Possible values
  are: "add" (add this mobile, default option), "remove" (remove this mobile)
  or "update" (update selected mobile).
- Attribute `order`: current ship order of selected mob. Possible values are:
  Pilot, Engineer, Gunner, Repair, Craft, Upgrading, Talk, Heal, Clean, Rest,
  Defend, Boarding.
- Tag `skill` define skill of mobile.
    - Attribute `name` is name of skill (from *game.dat* from *data*
      directory). If name is set to "WeaponSkill" it will be later replaced by
      proper weapon skill depending on mobile faction.
    - Optional attribute `action`: What to do with this skill. Possible values
      are: "add" (add this skill, default option), "remove" (remove this skill)
      or "update" (update selected skill).
    - If mobile should have constant level of skill, add attribute `level` with
      level of selected skill. If mobile should have random level of skill, add
      attribute `minlevel` with minimum level of skill and attribute `maxlevel`
      with maximum level of skill.
- Tag `attribute` define attribute of mobile. If mobile should have constant
  level of attribute, add attribute `level` with level of selected attribute.
  If mobile should have random level of attribute, add attributes `minlevel`
  with minimum level of attribute and attribute `maxlevel` with maximum level
  of attribute. If you want update attribute of mob, you must add all
  attributes values.
- Tag `priority` define orders priorities of mobile. Attribute `name` can have
  value: Piloting, Engineering, Operating guns, Repair ship, Manufacturing,
  Upgrading ship, Talking in bases, Healing wounded, Cleaning ship, Defend ship,
  Board enemy ship. Attribute `value` can have value Normal or High (only
  one High per mobile).
- Tag `item` define item in mobile inventory.
    - Attribute `index` is index of item from files (from *items.dat* file from
      *data* directory).
    - If mobile should have constant amount of item, add attribute `amount`
      with amount of item. If mobile should have random amount of item, add
      attribute `minamount` with minimum amount of item and attribute
      `maxamount` with maximum amount of item.
    - Optional attribute `action`: What to do with this item. Possible values
      are: "add" (add this item, default option), "remove" (remove this item)
      or "update" (update selected item).
- Tag `equipment` define which items are used by mobile. If mob will not have
  set an item in equipment slot, it will be generated randomly based on its
  skills and faction. In that situation, here is small chance that mob will
  not have any equipment in selected slot.
    - Attribute `index` is item index from inventory. Item index always starts
      with 1.
    - Attribute `slot` is name of equipment slot in which selected item is
      set. Possible values for `slot`: Weapon, Shield, Head, Torso, Arms,
      Legs, Tools.

## Factions

### General information
- Default game factions are in *factions.dat* file which is in *data* directory.
- If you want remove or update any existing faction, you can do it in
  *factions.dat* file in *data* directory or in modification file (better
  option) add tag `faction` with attribute `index` which value will be index
  of selected recipe and attribute `action`. Then if you modify an existing
  recipe, add changed values.

### Faction data structure
- Each faction is between `faction` tags.
- Attribute `index`: Index of faction (it can be a number or text) and must be
  unique. At this moment this value is used to create bases during starting new
  game and to determine which ships are enemies or friends.
- Optional attribute `action`: What to do with this faction. Possible values
  are: "add" (add this faction, default option), "remove" (remove this faction)
  or "update" (update selected faction).
- Attribute `name`: Name of factions displayed in game. Can be any text and
  unique.
- Attribute `membername`: Name of single mobile from this faction. Can be any
  text.
- Attribute `pluralmembername`: Plural of name of mobiles from this faction.
  Can be any text.
- Attributes `spawn`: chance to that newly created sky base will be owned by
  that faction. Value of this attributes is roll on dice with sum of `spawn`
  attributes of all factions. For example if there will be 2 factions, one with
  `spawn` = 50 and second with `spawn` = 20 then if roll on dice 70 will be
  equal or lower to 50 it will be first faction base, when between 51 and 70 it
  will be second faction base.
- Attributes `population`, `minpopulation` and `maxpopulation`: starting
  population of base owned by that faction. If it should be constant value, use
  attribute `population`. If it should be random value, use attribute
  `minpopulation` for minimum population and `maxpopulation` for maximum
  population. Minimum value is 0.
- Optional attribute `namestype`: Used in generating ship names of that faction
  and names of all bases. Can be `standard` (default value) or `robotic`.
- Attribute `healingtools`: Name of item type used to healing members of that
  faction and in diseased bases events. Must be valid item type from
  *data/game.dat* file.
- Attribute `healingskill`: Name of skill used to healing members of that
  faction. Must be valid skill name from *data/game.dat* file
- Attribute `baseicon`: hexadecimal number of character used as base icon for
  this faction on map. Must be valid UTF-8 character from current map font.
- Attribute `weaponskill`: Name of skill skill used by favorite weapon of that
  faction. Must be valid skill name from *data/game.dat* file.
- Tags `relation`: Relation of this faction with other faction. All factions
  must have this tags for each faction (even for self).
    - Optional attribute `action`: What to do with this relation. Possible
      values are: "add" (add this relation, default option) or "update" (update
      this relation).
    - Attribute `faction` is faction index to which relation will be set.
    - Attributes `minreputation`, `maxreputation`, `reputation`: starting
      reputation with selected faction. If it should have random reputation,
      use attribute `minreputation` for minimum level of reputation and
      `maxreputation` for maximum level of reputation. If it should be constant
      reputation, use attribute `reputation`. Minimum value is -100 and maximum
      is 100.
    - Attribute `friendly`: did selected faction is friendly to this faction.
      Value `Y` means `Yes`, value `N` means `No`. Used mostly to generate
      enemy ships.
- Tag `description`: In game description of item. Can have any value.
- Tags `foodtype`: Types of items used as food by this faction members. If
  no `foodtype` tags inside faction, that faction members can't be hungry.
    - Attribute `name`: Name of item type used as food. Must be valid item type
      from *data/game.dat* file.
    - Optional attribute `action`: What to do with this food. Possible values
      are: "add" (add this food, default option) or "remove" (remove this
      food).
- Tags `drinktype`: Types of items used as drink by this faction members. If
  no `drinktype` tags inside faction, that faction members can't be hungry.
    - Attribute `name`: Name of item type used as food. Must be valid item type
      from *data/game.dat* file.
    - Optional attribute `action`: What to do with this drink. Possible values
      are: "add" (add this drink, default option) or "remove" (remove this
      drink).
- Tags `flags`: Various info about faction (optional tag).
    - Attribute `name`: Name of flag. Possible values: `nogender` faction don't
      have genders and use male names as default, `diseaseimmune` faction
      members cannot become ill, no disease event for this faction, `nofatigue`
      faction members don't get tired so, they can't rest and regenerate
      health, `nomorale` faction members don't have morale (and bonuses from
      it), `naturalarmor` faction members get half of damage in melee combat,
      `toxicattack` faction members do more damage when attack without weapon
      to enemies who are not resistant to diseases (factions without
      `diseaseimmune` flag), `sentientships` faction ships don't need pilot and
      engineer (they are optional), `fanaticism` faction members gain faster
      morale and lose it slower than other factions, `loner` bases of that
      faction are not grouped near self.
    - Optional attribute `action`: What to do with this flag. Possible values
      are: "add" (add this flag, default option) or "remove" (remove this
      flag).
- Tags `career`: Available careers for player when choice this faction. Text
  between tags is description of career visible in new game setting. If faction
  don't have any available career then it is unavailable for player.
    - Attribute `index`: Index of career. Must be an existing career index from
      *data/careers.dat* file.
    - Optional attribute `action`: What to do with this career. Possible values
      are: "add" (add this career, default option), "remove" (remove this
      career) or "update" (update selected career).
    - Attribute `playerindex`: Index of mobile used for starting player
      character, when he/she choice this career. Value must be an existing mobile
      index from any mobiles file.
    - Attribute `shipindex`: Index of ship used for starting player ship when
      he/she choice this career. Value must be an existing ship index from any
      ships file.
    - Attribute `name`: optional attribute. Specific name of career for this
      faction. Will be show to player instead of default name.
- Tags `basetype`: Available base types for this faction. If you don't add
  any that tag, all bases types available in the game will be available for
  that faction.
    - Attribute `index`: Index of the base type from *data/bases.dat* file.
    - Attribute `chance`: chance to that newly created sky base will be this
      type. Value of this attribute is roll on dice with sum of "chance"
      attributes of all bases types available for this faction. For example if
      there will be 2 base types, one with "chance" = 50 and second with
      "chance" = 20 then if roll on dice 70 will be equal or lower to 50 it
      will be first base type, when between 51 and 70 it will be second base
      type.

## Stories

### General information
- Default game stories are in *stories.dat* file which is in *data* directory.
- If you want remove or update any existing story, you can do it in
  *stories.dat* file in *data* directory or in modification file (better
  option) add tag `story` with attribute `index` which value will be index of
  selected ship and attribute `action`. Then if you modify an existing ship, add
  changed values.

### Story data structure
- Each story is between "story" tags.
- Attribute `index`: Index of story (it can be a number or text) and must be
  unique. At this moment this value is used to manage current story in which
  player is involved.
- Optional attribute `action`: What to do with this story. Possible values
  are: "add" (add this story, default option), "remove" (remove this story)
  or "update" (update selected story).
- Attribute `start`: condition which must be met to start that story. Possible
  values are: dropitem - story starts on drop selected item from enemies from
  selected faction.
- Attribute `minsteps`: minimum amount of steps in that story.
- Attribute `maxsteps`: maximum amount of steps in that story.
- Attribute `startstep`: Index of step which will be used as first step in
story.
- Attribute `finalstep`: Index of step which will be used as final step in
  story.
- Tags `startdata`: contains data needed for story starting condition. For
  "dropitem" it will be index of item which should drop, mob faction from which
  item will be dropped, chance (1 to that number) for drop.
    - Optional attribute `action`: What to do with this entry. Possible values
      are: "add" (add this entry, default option) or "remove" (remove this
      entry).
- Tag `endtext`: text which will be show to player when he/she finish story.
- Tags `forbiddenfaction`: If player is in that faction, he can't start this
  story.
    - Optional attribute `action`: What to do with this entry. Possible values
      are: "add" (add this entry, default option) or "remove" (remove this
      entry).
- Tag `step` contains data for step of story.
    - Attribute `finish`: condition which must be met to finish this step.
      Possible values are: `askinbase` - go to next step when player ask about
      something in any or selected base, `destroyship` - go to next step when
      player destroy selected ship, `explore` - go to next step when player
      search selected map field.
- Tags `finishdata`: contains data needed for finish selected step.
    - Attribute `name` is name of data. Possible values: `item` - item index
      (for `askinbase` and `loot` steps), `base` - ask in any base (value
      `any`) or randomly selected (value `selected`) needed for `askinbase`
      steps. Names `faction` - index of faction to which ship belongs, `ship` -
      index of ship which must be destroyed (for `destroyship` and `loot`
      steps), `random` value if enemy ship should be selected randomly or `any`
      for any enemy ship (for `loot` step only). Names `x` and `y` are location
      on map where player must go to progress in story. Value `random` mean
      randomly selected place on map or numeric coordinates of map field. Both
      used by `destroyship` and `explore` steps. Name `condition` is used by
      all steps and mean which skill should be used for check did step will
      progress to next, or value `random` for chance. Name `chance` is
      used by all steps and mean chance (1 to that number for `random`
      condition or Skill + roll from 1 to 100) that step will progress to next.
    - Optional attribute `action`: What to do with this entry. Possible values
      are: "add" (add this entry, default option), "remove" (remove this entry)
      or "update" (update selected entry).
- Tag `text`: text which will be show to player when step starts.
    - Attribute `condition`: finish condition of previous step which was lead
      to this one. Possible values: `any`, `askinbase` and `destroyship`.
    - Optional attribute `action`: What to do with this text. Possible values
      are: "add" (add this text, default option), "remove" (remove this text)
      or "update" (update selected text).
- Tag `failtext`: text which will be show to player if step not progress to
  next.

## Careers

### General information
- Default game careers are in *careers.dat* file which is in *data* directory.
- If you want remove or update any existing career, you can do it in
  *careers.dat* file in *data* directory or in modification file (better
  option) add tag `career` with attribute `index` which value will be index
  of selected career and attribute `action`. Then if you modify an existing
  career, add changed values.

### Career data structure
- Each career is between "career" tags.
- Attribute `index` is index of career.
- Optional attribute `action`: What to do with this career. Possible values
  are: "add" (add this career, default option), "remove" (remove this career)
  or "update" (update selected career).
- Attribute `name` is name of career visible to player.
- Each career can have bonuses to experience to certain skills. Each that
  skill is between `skill` tag.
    - Attribute `name` is name of skill which will be have bonuses to
      experience.
    - Optional attribute `action`: What to do with this skill. Possible values
      are: "add" (add skill, default option) or "remove" (remove this skill
      from bonus list).

## Bases Types

### General information
- Default base types are in *bases.dat* file which is in *data* directory.
- If you want to remove or update any existing base type, you can do it in
  *bases.dat* file in *data* directory or in modification file (better option)
  add tag `base` with attribute `index` which value will be index of
  selected base type and attribute `action`. Then if you modify an existing
  base type, add changed values.

### Bases Types data structure
- Each base type is between "base" tags.
- Attribute `index` is a base type index and must be unique for each base
  type. It can be a number or text. Required
- Attribute `name` is a base type name showed to player in game. Should be
  unique too. Required.
- Attribute `color` is a base type color in hexadecimal used to show this
  base type to player on the map in game. Required.
- Optional attribute `action`: What to do with this entry. Possible values
  are: "add" (add this entry, default option), "remove" (remove this entry) or
  "update" (update selected entry).
- Tag `item` is used to set prices for selected item in that base.
   - Attribute `index`: Item index from "items.dat" file. Required.
   - Attribute `sellprice`: Price for which item is sell in that type of base.
     If you want make selected item buyable in that type of base, you must add
     this attribute.
   - Attribute `buyprice`: Price for which item will be bought in that type of
     base. If you want item to have special price in selected type of base you
     must add this attribute.
- Tag `recipe` is used to set which recipes are available to buy in that type
  of bases.
   - Attribute `index`: Index of the recipe to buy, from file "recipes.dat".
     Required.
- Tag `description`: In game description of selected type of bases. Will be
  presented to the player during selection of starting base in main menu.
- Tag `flags` is various info about selected base type. Possible options are:
  `shipyard` - that base type have shipyard. Ship repairs in that base are 50%
  cheaper, `temple` - that base type allow action `Pray` which raise player
  and the player ship crew members morale, additionally, healing wounded crew
  members in that base is twice time cheaper, `blackmarket` - that base type
  have random cargo (and normal cargo too, if defined) for sale, additionally,
  all sold items here can be buy again, `barracks` - that base type have more
  recruits available and they are cheaper to hire plus they have lower daily
  payment.

## Themes

### General information
- To edit default game theme, open file *steamsky.css* in *data/ui* directory.
- To create new theme, first create new directory in *themes* directory
  (by default it is *data/themes* but you can change it with game starting
  parameter, see [README.md](../README.md) for more info about it). Next,
  inside new theme directory, create two files: one with extension
  *.cfg*(theme configuration file) and second with extension *.css*. Their can
  have any names.
- To remove theme just delete theme directory from *themes* directory.
- More information about variables and settings is in default game theme file
  *data/ui/steamsky.css*.
- More information about how CSS works with GTK (GUI library used by the game)
  is available at:
  https://developer.gnome.org/gtk3/stable/chap-css-overview.html

### Theme configuration options
- All theme configuration options are standard configuration pairs
  `key = value`. Example: `Name = Default theme`.
- Each configuration pair must be in one line.
- Key `Name`: Name of theme. Will be displayed to player in game options when
  user will be selecting game theme.
- Key `FileName`: Name of *.css* file for theme.
- Key `EnemyShipIcon`: Hexadecimal number on UTF-8 character used to show
  Enemy Ship event on map. Optional.
- Key `AttackOnBaseIcon`: Hexadecimal number on UTF-8 character used to show
  Attack on Base event on map. Optional.
- Key `DiseaseIcon`: Hexadecimal number on UTF-8 character used to show
  Disease in Base event on map. Optional.
- Key `DoublePriceIcon`: Hexadecimal number on UTF-8 character used to show
  Double Price on Item in Base event on map. Optional.
- Key `FullDocksIcon`: Hexadecimal number on UTF-8 character used to show
  Full Docks in Base event on map. Optional.
- Key `EnemyPatrolIcon`: Hexadecimal number on UTF-8 character used to show
  Enemy Patrol event on map. Optional.
- Key `TraderIcon`: Hexadecimal number on UTF-8 character used to show
  Trader ship event on map. Optional.
- Key `FriendlyShipIcon`: Hexadecimal number on UTF-8 character used to show
  Friendly Ship event on map. Optional.
- Key `DeliverIcon`: Hexadecimal number on UTF-8 character used to show
  Deliver type mission on map. Optional.
- Key `DestroyIcon`: Hexadecimal number on UTF-8 character used to show
  Destroy Ship type mission on map. Optional.
- Key `PatrolIcon`: Hexadecimal number on UTF-8 character used to show
  Patrol Area type mission on map. Optional.
- Key `ExploreIcon`: Hexadecimal number on UTF-8 character used to show
  Explore Area type mission on map. Optional.
- Key `PassengerIcon`: Hexadecimal number on UTF-8 character used to show
  Transport Passenger type mission on map. Optional.
- Key `PilotIcon`: Hexadecimal number on UTF-8 character used to show
  Pilot position info. Optional.
- Key `EngineerIcon`: Hexadecimal number on UTF-8 character used to show
  Engineer position info. Optional.
- Key `GunnerIcon`: Hexadecimal number on UTF-8 character used to show
  Gunners position info. Optional.
- Key `CrewTraderIcon`: Hexadecimal number on UTF-8 character used to show
  Trader position info. Optional.
- Key `RepairIcon`: Hexadecimal number on UTF-8 character used to show
  Repair Ship info. Optional.
- Key `UpgradeIcon`: Hexadecimal number on UTF-8 character used to show
  Upgrade Ship info. Optional.
- Key `CleanIcon`: Hexadecimal number on UTF-8 character used to show
  Clean Ship info. Optional.
- Key `ManufactureIcon`: Hexadecimal number on UTF-8 character used to show
  Manufacturing info. Optional.
- Key `MoveMapUpIcon`: Hexadecimal number on UTF-8 character used to show
  on move map up button. Optional.
- Key `MoveMapDownIcon`: Hexadecimal number on UTF-8 character used to show
  on move map down button. Optional.
- Key `MoveMapLeftIcon`: Hexadecimal number on UTF-8 character used to show
  on move map left button. Optional.
- Key `MoveMapRightIcon`: Hexadecimal number on UTF-8 character used to show
  on move map right button. Optional.
- Key `NoFuelIcon`: Hexadecimal number on UTF-8 character used to show
  warning about lack of fuel. Optional.
- Key `NoFoodIcon`: Hexadecimal number on UTF-8 character used to show
  warning about lack of food. Optional.
- Key `NoDrinksIcon`: Hexadecimal number on UTF-8 character used to show
  warning about lack of drinks. Optional.
- Key `NotVisitedBaseIcon`: Hexadecimal number on UTF-8 character used to show
  not visited bases on map. Optional.
- Key `PlayerShipIcon`: Hexadecimal number on UTF-8 character used to show
  player ship on map. Optional.
- Key `EmptyMapIcon`: Hexadecimal number on UTF-8 character used to show
  empty spaces on map. Optional.
- Key `TargetIcon`: Hexadecimal number on UTF-8 character used to show
  current player ship destination on map. Optional.
- Key `StoryIcon`: Hexadecimal number on UTF-8 character used to show
  current story even location on map. Optional.
- Key `OverloadedIcon`: Hexadecimal number on UTF-8 character used to show
  warning about overloaded ship. Optional.
- Key `CheckButtonUnchecked`: Name of image file used as a unchecked button
  (for example, in crew member inventory).
- Key `CheckButtonChecked`: Name of image file used as a checked button
  (for example, in crew member inventory).

## Debugging
If you want test your changes, you may run game in debug mode. In this mode
game create file *debug.log* in *data* directory. To start game in debug mode
run it with parameter --debug=[debugtype]. Example:

`./steamsky --debug=everything`

At this moment available are three types of debug:

- Option 'everything' means to log each debug message to a file. At this moment
  this means information about loading game data (ships/recipes/items/modules)
  and also some combat data. This option may be useful to check that the data
  of new/added items, etc is correct. Additionally its spawns debug menu for modifying the
  game data.
- Option 'combat' means to log debug messages only from combat.
- Option 'menu' spawns only debug menu after starting the game for modifying the
  data.

