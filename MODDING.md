Some general informations about adding/editing some game elements (skills,
ships, ships modules, etc)

## Skills

### General informations
- Open file *game.dat* in *data* directory and edit line which starts with
  SkillsNames.
- Each value for skills must be separated by comma and space: `, `
- Whole SkillsNames entry must be done in one line (if your text editor have
  automatic line wrap enabled, don't forget to fix it).

### Changing existing skills names
- You can change name of any existing skill, just remember, that first skill is
  always used by Pilot, second by Engineer, third by Gunner and forth by person
  who talk/trade in bases.
- After change skill name, you been need to made proper changes in other game
  data files (recipes, items)

### Adding new skills
- To add new skill, just append it name to SkillsNames entry.

## Items types

### General informations
- Open file *game.dat* in *data* directory and edit line which starts with
  ItemsTypes.
- Each value for item type must be separated by comma and space: `, `
- Whole ItemsTypes entry must be done in one line (if your text editor have
  automatic line wrap enabled, don't forget to fix it).

### Changing existing items types
- In most cases you can change Item Type, with few exceptions: 'Fuel', 'Food',
  'Drink', 'RawFood', 'MissionItem', 'Body', 'AlchemySet' and 'Medicines'. If 
  you change any of this item type, game may stop working.
- After change of Item Type don't forget to do proper changes in other data
  files (like items or recipes)

### Adding new items types
- To add new item type, just append it name to ItemsTypes entry.

## Items

### General informations
- Open file *items.dat* in *data* directory.
- Each value in item data structure must be one line length.
- File must end with `[]`.

### Item data structure
- Each item start from `[` symbol. Numbers between `[` and `]` are just for 
  easier counting items indexes. You can write any text between symbols.
- Name: name of item displayed in various places (cargo info, crafting, etc)
- Weight: weight of one item in kilograms
- Type: item type of item (from *game.dat* file, entry *ItemsTypes*)
- Prices: item base price in bases. Each value mean price in selected base
  type: Industrial, Agricultural, Refinery, Shipyard. If you want that item
  will be not possible to buy/sellable in bases (or only selected base type) 
  set it price to 0.
- Buyable: did item can be bough in selected type of base: Industrial,
  Agricultural, Refinery, Shipyard. Y mean yes, N means No.
- ShowType: optional parameter. If you want to show item type in game (for
  example in cargo or in trade screen) different than item type from *game.dat*
  file, you can set this parameter to any text value.
- Value: optional parameter. For 'Food' or 'Raw Food' it is value of hunger 
  reduced by one portion. For 'Drink', value of thirst reduced by one portion.
  For ammunition it is damage done by that ammunition. For working tools it is
  chance for item to be damaged during work.
- Description: Any length description of item.

## Recipes

### General informations
- Open file *recipes.dat* in *data* directory.
- Each value in recipe data structure must be one line length.
- File must end with `[]`.

### Recipe data structure
- Each recipe start from `[` symbol. Numbers between `[` and `]` are just for 
  easier counting recipes indexes. You can write any text between symbols.
- Material: Item type of materials need for recipe. Each material must be
  separated with `, ` (comma and space)
- Amount: Amount of crafting materials needed for recipe. If you set more than
  one material for recipe, you must set amount for each of them. Each material
  amount must be separated with `, ` (comma and space)
- Result: Item index which will be produced by recipe (you can check this index
  in *items.dat* file)
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
  for Refinery and 4 for Shipyard
- Time: optional parameter. How long in minutes take crafting selected recipe.
  If not set, it take 15 minutes.

## Ship modules

### General informations
- Open file *shipmodules.dat* in *data* directory.
- Each value in ship module data structure must be one line length.
- File must end with `[]`.

### Ship module data structure
- Each ship module start from `[` symbol. Numbers between `[` and `]` are just for 
  easier counting modules indexes. You can write any text between symbols.
- Name: Standard name of module. Will be visible in ship info screen and in
  shipyards.
- Type: Type of module. Available options are: Engine, Cabin, Cockpit, Turret,
  Gun, Cargo, Hull, Armor, Battering\_ram, Alchemy\_Lab, Furnace,
  Water\_Collector, Workshop, Greenhouse, Medical\_room
- Weight: Weight of module in kilograms.
- Value: Depends on type of module. For 'Engine' it is fuel usage for travel by
  one map field. For 'Cabin' it is value of reduced tiredness of owner who rest
  there. For 'Gun' it is index of item type used as ammunition (item types are
  in *game.dat* file). For any other type of modules should be 0 (zero).
- MaxValue: Depends on type of module. For 'Hull' it is max free module space.
  For 'Engine' it is engine power. For 'Cabin' should be that same like Value
  value. For 'Cargo' it is maximum capacity in kilograms for cargo for that
  module. For 'Gun' or 'Battering\_ram' it is amount of damage done by selected
  weapon. For any other type of modules should be 0 (zero).
- Durability: Base durability of module. How many damage module can take before
  will be destroyed.
- Material: Type of item which will be used to repair or upgrade module.
- Skill: Name of skill which will be used during repair or upgrading module.
- Price: Base buy or sell price of module in shipyard.
- InstallTime: How long in minutes take install/remove selected module from
  ship.
- Unique: If set to 'Yes' then each ship can have only one module of that type.
- Size: Amount of hull module space used by module.
- Description: Any length description of item.

## Ships
