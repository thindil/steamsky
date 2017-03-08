Some general informations about adding/editing some game elements (skills,
ships, ships modules, etc)

## Skills

### General informations
- Open file *game.dat* in *data* directory and edit line which starts with
  SkillsNames.
- Each value for skills must be separated by coma and space: `, `
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
- Each value for item type must be separated by coma and space: `, `
- Whole ItemsTypes entry must be done in one line (if your text editor have
  automatic line wrap enabled, don't forget to fix it).

### Changing existing items types
- In most cases you can change Item Type, with few exceptions: 'Fuel', 'Food',
  'Drink', 'RawFood', 'MissionItem', 'Body' and 'AlchemySet'. If you change any
  of this item type, game may stop working.
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

## Ship modules

## Ships
