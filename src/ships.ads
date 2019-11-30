--    Copyright 2016-2019 Bartek thindil Jasicki
--
--    This file is part of Steam Sky.
--
--    Steam Sky is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    Steam Sky is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;
with DOM.Readers; use DOM.Readers;
with Crew; use Crew;
with Game; use Game;
with Items; use Items;
with Mobs; use Mobs;

-- ****h* Steamsky/Ships
-- FUNCTION
-- Provides code for manipulate ships
-- SOURCE
package Ships is
-- ****

   -- ****t* Ships/ShipSpeed
   -- FUNCTION
   -- Ship speed states
   -- SOURCE
   type ShipSpeed is
     (DOCKED, FULL_STOP, QUARTER_SPEED, HALF_SPEED, FULL_SPEED);
   -- ****

   -- ****t* Ships/ShipCombatAi
   -- FUNCTION
   -- NPC ships combat AI types
   -- SOURCE
   type ShipCombatAi is (NONE, BERSERKER, ATTACKER, COWARD, DISARMER);
   -- ****

   -- ****t* Ships/ShipUpgrade
   -- FUNCTION
   -- Player ship types of module upgrades
   -- SOURCE
   type ShipUpgrade is (NONE, DURABILITY, MAX_VALUE, VALUE);
   -- ****

   -- ****t* Ships/Data_Array
   -- FUNCTION
   -- Used to store ship modules data
   -- SOURCE
   type Data_Array is array(1 .. 3) of Integer;
   -- ****

   -- ****t* Ships/ModuleType2
   -- FUNCTION
   -- Types of ships modules
   -- SOURCE
   type ModuleType2 is
     (WORKSHOP, ANY, MEDICAL_ROOM, TRAINING_ROOM, ENGINE, CABIN, COCKPIT,
      TURRET, GUN, CARGO_ROOM, HULL, ARMOR, BATTERING_RAM, HARPOON_GUN);
   -- ****

   -- ****t* Ships/ModuleData
   -- FUNCTION
   -- Data structure for ship modules, medical room, cockpit, armor and cargo
   -- bays don't have any special fields
   -- PARAMETERS
   -- Name             - Name of module
   -- ProtoIndex       - Index of module prototype
   -- Weight           - Weight of module
   -- Durability       - 0 = destroyed
   -- MaxDurability    - Base durability
   -- Owner            - Crew member indexes for owners of module
   -- UpgradeProgress  - Progress of module upgrade
   -- UpgradeAction    - Type of module upgrade
   -- FuelUsage        - Amount of fuel used for each move on map
   -- Power            - Power of engine used for counting ship speed
   -- Disabled         - Did engine is disabled or not
   -- Cleanliness      - Cleanliness of selected cabin
   -- Quality          - Quality of selected cabin
   -- GunIndex         - Index of installed gun
   -- Damage           - Damage bonus for selected gun
   -- AmmoIndex        - Cargo index of ammunition used by selected gun
   -- InstalledModules - Amount of installed modules on ship
   -- MaxModules       - Amount of maximum installed modules for this hull
   -- CraftingIndex    - Index of crafting recipe or item which is
   --                    deconstructed
   -- CraftingTime     - Time needed to finish crating order
   -- CraftingAmount   - How many times repeat crafting order
   -- TrainedSkill     - Index of skill set to training
   -- Damage2          - Damage done by battering ram
   -- CoolingDown      - If true, battering ram can't attack
   -- Duration         - Duration bonus for selected harpoon gun
   -- HarpoonIndex     - Cargo index of ammunition used by selected harpoon
   --                    gun
   -- Data             - Various data for module (depends on module)
   -- SOURCE
   type ModuleData(MType: ModuleType2 := ANY) is record
      Name: Unbounded_String;
      ProtoIndex: Unbounded_String;
      Weight: Natural;
      Durability: Integer;
      MaxDurability: Integer;
      Owner: Natural_Container.Vector;
      UpgradeProgress: Integer;
      UpgradeAction: ShipUpgrade;
      case MType is
         when ENGINE =>
            FuelUsage: Positive;
            Power: Positive;
            Disabled: Boolean;
         when CABIN =>
            Cleanliness: Natural;
            Quality: Natural;
         when TURRET =>
            GunIndex: Natural;
         when GUN =>
            Damage: Positive;
            AmmoIndex: Natural;
         when HULL =>
            InstalledModules: Natural;
            MaxModules: Positive;
         when WORKSHOP =>
            CraftingIndex: Unbounded_String;
            CraftingTime: Natural;
            CraftingAmount: Natural;
         when MEDICAL_ROOM | COCKPIT | ARMOR | CARGO_ROOM =>
            null;
         when TRAINING_ROOM =>
            TrainedSkill: Natural;
         when BATTERING_RAM =>
            Damage2: Positive;
            CoolingDown: Boolean;
         when HARPOON_GUN =>
            Duration: Positive;
            HarpoonIndex: Natural;
         when ANY =>
            Data: Data_Array;
      end case;
   end record;
   -- ****

   -- ****t* Ships/Modules_Container
   -- FUNCTION
   -- Used to store modules data in ships
   -- SOURCE
   package Modules_Container is new Vectors(Positive, ModuleData);
   -- ****

   -- ****t* Ships/Crew_Container
   -- FUNCTION
   -- Used to store crew data in ships
   -- SOURCE
   package Crew_Container is new Vectors(Positive, Member_Data);
   -- ****

   -- ****t* Ships/ShipRecord
   -- FUNCTION
   -- Data structure for ships
   -- PARAMETERS
   -- Name          - Ship name
   -- SkyX          - X coordinate on sky map
   -- SKyY          - Y coordinate on sky map
   -- Speed         - Speed of ship
   -- Modules       - List of ship modules
   -- Cargo         - List of ship cargo
   -- Crew          - List of ship crew
   -- UpgradeModule - Number of module to upgrade
   -- DestinationX  - Destination X coordinate
   -- DestinationY  - Destination Y coordinate
   -- RepairModule  - Number of module to repair as first
   -- Description   - Description of ship
   -- HomeBase      - Index of home base of ship
   -- SOURCE
   type ShipRecord is record
      Name: Unbounded_String;
      SkyX: Integer;
      SkyY: Integer;
      Speed: ShipSpeed;
      Modules: Modules_Container.Vector;
      Cargo: Inventory_Container.Vector;
      Crew: Crew_Container.Vector;
      UpgradeModule: Natural;
      DestinationX: Integer;
      DestinationY: Integer;
      RepairModule: Natural;
      Description: Unbounded_String;
      HomeBase: Natural;
   end record;
   -- ****

   -- ****t* Ships/ProtoMember_Data
   -- FUNCTION
   -- Data structure for proto crew info
   -- PARAMETERS
   -- ProtoIndex - Index of proto mob which will be used as crew member
   -- MinAmount  - Mininum amount of that mob in crew
   -- MaxAmount  - Maximum amount of that mob in crew. If 0 then MinAmount
   --              will be amount
   -- SOURCE
   type ProtoMember_Data is record
      ProtoIndex: Unbounded_String;
      MinAmount: Positive;
      MaxAmount: Natural;
   end record;
   -- ****

   -- ****t* Ships/ProtoCrew_Container
   -- FUNCTION
   -- Used to store crew info in ships prototypes
   -- SOURCE
   package ProtoCrew_Container is new Vectors(Positive, ProtoMember_Data);
   -- ****

   -- ****t* Ships/ProtoShipData
   -- FUNCTION
   -- Data structure for ship prototypes
   -- PARAMETERS
   -- Name         - Prototype name
   -- Modules      - List of ship modules
   -- Accuracy     - Bonus to hit for ship
   -- CombatAI     - Behaviour of ship in combat
   -- Evasion      - Bonus to evade attacks
   -- Loot         - Amount of loot(moneys) gained for destroying ship
   -- Perception   - Bonus to spot player ship first
   -- Cargo        - List of ship cargo
   -- CombatValue  - Combat value of ship (used to generate enemies)
   -- Crew         - List of mobs used as ship crew
   -- Description  - Description of ship
   -- Owner        - Index of faction to which ship belong
   -- KnownRecipes - List of known recipes
   -- SOURCE
   type ProtoShipData is record
      Name: Unbounded_String;
      Modules: UnboundedString_Container.Vector;
      Accuracy: Natural_Array(1 .. 2);
      CombatAI: ShipCombatAi;
      Evasion: Natural_Array(1 .. 2);
      Loot: Natural_Array(1 .. 2);
      Perception: Natural_Array(1 .. 2);
      Cargo: MobInventory_Container.Vector;
      CombatValue: Positive;
      Crew: ProtoCrew_Container.Vector;
      Description: Unbounded_String;
      Owner: Unbounded_String;
      KnownRecipes: UnboundedString_Container.Vector;
   end record;
   -- ****

   -- ****t* Ships/ProtoShips_Container
   -- FUNCTION
   -- Used to store prototype ships data
   -- SOURCE
   package ProtoShips_Container is new Hashed_Maps(Unbounded_String,
      ProtoShipData, Ada.Strings.Unbounded.Hash, "=");
   -- ****

   -- ****v* Ships/ProtoShips_List
   -- FUNCTION
   -- List of all prototypes of ships
   -- SOURCE
   ProtoShips_List: ProtoShips_Container.Map;
   -- ****

   -- ****v* Ships/PlayerShip
   -- FUNCTION
   -- The player ship
   -- SOURCE
   PlayerShip: ShipRecord;
   -- ****

   -- ****v* Ships/ShipSyllablesStart
   -- FUNCTION
   -- List of first syllables for generating ships names
   -- SOURCE
   ShipSyllablesStart: UnboundedString_Container.Vector;
   -- ****

   -- ****v* Ships/ShipSyllablesMiddle
   -- FUNCTION
   -- List of middle syllables for generating ships names
   -- SOURCE
   ShipSyllablesMiddle: UnboundedString_Container.Vector;
   -- ****

   -- ****v* Ships/ShipSyllablesEnd
   -- FUNCTION
   -- List of last syllables for generating ships names
   -- SOURCE
   ShipSyllablesEnd: UnboundedString_Container.Vector;
   -- ****

   -- ****e* Ships/Ships_Invalid_Data
   -- FUNCTION
   -- Raised when invalid data in ships file
   -- SOURCE
   Ships_Invalid_Data: exception;
   -- ****

   -- ****f* Ships/CreateShip
   -- FUNCTION
   -- Create new ship
   -- PARAMETERS
   -- ProtoIndex     - Index of prototype ship which will be used to create
   --                  the new ship
   -- Name           - Name of the new ship
   -- X              - X coordinate of newly created ship on map
   -- Y              - Y coordinate of newly created ship on map
   -- Speed          - Starting speed of newly created ship
   -- RandomUpgrades - If true, newly created ship will be have
   --                  random upgrades to own modules. Default is true.
   -- RESULT
   -- Newly created ship
   -- SOURCE
   function CreateShip
     (ProtoIndex, Name: Unbounded_String; X, Y: Integer; Speed: ShipSpeed;
      RandomUpgrades: Boolean := True) return ShipRecord with
      Pre => (ProtoShips_Container.Contains(ProtoShips_List, ProtoIndex)),
      Test_Case => ("Test_CreateShip", Nominal);
      -- ****

      -- ****f* Ships/LoadShips
      -- FUNCTION
      -- Load ships from files
      -- PARAMETERS
      -- Reader - XML Reader from which ships data will be read
      -- SOURCE
   procedure LoadShips(Reader: Tree_Reader);
   -- ****

   -- ****f* Ships/CountShipWeight
   -- FUNCTION
   -- Count weight of ship (with modules and cargo)
   -- PARAMETERS
   -- Ship - Ship which weight will be counted
   -- RESULT
   -- Ship weight in kilograms
   -- SOURCE
   function CountShipWeight(Ship: ShipRecord) return Positive with
      Test_Case => ("Test_CountShipWeight", Robustness);
      -- ****

      -- ****f* Ships/GenerateShipName
      -- FUNCTION
      -- Generate random name for ship
      -- PARAMETERS
      -- Owner - Index of faction to which ship belongs
      -- RESULT
      -- Random name for a ship
      -- SOURCE
   function GenerateShipName
     (Owner: Unbounded_String) return Unbounded_String with
      Pre => Owner /= Null_Unbounded_String,
      Test_Case => ("Test_GenerateShipName", Nominal);
      -- ****

      -- ****f* Ships/CountCombatValue
      -- FUNCTION
      -- Count combat value of player ship
      -- RESULT
      -- Numeric level of combat value of player ship
      -- SOURCE
   function CountCombatValue return Natural with
      Test_Case => ("Test_CountCombatValue", Robustness);
      -- ****

      -- ****f* Ships/GetCabinQuality
      -- FUNCTION
      -- Get description of quality of selected cabin in player ship
      -- PARAMETERS
      -- Quality - Numeric value of cabin quality
      -- RESULT
      -- Description of cabin quality
      -- SOURCE
   function GetCabinQuality(Quality: Natural) return String with
      Test_Case => ("Test_GetCabinQuality", Robustness);
      -- ****

end Ships;
