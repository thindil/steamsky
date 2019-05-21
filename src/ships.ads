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

package Ships is
   type ShipSpeed is
     (DOCKED, FULL_STOP, QUARTER_SPEED, HALF_SPEED, FULL_SPEED);
   type ShipCombatAi is (NONE, BERSERKER, ATTACKER, COWARD, DISARMER);
   type ShipUpgrade is (NONE, DURABILITY, MAX_VALUE, VALUE);
   type Data_Array is array(1 .. 3) of Integer;
   type ModuleType2 is
     (WORKSHOP, ANY, MEDICAL_ROOM, TRAINING_ROOM, ENGINE, CABIN, COCKPIT,
      TURRET, GUN, CARGO_ROOM, HULL, ARMOR, BATTERING_RAM, HARPOON_GUN);
   type Owners_Array is array(Positive range<>) of Integer;
   type ModuleData(MType: ModuleType2 := ANY; Owners: Positive := 1)
   is -- Data structure for ship modules
   record
      Name: Unbounded_String; -- Name of module
      ProtoIndex: Unbounded_String; -- Index of module prototype
      Weight: Natural; -- Weight of module
      Durability: Integer; -- 0 = destroyed
      MaxDurability: Integer; -- Base durability
      Owner: Owners_Array(1..Owners); -- Crew member indexes for owners of module
      UpgradeProgress: Integer; -- Progress of module upgrade
      UpgradeAction: ShipUpgrade; -- Type of module upgrade
      case MType is
         when ENGINE =>
            FuelUsage: Positive; -- Amount of fuel used for each move on map
            Power: Positive; -- Power of engine used for counting ship speed
            Disabled: Boolean; -- Did engine is disabled or not
         when CABIN =>
            Cleanliness: Natural; -- Cleanliness of selected cabin
            Quality: Natural; -- Quality of selected cabin
         when TURRET =>
            GunIndex: Natural; -- Index of installed gun
         when GUN =>
            Damage: Positive; -- Damage bonus for selected gun
            AmmoIndex: Natural; -- Cargo index of ammunition used by selected gun
         when HULL =>
            InstalledModules: Natural; -- Amount of installed modules on ship
            MaxModules: Positive; -- Amount of maximum installed modules for this hull
         when WORKSHOP =>
            CraftingIndex: Unbounded_String; -- Index of crafting recipe or item which is deconstructed
            CraftingTime: Natural; -- Time needed to finish crating order
            CraftingAmount: Natural; -- How many times repeat crafting order
         when MEDICAL_ROOM | COCKPIT | ARMOR | CARGO_ROOM =>
            null; -- Medical room, cockpit, armor and cargo bays don't have any special fields
         when TRAINING_ROOM =>
            TrainedSkill: Natural; -- Index of skill set to training
         when BATTERING_RAM =>
            Damage2: Positive; -- Damage done by battering ram
            CoolingDown: Boolean; -- If true, battering ram can't attack
         when HARPOON_GUN =>
            Duration: Positive; -- Duration bonus for selected harpoon gun
            HarpoonIndex: Natural; -- Cargo index of ammunition used by selected harpoon gun
         when ANY =>
            Data: Data_Array; -- Various data for module (depends on module)
      end case;
   end record;
   package Modules_Container is new Vectors(Positive, ModuleData);
   package Crew_Container is new Vectors(Positive, Member_Data);
   type ShipRecord is -- Data structure for ships
   record
      Name: Unbounded_String; -- Ship name
      SkyX: Integer; -- X coordinate on sky map
      SkyY: Integer; -- Y coordinate on sky map
      Speed: ShipSpeed; -- Speed of ship
      Modules: Modules_Container.Vector; -- List of ship modules
      Cargo: Inventory_Container.Vector; -- List of ship cargo
      Crew: Crew_Container.Vector; -- List of ship crew
      UpgradeModule: Natural; -- Number of module to upgrade
      DestinationX: Integer; -- Destination X coordinate
      DestinationY: Integer; -- Destination Y coordinate
      RepairModule: Natural; -- Number of module to repair as first
      Description: Unbounded_String; -- Description of ship
      HomeBase: Natural; -- Index of home base of ship
   end record;
   type ShipSkills_Array is array(1 .. 2) of Natural;
   type ProtoMember_Data is -- Data structure for proto crew info
   record
      ProtoIndex: Unbounded_String; -- Index of proto mob which will be used as crew member
      MinAmount: Positive; -- Mininum amount of that mob in crew
      MaxAmount: Natural; -- Maximum amount of that mob in crew. If 0 then MinAmount will be amount
   end record;
   package ProtoCrew_Container is new Vectors(Positive, ProtoMember_Data);
   type ProtoShipData is -- Data structure for ship prototypes
   record
      Name: Unbounded_String; -- Prototype name
      Modules: UnboundedString_Container.Vector; -- List of ship modules
      Accuracy: ShipSkills_Array; -- Bonus to hit for ship
      CombatAI: ShipCombatAi; -- Behaviour of ship in combat
      Evasion: ShipSkills_Array; -- Bonus to evade attacks
      Loot: ShipSkills_Array; -- Amount of loot(moneys) gained for destroying ship
      Perception: ShipSkills_Array; -- Bonus to spot player ship first
      Cargo: MobInventory_Container.Vector; -- List of ship cargo
      CombatValue: Positive; -- Combat value of ship (used to generate enemies)
      Crew: ProtoCrew_Container.Vector; -- List of mobs used as ship crew
      Description: Unbounded_String; -- Description of ship
      Owner: Unbounded_String; -- Index of faction to which ship belong
      KnownRecipes: UnboundedString_Container.Vector; -- List of known recipes
   end record;
   package ProtoShips_Container is new Hashed_Maps(Unbounded_String,
      ProtoShipData, Ada.Strings.Unbounded.Hash, "=");
   ProtoShips_List: ProtoShips_Container.Map;
   PlayerShip: ShipRecord;
   ShipSyllablesStart: UnboundedString_Container.Vector;
   ShipSyllablesMiddle: UnboundedString_Container.Vector;
   ShipSyllablesEnd: UnboundedString_Container.Vector;
   Ships_Invalid_Data: exception; -- Raised when invalid data in ships file

   function CreateShip(ProtoIndex, Name: Unbounded_String; X, Y: Integer;
      Speed: ShipSpeed; RandomUpgrades: Boolean := True) return ShipRecord with
      Pre =>
      (ProtoShips_Container.Contains
         (ProtoShips_List, ProtoIndex)); -- Create new ship
   procedure LoadShips(Reader: Tree_Reader); -- Load ships from files
   function CountShipWeight
     (Ship: ShipRecord)
     return Positive; -- Count weight of ship (with modules and cargo)
   function GenerateShipName
     (Owner: Unbounded_String) return Unbounded_String with
      Pre => Owner /= Null_Unbounded_String; -- Generate random name for ship
   function CountCombatValue
     return Natural; -- Count combat value of player ship
   function GetCabinQuality
     (Quality: Natural)
     return String; -- Get description of quality of selected cabin in player ship

end Ships;
