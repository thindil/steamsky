--    Copyright 2016-2021 Bartek thindil Jasicki
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors; use Ada.Containers;
with Ships; use Ships;
with Game; use Game;

-- ****h* Combat/Combat
-- FUNCTION
-- Provide code for ship to ship combat
-- SOURCE
package Combat is
-- ****

   -- ****v* Combat/Combat.EnemyName
   -- FUNCTION
   -- Name of enemy
   -- SOURCE
   EnemyName: Unbounded_String := Null_Unbounded_String;
   -- ****

   -- ****v* Combat/Combat.PilotOrder, Combat.EngineerOrder
   -- FUNCTION
   -- Orders for crew members
   -- SOURCE
   PilotOrder, EngineerOrder: Natural := 0;
   -- ****

   -- ****t* Combat/Combat.GunsInfoArray
   -- FUNCTION
   -- Data structure for gun information: 1 - Gun index in ship modules
   -- list, 2 - Gunner order, 3 - Amount of shoots from the gun, value below
   -- zero means that gun shoot once per that amount of rounds
   -- SOURCE
   type GunsInfoArray is array(1 .. 3) of Integer;
   -- ****

   -- ****t* Combat/Combat.Guns_Container
   -- FUNCTION
   -- Used to store data for player ship guns
   -- SOURCE
   package Guns_Container is new Vectors(Positive, GunsInfoArray);
   -- ****

   -- ****v* Combat/Combat.Guns
   -- FUNCTION
   -- List of guns installed on player ship
   -- SOURCE
   Guns: Guns_Container.Vector;
   -- ****

   -- ****v* Combat/Combat.BoardingOrders
   -- FUNCTION
   -- List of orders for boarding party
   -- SOURCE
   BoardingOrders: Integer_Container.Vector;
   -- ****

   -- ****s* Combat/Combat.Enemy_Record
   -- FUNCTION
   -- Data structure for enemies
   -- PARAMETERS
   -- Ship            - Ship data for enemy
   -- Accuracy        - Bonus to accuracy
   -- Distance        - Current distance to enemy
   -- CombatAI        - Enemy in combat AI type
   -- Evasion         - Bonus to evasion
   -- Loot            - Amount of loot(money) looted from ship
   -- Perception      - Bonus to perception
   -- HarpoonDuration - How long (amount of rounds) ship will be stopped by
   --                   player harpoon
   -- Guns            - List of guns installed on the enemy ship
   -- SOURCE
   type Enemy_Record is record
      Ship: ShipRecord;
      Accuracy: Natural;
      Distance: Integer;
      CombatAI: ShipCombatAi;
      Evasion: Natural;
      Loot: Natural;
      Perception: Natural;
      HarpoonDuration: Natural;
      Guns: Guns_Container.Vector;
   end record;
   -- ****

   -- ****v* Combat/Combat.Enemy
   -- FUNCTION
   -- Enemy information
   -- SOURCE
   Enemy: Enemy_Record;
   -- ****

   -- ****v* Combat/Combat.EndCombat
   -- FUNCTION
   -- True if combat ends
   -- SOURCE
   EndCombat: Boolean;
   -- ****

   -- ****v* Combat/Combat.MessagesStarts
   -- FUNCTION
   -- Start index for showing messages
   -- SOURCE
   MessagesStarts: Natural;
   -- ****

   -- ****v* Combat/Combat.OldSpeed
   -- FUNCTION
   -- Speed of player ship before combat
   -- SOURCE
   OldSpeed: ShipSpeed := FULL_SPEED;
   -- ****

   -- ****v* Combat/Combat.HarpoonDuration
   -- FUNCTION
   -- How long (amount of rounds) player ship will be stopped by enemy harpoon
   -- SOURCE
   HarpoonDuration: Natural;
   -- ****

   -- ****v* Combat/Combat.EnemyShipIndex
   -- FUNCTION
   -- Prototype index of enemy ship
   -- SOURCE
   EnemyShipIndex: Unbounded_String;
   -- ****

   -- ****f* Combat/Combat.StartCombat
   -- FUNCTION
   -- Generate enemy and start battle
   -- PARAMETERS
   -- EnemyIndex - Index of prototype ship of enemy which will be created
   -- NewCombat  - If true, it is a new combat. Default is true.
   -- RESULT
   -- True if combat starts, otherwise false
   -- SOURCE
   function StartCombat
     (EnemyIndex: Unbounded_String; NewCombat: Boolean := True)
      return Boolean with
      Pre => ProtoShips_Container.Contains(ProtoShips_List, EnemyIndex),
      Test_Case => (Name => "Test_StartCombat", Mode => Nominal);
      -- ****

      -- ****f* Combat/Combat.CombatTurn
      -- FUNCTION
      -- Count damage/ships actions, etc
      -- SOURCE
   procedure CombatTurn with
      Test_Case => (Name => "Test_CombatTurn", Mode => Robustness);
      -- ****

end Combat;
