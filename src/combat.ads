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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors; use Ada.Containers;
with Ships; use Ships;
with Game; use Game;

-- ****h* Steamsky/Combat
-- FUNCTION
-- Provide code for ship to ship combat
-- SOURCE
package Combat is
-- ****

   -- ****v* Combat/EnemyName
   -- FUNCTION
   -- Name of enemy
   -- SOURCE
   EnemyName: Unbounded_String := Null_Unbounded_String;
   -- ****
   -- ****v* Combat/PilotOrder, EngineerOrder
   -- FUNCTION
   -- Orders for crew members
   -- SOURCE
   PilotOrder, EngineerOrder: Positive;
   -- ****
   -- ****t* Combat/GunsInfoArray
   -- FUNCTION
   -- Data structure for guns information
   -- SOURCE
   type GunsInfoArray is array(1 .. 2) of Positive;
   -- ****
   -- ****t* Combat/Guns_Container
   -- FUNCTION
   -- Used to store data for player ship guns
   -- SOURCE
   package Guns_Container is new Vectors(Positive, GunsInfoArray);
   -- ****
   -- ****v* Combat/Guns
   -- FUNCTION
   -- List of guns installed on player ship
   -- SOURCE
   Guns: Guns_Container.Vector;
   -- ****
   -- ****v* Combat/BoardingOrders
   -- FUNCTION
   -- List of orders for boarding party
   -- SOURCE
   BoardingOrders: Integer_Container.Vector;
   -- ****
   -- ****t* Combat/Enemy_Record
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
   end record;
   -- ****
   -- ****v* Combat/Enemy
   -- FUNCTION
   -- Enemy information
   -- SOURCE
   Enemy: Enemy_Record;
   -- ****
   -- ****v* Combat/EndCombat
   -- FUNCTION
   -- True if combat ends
   -- SOURCE
   EndCombat: Boolean;
   -- ****
   -- ****v* Combat/MessagesStarts
   -- FUNCTION
   -- Start index for showing messages
   -- SOURCE
   MessagesStarts: Natural;
   -- ****
   -- ****v* Combat/OldSpeed
   -- FUNCTION
   -- Speed of player ship before combat
   -- SOURCE
   OldSpeed: ShipSpeed;
   -- ****
   -- ****v* Combat/HarpoonDuration
   -- FUNCTION
   -- How long (amount of rounds) player ship will be stopped by enemy harpoon
   -- SOURCE
   HarpoonDuration: Natural;
   -- ****
   -- ****v* Combat/EnemyShipIndex
   -- FUNCTION
   -- Prototype index of enemy ship
   -- SOURCE
   EnemyShipIndex: Unbounded_String;
   -- ****

   -- ****f* Combat/StartCombat
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
      Pre => ProtoShips_Container.Contains(ProtoShips_List, EnemyIndex);
      -- ****
      -- ****f* Combat/CombatTurn
      -- FUNCTION
      -- Count damage/ships actions, etc
      -- SOURCE
   procedure CombatTurn;
   -- ****

end Combat;
