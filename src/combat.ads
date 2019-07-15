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

package Combat is

   -- Name of enemy
   EnemyName: Unbounded_String := Null_Unbounded_String;
   -- Orders for crew members
   PilotOrder, EngineerOrder: Positive;
   -- Data structure for guns informations
   type GunsInfoArray is array(1 .. 2) of Positive;
   package Guns_Container is new Vectors(Positive, GunsInfoArray);
   -- List of guns installed on player ship
   Guns: Guns_Container.Vector;
   -- List of orders for boarding party
   BoardingOrders: Integer_Container.Vector;
   -- Data structure for enemies
   type Enemy_Record is record
      Ship: ShipRecord; -- Ship data for enemy
      Accuracy: Natural; -- Bonus to accuracy
      Distance: Integer; -- Current distance to enemy
      CombatAI: ShipCombatAi; -- Enemy in combat AI type
      Evasion: Natural; -- Bonus to evasion
      Loot: Natural; -- Amount of loot(money) looted from ship
      Perception: Natural; -- Bonus to perception
      HarpoonDuration: Natural; -- How long (amount of rounds) ship will be stopped by player harpoon
   end record;
   -- Enemy informations
   Enemy: Enemy_Record;
   -- True if combat ends
   EndCombat: Boolean;
   -- Start index for showing messages
   MessagesStarts: Natural;
   -- Speed of player ship before combat
   OldSpeed: ShipSpeed;
   -- How long (amount of rounds) player ship will be stopped by enemy harpoon
   HarpoonDuration: Natural;
   -- Prototype index of enemy ship
   EnemyShipIndex: Unbounded_String;

   -- Generate enemy and start battle, return True if combat starts
   function StartCombat
     (EnemyIndex: Unbounded_String; NewCombat: Boolean := True)
      return Boolean with
      Pre => ProtoShips_Container.Contains(ProtoShips_List, EnemyIndex);
      -- Count damage/ships actions, etc
   procedure CombatTurn;

end Combat;
