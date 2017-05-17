--    Copyright 2016-2017 Bartek thindil Jasicki
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

   EnemyName: Unbounded_String := Null_Unbounded_String; -- Name of enemy;
   PilotOrder, EngineerOrder: Positive; -- Orders for crew members
   type GunsInfoArray is
     array(1 .. 2) of Positive; -- Data structure for guns informations
   package Guns_Container is new Vectors(Positive, GunsInfoArray);
   Guns: Guns_Container.Vector; -- List of guns installed on player ship
   type Enemy_Record is -- Data structure for enemies
   record
      Ship: ShipRecord; -- Ship data for enemy
      Accuracy: Natural; -- Bonus to accuracy
      Distance: Integer; -- Current distance to enemy
      CombatAI: ShipCombatAi; -- Enemy in combat AI type
      Evasion: Natural; -- Bonus to evasion
      Loot: Natural; -- Amount of loot(money) looted from ship
      Perception: Natural; -- Bonus to perception
   end record;
   Enemy: Enemy_Record; -- Enemy informations
   EndCombat: Boolean; -- True if combat ends
   MessagesStarts: Natural; -- Start index for showing messages
   OldSpeed: ShipSpeed; -- Speed of player ship before combat

   function StartCombat
     (EnemyIndex: Positive;
      NewCombat: Boolean :=
        True)
     return GameStates; -- Generate enemy and start battle
   procedure CombatTurn; -- Count damage/ships actions, etc

end Combat;
