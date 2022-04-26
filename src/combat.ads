--    Copyright 2016-2022 Bartek thindil Jasicki
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

   -- ****v* Combat/Combat.Enemy_Name
   -- FUNCTION
   -- Name of enemy
   -- SOURCE
   Enemy_Name: Tiny_String.Bounded_String := Tiny_String.Null_Bounded_String;
   -- ****

   -- ****v* Combat/Combat.Pilot_Order, Combat.Engineer_Order
   -- FUNCTION
   -- Orders for crew members
   -- SOURCE
   Pilot_Order, Engineer_Order: Natural := 0;
   -- ****

   -- ****t* Combat/Combat.Guns_Info_Array
   -- FUNCTION
   -- Data structure for gun information: 1 - Gun index in ship modules
   -- list, 2 - Gunner order, 3 - Amount of shoots from the gun, value below
   -- zero means that gun shoot once per that amount of rounds
   -- SOURCE
   type Guns_Info_Array is array(1 .. 3) of Integer with
      Default_Component_Value => 0;
   -- ****

   -- ****t* Combat/Combat.Guns_Container
   -- FUNCTION
   -- Used to store data for player ship guns
   -- SOURCE
   package Guns_Container is new Vectors
     (Index_Type => Positive, Element_Type => Guns_Info_Array);
   -- ****

   -- ****v* Combat/Combat.Guns
   -- FUNCTION
   -- List of guns installed on player ship
   -- SOURCE
   Guns: Guns_Container.Vector;
   -- ****

   -- ****v* Combat/Combat.Boarding_Orders
   -- FUNCTION
   -- List of orders for boarding party
   -- SOURCE
   Boarding_Orders: Integer_Container.Vector;
   -- ****

   -- ****s* Combat/Combat.Enemy_Record
   -- FUNCTION
   -- Data structure for enemies
   -- PARAMETERS
   -- Ship             - Ship data for enemy
   -- Accuracy         - Bonus to accuracy
   -- Distance         - Current distance to enemy
   -- Combat_Ai        - Enemy in combat AI type
   -- Evasion          - Bonus to evasion
   -- Loot             - Amount of loot(money) looted from ship
   -- Perception       - Bonus to perception
   -- Harpoon_Duration - How long (amount of rounds) ship will be stopped by
   --                    player harpoon
   -- Guns             - List of guns installed on the enemy ship
   -- SOURCE
   type Enemy_Record is record
      Ship: Ship_Record;
      Accuracy: Natural := 0;
      Distance: Integer := 0;
      Combat_Ai: Ship_Combat_Ai;
      Evasion: Natural := 0;
      Loot: Natural := 0;
      Perception: Natural := 0;
      Harpoon_Duration: Natural := 0;
      Guns: Guns_Container.Vector;
   end record;
   -- ****

   -- ****v* Combat/Combat.Enemy
   -- FUNCTION
   -- Enemy information
   -- SOURCE
   Enemy: Enemy_Record;
   -- ****

   -- ****v* Combat/Combat.End_Combat
   -- FUNCTION
   -- True if combat ends
   -- SOURCE
   End_Combat: Boolean;
   -- ****

   -- ****v* Combat/Combat.Messages_Starts
   -- FUNCTION
   -- Start index for showing messages
   -- SOURCE
   Messages_Starts: Natural;
   -- ****

   -- ****v* Combat/Combat.Old_Speed
   -- FUNCTION
   -- Speed of player ship before combat
   -- SOURCE
   Old_Speed: Ship_Speed := FULL_SPEED;
   -- ****

   -- ****v* Combat/Combat.Harpoon_Duration
   -- FUNCTION
   -- How long (amount of rounds) player ship will be stopped by enemy harpoon
   -- SOURCE
   Harpoon_Duration: Natural;
   -- ****

   -- ****v* Combat/Combat.Enemy_Ship_Index
   -- FUNCTION
   -- Prototype index of enemy ship
   -- SOURCE
   Enemy_Ship_Index: Proto_Ships_Container.Extended_Index;
   -- ****

   -- ****f* Combat/Combat.Start_Combat
   -- FUNCTION
   -- Generate enemy and start battle
   -- PARAMETERS
   -- Enemy_Index - Index of prototype ship of enemy which will be created
   -- New_Combat  - If true, it is a new combat. Default is true.
   -- RESULT
   -- True if combat starts, otherwise false
   -- SOURCE
   function Start_Combat
     (Enemy_Index: Proto_Ships_Container.Extended_Index;
      New_Combat: Boolean := True) return Boolean with
      Pre => Enemy_Index <= Proto_Ships_List.Last_Index,
      Test_Case => (Name => "Test_StartCombat", Mode => Nominal);
      -- ****

      -- ****f* Combat/Combat.Combat_Turn
      -- FUNCTION
      -- Count damage/ships actions, etc
      -- SOURCE
   procedure Combat_Turn with
      Test_Case => (Name => "Test_CombatTurn", Mode => Robustness);
      -- ****

end Combat;
