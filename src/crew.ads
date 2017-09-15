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
with Items; use Items;

package Crew is

   type Skill_Array is array(1 .. 3) of Natural;
   package Skills_Container is new Vectors(Positive, Skill_Array);
   type Crew_Orders is
     (Pilot,
      Engineer,
      Gunner,
      Repair,
      Craft,
      Upgrading,
      Talk,
      Heal,
      Clean,
      Rest);
   type Orders_Array is array(1 .. 9) of Natural;
   type Attributes_Array is array(1 .. 2) of Natural;
   package Attributes_Container is new Vectors(Positive, Attributes_Array);
   type Member_Data is -- Data structure for ship crew member
   record
      Name: Unbounded_String; -- Name of member
      Gender: Character; -- Gender of member
      Health: Natural; -- Level of health of member
      Tired: Natural; -- Tiredness of member
      Skills: Skills_Container
        .Vector; -- Names indexes, levels and experience in skills of member
      Hunger: Natural; -- Hunger level of member
      Thirst: Natural; -- Thirst level of member
      Order: Crew_Orders; -- Current order for member
      PreviousOrder: Crew_Orders; -- Previous order for member
      OrderTime: Integer; -- Minutes to next check for order result
      Orders: Orders_Array; -- Priority of orders of member
      Attributes: Attributes_Container
        .Vector; -- Levels and experience in attributes of member
      Inventory: Inventory_Container.Vector; -- Owned items by member
   end record;
   Crew_Order_Error: exception; -- Raised when new order can't be set for selected crew member
   Crew_No_Space_Error: exception; -- Raised when no space for new item in crew member inventory

   procedure GiveOrders
     (MemberIndex: Positive;
      GivenOrder: Crew_Orders;
      ModuleIndex: Natural := 0;
      CheckPriorities: Boolean :=
        True); -- Change order for selected crew member
   procedure GainExp
     (Amount: Natural;
      SkillNumber, CrewIndex: Positive); -- Gain experience in selected skill.
   function GenerateMemberName
     (Gender: Character)
     return Unbounded_String; -- Generate random name for crew member
   procedure UpdateCrew
     (Minutes: Positive;
      TiredPoints: Natural); -- Update ship crew
   procedure UpdateOrders; -- Update crew orders based on their orders priorities
   procedure WaitForRest; -- Wait until whole crew is rested
   function GetSkillLevelName
     (SkillLevel: Positive) return String; -- Get member skill level name
   function GetAttributeLevelName
     (AttributeLevel: Positive)
     return String; -- Get member attribute level name
   procedure UpdateInventory
     (MemberIndex: Positive;
      Amount: Integer;
      ProtoIndex,
      Durability,
      InventoryIndex: Natural :=
        0); -- Update member inventory
   function FreeInventory
     (MemberIndex: Positive;
      Amount: Integer)
     return Integer; -- Return available space in crew member inventory after adding/extracting Amount

end Crew;
