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
with Items; use Items;
with Game; use Game;

package Crew is

-- ****t* Crew/Skill_Array
-- SOURCE
   type Skill_Array is array(1 .. 3) of Natural;
-- ****

-- ****t* Crew/Skills_Container
-- SOURCE
   package Skills_Container is new Vectors(Positive, Skill_Array);
-- ****

-- ****t* Crew/Crew_Orders
-- SOURCE
   type Crew_Orders is
     (Pilot, Engineer, Gunner, Repair, Craft, Upgrading, Talk, Heal, Clean,
      Rest, Defend, Boarding, Train);
-- ****

-- ****t* Crew/Orders_Array
-- SOURCE
   type Orders_Array is array(1 .. 12) of Natural;
-- ****

-- ****t* Crew/Attributes_Array
-- SOURCE
   type Attributes_Array is array(1 .. 2) of Natural;
-- ****

-- ****t* Crew/Equipment_Array
-- SOURCE
   type Equipment_Array is array(1 .. 7) of Natural;
-- ****

-- ****t* Crew/Attributes_Container
-- SOURCE
   package Attributes_Container is new Vectors(Positive, Attributes_Array);
-- ****

-- ****t* Crew/Member_Data
-- FUNCTION
-- Data structure for ship crew member
-- SOURCE
   type Member_Data is record
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
      Equipment: Equipment_Array; -- Items indexes from inventory used by character: 1 - weapon, 2 - shield, 3 - helmet, 4 - torso, 5 - arms, 6 - legs, 7 - tool
      Payment: Attributes_Array; -- How much money member takes as payment. 1 - daily payment, 2 - percent from each trade
      ContractLength: Integer; -- How many days crew member will be in crew. -1 mean pernament contract
      Morale: Attributes_Array; -- Morale of crew member, between 0 and 100, 1 - level, 2 - points to next level
      Loyalty: Natural; -- Loyalty of crew member, between 0 and 100
      HomeBase: Positive; -- Index of base from which crew member is
      Faction: Unbounded_String; -- Index of faction to which crew member belongs
   end record;
-- ****

-- ****v* Crew/MaleSyllablesStart
-- SOURCE
   MaleSyllablesStart: UnboundedString_Container.Vector;
-- ****

-- ****v* Crew/MaleSyllablesMiddle
-- SOURCE
   MaleSyllablesMiddle: UnboundedString_Container.Vector;
-- ****

-- ****v* Crew/MaleSyllablesEnd
-- SOURCE
   MaleSyllablesEnd: UnboundedString_Container.Vector;
-- ****

-- ****v* Crew/MaleVocals
-- SOURCE
   MaleVocals: UnboundedString_Container.Vector;
-- ****

-- ****v* Crew/MaleConsonants
-- SOURCE
   MaleConsonants: UnboundedString_Container.Vector;
-- ****

-- ****v* Crew/FemaleSyllablesStart
-- SOURCE
   FemaleSyllablesStart: UnboundedString_Container.Vector;
-- ****

-- ****v* Crew/FemaleSyllablesMiddle
-- SOURCE
   FemaleSyllablesMiddle: UnboundedString_Container.Vector;
-- ****

-- ****v* Crew/FemaleSyllablesEnd
-- SOURCE
   FemaleSyllablesEnd: UnboundedString_Container.Vector;
-- ****

-- ****v* Crew/FemaleVocals
-- SOURCE
   FemaleVocals: UnboundedString_Container.Vector;
-- ****

-- ****v* Crew/Crew_Order_Error
-- FUNCTION
-- Raised when new order can't be set for selected crew member
-- SOURCE
   Crew_Order_Error: exception;
-- ****
-- ****v* Crew/Crew_No_Space_Error
-- FUNCTION
-- Raised when no space for new item in crew member inventory
-- SOURCE
   Crew_No_Space_Error: exception;
-- ****

-- ****f* Crew/GainExp
-- FUNCTION
-- Gain experience in selected skill.
-- SOURCE
   procedure GainExp(Amount: Natural; SkillNumber, CrewIndex: Positive) with
      Pre => SkillNumber <= Skills_List.Last_Index;
-- ****
-- ****f* Crew/GenerateMemberName
-- FUNCTION
-- Generate random name for crew member
-- SOURCE
   function GenerateMemberName
     (Gender: Character; FactionIndex: Unbounded_String)
      return Unbounded_String with
      Pre =>
      ((Gender = 'M' or Gender = 'F') and
       FactionIndex /= Null_Unbounded_String);
-- ****
-- ****f* Crew/FindCabin
-- FUNCTION
-- Find index of cabin for selected crew member
-- SOURCE
   function FindCabin(MemberIndex: Positive) return Natural;
-- ****
-- ****f* Crew/UpdateCrew
-- FUNCTION
-- Update player ship crew
-- SOURCE
   procedure UpdateCrew
     (Minutes: Positive; TiredPoints: Natural; InCombat: Boolean := False);
-- ****
-- ****f* Crew/WaitForRest;
-- FUNCTION
-- Wait until whole crew is rested
-- SOURCE
   procedure WaitForRest;
-- ****
-- ****f* Crew/GetSkillLevelName
-- FUNCTION
-- Get member skill level name
-- SOURCE
   function GetSkillLevelName(SkillLevel: Positive) return String;
-- ****
-- ****f* Crew/GetAttributeLevelName
-- FUNCTION
-- Get member attribute level name
-- SOURCE
   function GetAttributeLevelName(AttributeLevel: Positive) return String;
-- ****
-- ****f* Crew/DailyPayment;
-- FUNCTION
-- Daily payment and upgrade contracts length for player ship crew members
-- SOURCE
   procedure DailyPayment;
-- ****

end Crew;
