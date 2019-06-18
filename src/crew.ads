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

   type Skill_Array is array(1 .. 3) of Natural;
   package Skills_Container is new Vectors(Positive, Skill_Array);
   type Crew_Orders is
     (Pilot, Engineer, Gunner, Repair, Craft, Upgrading, Talk, Heal, Clean,
      Rest, Defend, Boarding, Train);
   type Orders_Array is array(1 .. 12) of Natural;
   type Attributes_Array is array(1 .. 2) of Natural;
   type Equipment_Array is array(1 .. 7) of Natural;
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
      Equipment: Equipment_Array; -- Items indexes from inventory used by character: 1 - weapon, 2 - shield, 3 - helmet, 4 - torso, 5 - arms, 6 - legs, 7 - tool
      Payment: Attributes_Array; -- How much money member takes as payment. 1 - daily payment, 2 - percent from each trade
      ContractLength: Integer; -- How many days crew member will be in crew. -1 mean pernament contract
      Morale: Attributes_Array; -- Morale of crew member, between 0 and 100, 1 - level, 2 - points to next level
      Loyalty: Natural; -- Loyalty of crew member, between 0 and 100
      HomeBase: Positive; -- Index of base from which crew member is
      Faction: Unbounded_String; -- Index of faction to which crew member belongs
   end record;
   MaleSyllablesStart: UnboundedString_Container.Vector;
   MaleSyllablesMiddle: UnboundedString_Container.Vector;
   MaleSyllablesEnd: UnboundedString_Container.Vector;
   MaleVocals: UnboundedString_Container.Vector;
   MaleConsonants: UnboundedString_Container.Vector;
   FemaleSyllablesStart: UnboundedString_Container.Vector;
   FemaleSyllablesMiddle: UnboundedString_Container.Vector;
   FemaleSyllablesEnd: UnboundedString_Container.Vector;
   FemaleVocals: UnboundedString_Container.Vector;
   Crew_Order_Error: exception; -- Raised when new order can't be set for selected crew member
   Crew_No_Space_Error: exception; -- Raised when no space for new item in crew member inventory

   procedure GainExp(Amount: Natural; SkillNumber, CrewIndex: Positive) with
      Pre => SkillNumber <=
      Skills_List.Last_Index; -- Gain experience in selected skill.
   function GenerateMemberName
     (Gender: Character; FactionIndex: Unbounded_String)
      return Unbounded_String with
      Pre =>
      ((Gender = 'M' or Gender = 'F') and
       FactionIndex /=
         Null_Unbounded_String); -- Generate random name for crew member
   function FindCabin
     (MemberIndex: Positive)
      return Natural; -- Find index of cabin for selected crew member
   -- Update player ship crew
   procedure UpdateCrew
     (Minutes: Positive; TiredPoints: Natural; InCombat: Boolean := False);
   procedure WaitForRest; -- Wait until whole crew is rested
   function GetSkillLevelName
     (SkillLevel: Positive) return String; -- Get member skill level name
   function GetAttributeLevelName
     (AttributeLevel: Positive)
      return String; -- Get member attribute level name
   procedure DailyPayment; -- Daily payment and upgrade contracts length for player ship crew members

end Crew;
