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
with Items; use Items;
with Game; use Game;

-- ****h* Crew/Crew
-- FUNCTION
-- Provide code for manipulate player ship crew
-- SOURCE
package Crew is
-- ****

   -- ****t* Crew/Crew.Skill_Array
   -- FUNCTION
   -- Data structure for skills: 1 - Skill index, 2 - skill level, 3 - current experience in skill
   -- SOURCE
   type Skill_Array is array(1 .. 3) of Natural;
   -- ****

   -- ****t* Crew/Crew.Skills_Container
   -- FUNCTION
   -- Used to store skills data
   -- SOURCE
   package Skills_Container is new Vectors(Positive, Skill_Array);
   -- ****

   -- ****t* Crew/Crew.Crew_Orders
   -- FUNCTION
   -- Available orders for ships crews
   -- SOURCE
   type Crew_Orders is
     (Pilot, Engineer, Gunner, Repair, Craft, Upgrading, Talk, Heal, Clean,
      Rest, Defend, Boarding, Train);
   -- ****

   -- ****t* Crew/Crew.Equipment_Array
   -- FUNCTION
   -- Data structure for currently equipped items for crew members. 1 - weapon,
   -- 2 - shield, 3 - helmet, 4 - torso, 5 - arms, 6 - legs, 7 - tool
   -- SOURCE
   type Equipment_Array is array(1 .. 7) of Natural;
   -- ****

   -- ****t* Crew/Crew.Skill_Range
   -- FUNCTION
   -- Range used for skills but also for health, tiredness, hunger, thirst and
   -- loyalty
   -- SOURCE
   subtype Skill_Range is Natural range 0 .. 100;
   -- ****

   -- ****s* Crew/Crew.Member_Data
   -- FUNCTION
   -- Data structure for ship crew member
   -- PARAMETERS
   -- Name           - Name of member
   -- Gender         - Gender of member
   -- Health         - Level of health of member
   -- Tired          - Tiredness of member
   -- Skills         - Names indexes, levels and experience in skills of
   --                  member
   -- Hunger         - Hunger level of member
   -- Thirst         - Thirst level of member
   -- Order          - Current order for member
   -- PreviousOrder  - Previous order for member
   -- OrderTime      - Minutes to next check for order result
   -- Orders         - Priority of orders of member
   -- Attributes     - Levels and experience in attributes of member
   -- Inventory      - Owned items by member
   -- Equipment      - Items indexes from inventory used by character:
   --                  1 - weapon, 2 - shield, 3 - helmet, 4 - torso,
   --                  5 - arms, 6 - legs, 7 - tool
   -- Payment        - How much money member takes as payment.
   --                  1 - daily payment, 2 - percent from each trade
   -- ContractLength - How many days crew member will be in crew. -1 mean
   --                  pernament contract
   -- Morale         - Morale of crew member, between 0 and 100, 1 - level,
   --                  2 - points to next level
   -- Loyality       - Loyalty of crew member, between 0 and 100
   -- HomeBase       - Index of base from which crew member is
   -- Faction        - Index of faction to which crew member belongs
   -- SOURCE
   type Member_Data is record
      Name: Unbounded_String;
      Gender: Character;
      Health: Skill_Range;
      Tired: Natural range 0 .. 150;
      Skills: Skills_Container.Vector;
      Hunger: Skill_Range;
      Thirst: Skill_Range;
      Order: Crew_Orders;
      PreviousOrder: Crew_Orders;
      OrderTime: Integer;
      Orders: Natural_Array(1 .. 12);
      Attributes: Attributes_Container.Vector;
      Inventory: Inventory_Container.Vector;
      Equipment: Equipment_Array;
      Payment: Attributes_Array;
      ContractLength: Integer;
      Morale: Attributes_Array;
      Loyalty: Skill_Range;
      HomeBase: Bases_Range;
      Faction: Unbounded_String;
   end record;
   -- ****

   -- ****v* Crew/Crew.MaleSyllablesStart
   -- FUNCTION
   -- List of males first syllables for generating crew members names
   -- SOURCE
   MaleSyllablesStart: UnboundedString_Container.Vector;
   -- ****

   -- ****v* Crew/Crew.MaleSyllablesMiddle
   -- FUNCTION
   -- List of males middle syllables for generating crew members names
   -- SOURCE
   MaleSyllablesMiddle: UnboundedString_Container.Vector;
   -- ****

   -- ****v* Crew/Crew.MaleSyllablesEnd
   -- FUNCTION
   -- List of males last syllables for generating crew members names
   -- SOURCE
   MaleSyllablesEnd: UnboundedString_Container.Vector;
   -- ****

   -- ****v* Crew/Crew.MaleVocals
   -- FUNCTION
   -- List of males vocals for generating crew members names
   -- SOURCE
   MaleVocals: UnboundedString_Container.Vector;
   -- ****

   -- ****v* Crew/Crew.MaleConsonants
   -- FUNCTION
   -- List of males consonants for generating crew members names
   -- SOURCE
   MaleConsonants: UnboundedString_Container.Vector;
   -- ****

   -- ****v* Crew/Crew.FemaleSyllablesStart
   -- FUNCTION
   -- List of females first syllables for generating crew members names
   -- SOURCE
   FemaleSyllablesStart: UnboundedString_Container.Vector;
   -- ****

   -- ****v* Crew/Crew.FemaleSyllablesMiddle
   -- FUNCTION
   -- List of females middle syllables for generating crew members names
   -- SOURCE
   FemaleSyllablesMiddle: UnboundedString_Container.Vector;
   -- ****

   -- ****v* Crew/Crew.FemaleSyllablesEnd
   -- FUNCTION
   -- List of females last syllables for generating crew members names
   -- SOURCE
   FemaleSyllablesEnd: UnboundedString_Container.Vector;
   -- ****

   -- ****v* Crew/Crew.FemaleVocals
   -- FUNCTION
   -- List of females vocals for generating crew members names
   -- SOURCE
   FemaleVocals: UnboundedString_Container.Vector;
   -- ****

   -- ****e* Crew/Crew.Crew_Order_Error
   -- FUNCTION
   -- Raised when new order can't be set for selected crew member
   -- SOURCE
   Crew_Order_Error: exception;
   -- ****

   -- ****e* Crew/Crew.Crew_No_Space_Error
   -- FUNCTION
   -- Raised when no space for new item in crew member inventory
   -- SOURCE
   Crew_No_Space_Error: exception;
   -- ****

   -- ****f* Crew/Crew.GainExp
   -- FUNCTION
   -- Gain experience in selected skill.
   -- PARAMETERS
   -- Amount      - Amount of gained experience
   -- SkillNumber - Index of skill in skills list
   -- CrewIndex   - Crew index of member
   -- SOURCE
   procedure GainExp(Amount: Natural; SkillNumber, CrewIndex: Positive) with
      Pre => SkillNumber <= Skills_List.Last_Index,
      Test_Case => (Name => "Test_GainExp", Mode => Nominal);
      -- ****

      -- ****f* Crew/Crew.GenerateMemberName
      -- FUNCTION
      -- Generate random name for crew member
      -- PARAMETERS
      -- Gender       - Gender of crew member which name will be generated
      -- FactionIndex - Faction to which crew member belongs
      -- RESULT
      -- Random name for crew member
      -- SOURCE
   function GenerateMemberName
     (Gender: Character; FactionIndex: Unbounded_String)
      return Unbounded_String with
      Pre =>
      ((Gender = 'M' or Gender = 'F') and
       FactionIndex /= Null_Unbounded_String),
      Test_Case => (Name => "Test_GenerateMemberName", Mode => Nominal);
      -- ****

      -- ****f* Crew/Crew.FindCabin
      -- FUNCTION
      -- Find index of cabin which belongs to selected crew member
      -- PARAMETERS
      -- MemberIndex: Crew index of crew member which cabin is looking for
      -- RESULT
      -- Player ship module index of owned cabin or 0 if crew member don't
      -- have any cabin assigned
      -- SOURCE
   function FindCabin(MemberIndex: Positive) return Natural with
      Test_Case => (Name => "Test_FindCabin", Mode => Robustness);
      -- ****

      -- ****f* Crew/Crew.UpdateCrew
      -- FUNCTION
      -- Update player ship crew
      -- PARAMETERS
   -- Minutes     - Amount of in-game minutes which passed
   -- TiredPoints - Amount of Tired points which will be added to crew members
   -- InCombat    - If true, player is in combat. Default is false
   -- SOURCE
   procedure UpdateCrew
     (Minutes: Positive; TiredPoints: Natural; InCombat: Boolean := False) with
      Test_Case => (Name => "Test_UpdateCrew", Mode => Robustness);
      -- ****

      -- ****f* Crew/Crew.WaitForRest
      -- FUNCTION
      -- Wait until whole crew is rested
      -- SOURCE
   procedure WaitForRest with
      Test_Case => (Name => "Test_WaitForRest", Mode => Robustness);
      -- ****

      -- ****f* Crew/Crew.GetSkillLevelName
      -- FUNCTION
      -- Get member skill level name
      -- PARAMETERS
      -- SkillLevel - Numeric value of skill level
      -- RESULT
      -- Name (as words) of skill level
      -- SOURCE
   function GetSkillLevelName(SkillLevel: Skill_Range) return String with
      Test_Case => (Name => "Test_GetSkillLevelName", Mode => Nominal);
      -- ****

      -- ****f* Crew/Crew.GetAttributeLevelName
      -- FUNCTION
      -- Get member attribute level name
      -- PARAMETERS
      -- AttributeLevel - Numeric value of attribute level
      -- RESULT
      -- Name (as words) of attribute level
      -- SOURCE
   function GetAttributeLevelName(AttributeLevel: Positive) return String with
      Pre => (AttributeLevel <= 50),
      Test_Case => (Name => "Test_GetAttributeLevelName", Mode => Nominal);
      -- ****

      -- ****f* Crew/Crew.DailyPayment
      -- FUNCTION
   -- Daily payment and upgrade contracts length for player ship crew members
   -- SOURCE
   procedure DailyPayment with
      Test_Case => (Name => "Test_DailyPayment", Mode => Robustness);
      -- ****

      -- ****f* Crew/Crew.GetTrainingToolQuality
      -- FUNCTION
      -- Get minumum required quality for training tool for the selected skill
      -- for the selected crew member
      -- PARAMETERS
      -- MemberIndex - Index of crew member which skills will be queried
      -- SkillIndex  - Index of skill of which tool will be queried
      -- RESULT
      -- Minimum required quality of training tool or 100 if not set for this
      -- skill
      -- SOURCE
   function GetTrainingToolQuality
     (MemberIndex, SkillIndex: Positive) return Positive with
      Pre => SkillIndex <= Skills_List.Last_Index,
      Test_Case => (Name => "Test_GetTrainingToolQuality", Mode => Nominal);
      -- ****

end Crew;
