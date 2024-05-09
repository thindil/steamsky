--    Copyright 2016-2024 Bartek thindil Jasicki
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

with Ada.Containers.Formal_Vectors; use Ada.Containers;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Items; use Items;
with Game; use Game;

-- ****h* Crew/Crew
-- FUNCTION
-- Provide code for manipulate player ship crew
-- SOURCE
package Crew is
-- ****

   -- ****s* Crew/Crew.Skill_Info
   -- FUNCTION
   -- Data structure for skills
   -- PARAMETERS
   -- Index      - The index of the skill in the skills list
   -- Level      - The current level of the skill
   -- Experience - The amount of experience in the skill
   -- HISTORY
   -- 6.6 - Added
   -- SOURCE
   type Skill_Info is record
      Index: Skills_Amount_Range := 1;
      Level: Skill_Range := 0;
      Experience: Natural := 0;
   end record;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Crew/Crew.Empty_Skill_Info
   -- FUNCTION
   -- Default empty skill
   -- HISTORY
   -- 6.6 - Added
   -- SOURCE
   Empty_Skill_Info: constant Skill_Info := Skill_Info'(others => <>);
   -- ****

   -- ****t* Crew/Crew.Skills_Container
   -- FUNCTION
   -- Used to store skills data
   -- SOURCE
   package Skills_Container is new Formal_Vectors
     (Index_Type => Skills_Amount_Range, Element_Type => Skill_Info);
   -- ****

   -- ****t* Crew/Crew.Crew_Orders
   -- FUNCTION
   -- Available orders for ships crews
   -- SOURCE
   type Crew_Orders is
     (PILOT, ENGINEER, GUNNER, REPAIR, CRAFT, UPGRADING, TALK, HEAL, CLEAN,
      REST, DEFEND, BOARDING, TRAIN) with
      Default_Value => REST;
      -- ****

      -- ****d* Crew/Crew.Default_Crew_Order
      -- FUNCTION
      -- The default order for the crew members
      -- SOURCE
   Default_Crew_Order: constant Crew_Orders := REST;
   -- ****

   -- ****t* Crew/Crew.Equipment_Locations
   -- FUNCTION
   -- The list of equipment locations
   -- SOURCE
   type Equipment_Locations is
     (WEAPON, SHIELD, HELMET, TORSO, ARMS, LEGS, TOOL) with
      Default_Value => WEAPON;
      -- ****

      -- ****d* Crew/Crew.Default_Equipment_Location
      -- FUNCTION
      -- The default location for an equipment
      -- SOURCE
   Default_Equipment_Location: constant Equipment_Locations := WEAPON;
   -- ****

   -- ****t* Crew/Crew.Equipment_Array
   -- FUNCTION
   -- Data structure for currently equipped items for crew members.
   -- SOURCE
   type Equipment_Array is array(Equipment_Locations) of Natural with
      Default_Component_Value => 0;
      -- ****

      -- ****d* Crew/Crew.Empty_Equipment_Array
      -- FUNCTION
      -- The default empty equipment array
      -- SOURCE
   Empty_Equipment_Array: constant Equipment_Array :=
     Equipment_Array'(others => <>);
   -- ****

   -- ****s* Crew/Crew.Mob_Attribute_Record
   -- FUNCTION
   -- Used to store the attributes of mobs
   -- PARAMETERS
   -- Level      - The level of the attribute
   -- Experience - The experience amount in the attribute
   -- HISTORY
   -- 6.6 - Added
   -- SOURCE
   type Mob_Attribute_Record is record
      Level: Positive range 1 .. 50 := 1;
      Experience: Natural := 0;
   end record;
   -- ****

   -- ****d* Crew/Crew.Empty_Attributes
   -- FUNCTION
   -- Empty values for mob attributes
   -- HISTORY
   -- 6.6 - Added
   -- SOURCE
   Empty_Attributes: constant Mob_Attribute_Record :=
     Mob_Attribute_Record'(others => <>);
     -- ****
     --## rule on REDUCEABLE_SCOPE

     --## rule off TYPE_INITIAL_VALUES
     -- ****t* Crew/Crew.Mob_Attributes
     -- FUNCTION
     -- Array used to store attributes of the mobs (crew, other mobs, recruits,
     -- etc).
     -- HISTORY
     -- 6.5 - Added
     -- 6.6 - Changed from array of array to array of record
     -- SOURCE
   type Mob_Attributes is
     array(Attributes_Amount_Range range <>) of Mob_Attribute_Record;
     -- ****

     -- ****s* Crew/Crew.Mob_Record
     -- FUNCTION
     -- Abstract record to store all common settings for mobs (crew, other mobs,
     -- recruits, etc)
     -- PARAMETERS
     -- Attributes           - Levels and experience in attributes of the mob
     -- Amount_Of_Attributes - The amount of attributes declared in the game
     -- Amount_Of_Skills     - The amount of skills declared in the game
     -- HISTORY
     -- 6.5 - Added
     -- SOURCE
   type Mob_Record
     (Amount_Of_Attributes: Attributes_Amount_Range;
      Amount_Of_Skills: Skills_Amount_Range)
   is abstract tagged record
      Attributes: Mob_Attributes(1 .. Amount_Of_Attributes);
      Skills: Skills_Container.Vector (Capacity => Amount_Of_Skills);
   end record;
   -- ****

   -- ****s* Crew/Crew.Member_Data
   -- FUNCTION
   -- Data structure for ship crew member
   -- PARAMETERS
   -- Name            - Name of member
   -- Gender          - Gender of member
   -- Health          - Level of health of member
   -- Tired           - Tiredness of member
   -- Skills          - Names indexes, levels and experience in skills of
   --                   member
   -- Hunger          - Hunger level of member
   -- Thirst          - Thirst level of member
   -- Order           - Current order for member
   -- Previous_Order  - Previous order for member
   -- Order_Time      - Minutes to next check for order result
   -- Orders          - Priority of orders of member
   -- Attributes      - Levels and experience in attributes of member
   -- Inventory       - Owned items by member
   -- Equipment       - Items indexes from inventory used by character:
   --                   1 - weapon, 2 - shield, 3 - helmet, 4 - torso,
   --                   5 - arms, 6 - legs, 7 - tool
   -- Payment         - How much money member takes as payment.
   --                   1 - daily payment, 2 - percent from each trade
   -- Contract_Length - How many days crew member will be in crew. -1 mean
   --                   pernament contract
   -- Morale          - Morale of crew member, between 0 and 100, 1 - level,
   --                   2 - points to next level
   -- Loyality        - Loyalty of crew member, between 0 and 100
   -- Home_Base       - Index of base from which crew member is
   -- Faction         - Index of faction to which crew member belongs
   -- SOURCE
   type Member_Data is new Mob_Record with record
      Name: Tiny_String.Bounded_String;
      Gender: Character;
      Health: Skill_Range;
      Tired: Natural range 0 .. 150 := 0;
      Hunger: Skill_Range;
      Thirst: Skill_Range;
      Order: Crew_Orders;
      Previous_Order: Crew_Orders;
      Order_Time: Integer := 15;
      Orders: Natural_Array(1 .. 12);
      Inventory: Inventory_Container.Vector (Capacity => 32);
      Equipment: Equipment_Array;
      Payment: Attributes_Array;
      Contract_Length: Integer := 0;
      Morale: Attributes_Array;
      Loyalty: Skill_Range;
      Home_Base: Bases_Range;
      Faction: Tiny_String.Bounded_String;
   end record;
   -- ****
   --## rule on TYPE_INITIAL_VALUES

   -- ****e* Crew/Crew.Crew_Order_Error
   -- FUNCTION
   -- Raised when new order can't be set for selected crew member
   -- SOURCE
   Crew_Order_Error: exception;
   -- ****

   -- ****f* Crew/Crew.Gain_Exp
   -- FUNCTION
   -- Gain experience in selected skill.
   -- PARAMETERS
   -- Amoun t      - Amount of gained experience
   -- Skill_Number - Index of skill in skills list
   -- Crew_Index   - Crew index of member
   -- SOURCE
   procedure Gain_Exp
     (Amount: Natural; Skill_Number: Skills_Amount_Range;
      Crew_Index: Positive) with
      Pre => Skill_Number <= Skills_Amount;
      -- ****

      -- ****f* Crew/Crew.Generate_Member_Name
      -- FUNCTION
      -- Generate random name for crew member
      -- PARAMETERS
      -- Gender        - Gender of crew member which name will be generated
      -- Faction_Index - Faction to which crew member belongs
      -- RESULT
      -- Random name for crew member
      -- SOURCE
   function Generate_Member_Name
     (Gender: Character; Faction_Index: Tiny_String.Bounded_String)
      return Tiny_String.Bounded_String with
      Pre => Gender in 'M' | 'F' and
      Tiny_String.Length(Source => Faction_Index) > 0;
      -- ****

      -- ****f* Crew/Crew.Wait_For_Rest
      -- FUNCTION
      -- Wait until whole crew is rested
      -- SOURCE
   procedure Wait_For_Rest;
   -- ****

   -- ****f* Crew/Crew.Get_Skill_Level_Name
   -- FUNCTION
   -- Get member skill level name
   -- PARAMETERS
   -- Skill_Level - Numeric value of skill level
   -- RESULT
   -- Name (as words) of skill level
   -- SOURCE
   function Get_Skill_Level_Name(Skill_Level: Skill_Range) return String;
   -- ****

   -- ****f* Crew/Crew.Get_Attribute_Level_Name
   -- FUNCTION
   -- Get member attribute level name
   -- PARAMETERS
   -- Attribute_Level - Numeric value of attribute level
   -- RESULT
   -- Name (as words) of attribute level
   -- SOURCE
   function Get_Attribute_Level_Name
     (Attribute_Level: Positive) return String with
      Pre => Attribute_Level <= 50;
      -- ****

-- Temporary code to interact with Nim

   --## rule off TYPE_INITIAL_VALUES
   type Nim_Attributes_Array is array(1 .. 16, 1 .. 2) of Integer;
   type Nim_Skills_Array is array(1 .. 64, 1 .. 3) of Integer;
   type Nim_Equipment_Array is array(0 .. 6) of Integer;

   type Nim_Member_Data is record
      Attributes: Nim_Attributes_Array;
      Skills: Nim_Skills_Array;
      Name: chars_ptr;
      Gender: Character;
      Health: Integer;
      Tired: Integer;
      Hunger: Integer;
      Thirst: Integer;
      Order: Integer;
      Previous_Order: Integer;
      Order_Time: Integer;
      Orders: Natural_Array(1 .. 12);
      Equipment: Nim_Equipment_Array;
      Payment: Attributes_Array;
      Contract_Length: Integer;
      Morale: Attributes_Array;
      Loyalty: Integer;
      Home_Base: Integer;
      Faction: chars_ptr;
   end record;
   --## rule on TYPE_INITIAL_VALUES

   function Member_To_Nim(Member: Member_Data) return Nim_Member_Data;
   procedure Member_From_Nim
     (Member: Nim_Member_Data; Ada_Member: in out Member_Data);

end Crew;
