--    Copyright 2017-2022 Bartek thindil Jasicki
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

-- ****h* Ships/SCrew
-- FUNCTION
-- Provides code for manipulate ships crews
-- SOURCE
package Ships.Crew is
-- ****

   -- ****f* SCrew/SCrew.Get_Skill_Level
   -- FUNCTION
   -- Get level of skill of selected crew member
   -- PARAMETERS
   -- Member      - Crew member which skill will be looking for
   -- Skill_Index - Index of skill in skills list
   -- RESULT
   -- Real level of selected skill of selected crew member
   -- SOURCE
   function Get_Skill_Level
     (Member: Member_Data; Skill_Index: Skills_Amount_Range)
      return Skill_Range with
      Pre => Skill_Index in 1 .. Skills_Amount,
      Test_Case => (Name => "Test_GetSkillLevel", Mode => Nominal);
      -- ****

      -- ****f* SCrew/SCrew.Death
      -- FUNCTION
      -- Handle crew member death
      -- PARAMETERS
      -- Member_Index - Crew index of the member which died
      -- Reason       - Reason of the death
      -- Ship         - Ship in which crew member died
      -- Create_Body  - If true, create body for dead crew member. Default is
      --                true
      -- RESULT
      -- Parameter Ship with updated data (crew, cargo, modules)
      -- SOURCE
   procedure Death
     (Member_Index: Crew_Container.Extended_Index; Reason: Unbounded_String;
      Ship: in out Ship_Record; Create_Body: Boolean := True) with
      Pre =>
      (Member_Index in Ship.Crew.First_Index .. Ship.Crew.Last_Index and
       Reason /= Null_Unbounded_String),
      Test_Case => (Name => "Test_Death", Mode => Nominal);
      -- ****

      -- ****f* SCrew/SCrew.Delete_Member
      -- FUNCTION
      -- Delete selected member from crew list
      -- PARAMETERS
      -- Member_Index - Crew index of the member which will be deleted
      -- Ship         - Ship which crew will be modified
      -- RESULT
      -- Parameter Ship with modified data (crew and modules)
      -- SOURCE
   procedure Delete_Member
     (Member_Index: Crew_Container.Extended_Index;
      Ship: in out Ship_Record) with
      Pre => Member_Index in Ship.Crew.First_Index .. Ship.Crew.Last_Index,
      Test_Case => (Name => "Test_DeleteMember", Mode => Nominal);
      -- ****

   -- ****f* SCrew/SCrew.Find_Member
   -- FUNCTION
   -- Find index of first crew member with selected order
   -- PARAMETERS
   -- Order - Current crew order which will be looking for
   -- Crew  - Crew of ship which will be searched
   -- RESULT
   -- Crew index of crew member with selected order or 0 if nothing was found
   -- SOURCE
   function Find_Member
     (Order: Crew_Orders; Crew: Crew_Container.Vector := Player_Ship.Crew)
      return Crew_Container.Extended_Index with
      Post => Find_Member'Result <= Crew.Last_Index,
      Test_Case => (Name => "Test_FindMember", Mode => Nominal);
      -- ****

      -- ****f* SCrew/SCrew.Give_Orders
      -- FUNCTION
      -- Change order for selected crew member
      -- PARAMETERS
      -- Ship             - Ship in which crew member will be have changed order
      -- Member_Index     - Crew index of member to change order
      -- Given_Order      - New order for selected crew member
      -- Module_Index     - Index of module to assign to crew member with new
      --                    order. Default is 0 - no module assigned
      -- Check_Priorities - If true, check orders priorities of whole crew.
      --                    Default is true
      -- Result
      -- Parameter Ship with modified data (crew, modules, cargo)
      -- SOURCE
   procedure Give_Orders
     (Ship: in out Ship_Record; Member_Index: Crew_Container.Extended_Index;
      Given_Order: Crew_Orders;
      Module_Index: Modules_Container.Extended_Index := 0;
      Check_Priorities: Boolean := True) with
      Pre =>
      (Member_Index in Ship.Crew.First_Index .. Ship.Crew.Last_Index and
       Module_Index <= Ship.Modules.Last_Index),
      Test_Case => (Name => "Test_GiveOrders", Mode => Nominal);
      -- ****

      -- ****f* SCrew/SCrew.Update_Orders
      -- FUNCTION
      -- Update crew orders based on their orders priorities
      -- PARAMETERS
      -- Ship   - Ship in which crew will be check
      -- Combat - If true, ship is in combat. Default is false
      -- RESULT
      -- Parameter Ship with modified data (crew, modules, cargo)
      -- SOURCE
   procedure Update_Orders
     (Ship: in out Ship_Record; Combat: Boolean := False) with
      Test_Case => (Name => "Test_UpdateOrders", Mode => Robustness);
      -- ****

      -- ****f* SCrew/SCrew.Update_Morale
      -- FUNCTION
      -- Update morale of selected crew member by value
      -- PARAMETERS
      -- Ship         - Ship on which crew member will be have updated morale
      -- Member_Index - Crew index of member to update morale
      -- Value        - Amount of morale to add or substract
      -- RESULT
      -- Parameter Ship with modified crew info
      -- SOURCE
   procedure Update_Morale
     (Ship: in out Ship_Record; Member_Index: Crew_Container.Extended_Index;
      Value: Integer) with
      Pre => Member_Index in Ship.Crew.First_Index .. Ship.Crew.Last_Index,
      Test_Case => (Name => "Test_UpdateMorale", Mode => Nominal);
      -- ****

   function Get_Current_Order
     (Member_Index: Positive) return Unbounded_String with
      Pre => Member_Index in
        Player_Ship.Crew.First_Index .. Player_Ship.Crew.Last_Index;

end Ships.Crew;
