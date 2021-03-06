--    Copyright 2017-2021 Bartek thindil Jasicki
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

-- ****h* Ships/SCrew
-- FUNCTION
-- Provides code for manipulate ships crews
-- SOURCE
package Ships.Crew is
-- ****

   -- ****f* SCrew/SCrew.GetSkillLevel
   -- FUNCTION
   -- Get level of skill of selected crew member
   -- PARAMETERS
   -- Member     - Crew member which skill will be looking for
   -- SkillIndex - Index of skill in skills list
   -- RESULT
   -- Real level of selected skill of selected crew member
   -- SOURCE
   function GetSkillLevel
     (Member: Member_Data; SkillIndex: SkillsData_Container.Extended_Index)
      return Skill_Range with
      Pre => SkillIndex in Skills_List.First_Index .. Skills_List.Last_Index,
      Test_Case => (Name => "Test_GetSkillLevel", Mode => Nominal);
      -- ****

      -- ****f* SCrew/SCrew.Death
      -- FUNCTION
      -- Handle crew member death
      -- PARAMETERS
      -- MemberIndex - Crew index of the member which died
      -- Reason      - Reason of the death
      -- Ship        - Ship in which crew member died
      -- CreateBody  - If true, create body for dead crew member. Default is
      --               true
      -- RESULT
      -- Parameter Ship with updated data (crew, cargo, modules)
      -- SOURCE
   procedure Death
     (MemberIndex: Crew_Container.Extended_Index; Reason: Unbounded_String;
      Ship: in out Ship_Record; CreateBody: Boolean := True) with
      Pre =>
      (MemberIndex in Ship.Crew.First_Index .. Ship.Crew.Last_Index and
       Reason /= Null_Unbounded_String),
      Test_Case => (Name => "Test_Death", Mode => Nominal);
      -- ****

      -- ****f* SCrew/SCrew.DeleteMember
      -- FUNCTION
      -- Delete selected member from crew list
      -- PARAMETERS
      -- MemberIndex - Crew index of the member which will be deleted
      -- Ship        - Ship which crew will be modified
      -- RESULT
      -- Parameter Ship with modified data (crew and modules)
      -- SOURCE
   procedure DeleteMember
     (MemberIndex: Crew_Container.Extended_Index;
      Ship: in out Ship_Record) with
      Pre => MemberIndex in Ship.Crew.First_Index .. Ship.Crew.Last_Index,
      Test_Case => (Name => "Test_DeleteMember", Mode => Nominal);
      -- ****

   -- ****f* SCrew/SCrew.FindMember
   -- FUNCTION
   -- Find index of first crew member with selected order
   -- PARAMETERS
   -- Order - Current crew order which will be looking for
   -- Crew  - Crew of ship which will be searched
   -- RESULT
   -- Crew index of crew member with selected order or 0 if nothing was found
   -- SOURCE
   function FindMember
     (Order: Crew_Orders; Crew: Crew_Container.Vector := Player_Ship.Crew)
      return Crew_Container.Extended_Index with
      Test_Case => (Name => "Test_FindMember", Mode => Robustness);
      -- ****

      -- ****f* SCrew/SCrew.GiveOrders
      -- FUNCTION
      -- Change order for selected crew member
      -- PARAMETERS
      -- Ship            - Ship in which crew member will be have changed order
      -- MemberIndex     - Crew index of member to change order
      -- GivenOrder      - New order for selected crew member
      -- ModuleIndex     - Index of module to assign to crew member with new
      --                   order. Default is 0 - no module assigned
      -- CheckPriorities - If true, check orders priorities of whole crew.
      --                   Default is true
      -- Result
      -- Parameter Ship with modified data (crew, modules, cargo)
      -- SOURCE
   procedure GiveOrders
     (Ship: in out Ship_Record; MemberIndex: Crew_Container.Extended_Index;
      GivenOrder: Crew_Orders;
      ModuleIndex: Modules_Container.Extended_Index := 0;
      CheckPriorities: Boolean := True) with
      Pre =>
      (MemberIndex in Ship.Crew.First_Index .. Ship.Crew.Last_Index and
       ModuleIndex <= Ship.Modules.Last_Index),
      Test_Case => (Name => "Test_GiveOrders", Mode => Nominal);
      -- ****

      -- ****f* SCrew/SCrew.UpdateOrders
      -- FUNCTION
      -- Update crew orders based on their orders priorities
      -- PARAMETERS
      -- Ship   - Ship in which crew will be check
      -- Combat - If true, ship is in combat. Default is false
      -- RESULT
      -- Parameter Ship with modified data (crew, modules, cargo)
      -- SOURCE
   procedure UpdateOrders
     (Ship: in out Ship_Record; Combat: Boolean := False) with
      Test_Case => (Name => "Test_UpdateOrders", Mode => Robustness);
      -- ****

      -- ****f* SCrew/SCrew.UpdateMorale
      -- FUNCTION
      -- Update morale of selected crew member by value
      -- PARAMETERS
      -- Ship        - Ship on which crew member will be have updated morale
      -- MemberIndex - Crew index of member to update morale
      -- Value       - Amount of morale to add or substract
      -- RESULT
      -- Parameter Ship with modified crew info
      -- SOURCE
   procedure UpdateMorale
     (Ship: in out Ship_Record; MemberIndex: Crew_Container.Extended_Index;
      Value: Integer) with
      Pre => MemberIndex in Ship.Crew.First_Index .. Ship.Crew.Last_Index,
      Test_Case => (Name => "Test_UpdateMorale", Mode => Nominal);
      -- ****

end Ships.Crew;
