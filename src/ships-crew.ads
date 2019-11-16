--    Copyright 2017-2019 Bartek thindil Jasicki
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

-- ****h* Steamsky/Ships.Crew
-- FUNCTION
-- Provides code for manipulate ships crews
-- SOURCE
package Ships.Crew is
-- ****

   -- ****f* Ships.Crew/GetSkillLevel
   -- FUNCTION
   -- Get level of skill of selected crew member
   -- PARAMETERS
   -- Member     - Crew member which skill will be looking for
   -- SkillIndex - Index of skill in skills list
   -- RESULT
   -- Real level of selected skill of selected crew member
   -- SOURCE
   function GetSkillLevel
     (Member: Member_Data; SkillIndex: Positive) return Natural with
      Pre => SkillIndex <= Skills_List.Last_Index,
      Test_Case => ("Test_GetSkillLevel", Nominal);
      -- ****

      -- ****f* Ships.Crew/Death
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
     (MemberIndex: Positive; Reason: Unbounded_String; Ship: in out ShipRecord;
      CreateBody: Boolean := True) with
      Pre =>
      (MemberIndex <= Ship.Crew.Last_Index and
       Reason /= Null_Unbounded_String),
      Test_Case => ("Test_Death", Nominal);
      -- ****

      -- ****f* Ships.Crew/DeleteMember
      -- FUNCTION
      -- Delete selected member from crew list
      -- PARAMETERS
      -- MemberIndex - Crew index of the member which will be deleted
      -- Ship        - Ship which crew will be modified
      -- RESULT
      -- Parameter Ship with modified data (crew and modules)
      -- SOURCE
   procedure DeleteMember(MemberIndex: Positive; Ship: in out ShipRecord) with
      Pre => MemberIndex <= Ship.Crew.Last_Index,
      Test_Case => ("Test_DeleteMember", Nominal);
      -- ****

   -- ****f* Ships.Crew/FindMember
   -- FUNCTION
   -- Find index of first crew member with selected order
   -- PARAMETERS
   -- Order - Current crew order which will be looking for
   -- Crew  - Crew of ship which will be searched
   -- RESULT
   -- Crew index of crew member with selected order or 0 if nothing was found
   -- SOURCE
   function FindMember
     (Order: Crew_Orders; Crew: Crew_Container.Vector := PlayerShip.Crew)
      return Natural with
      Test_Case => ("Test_FindMember", Robustness);
      -- ****

      -- ****f* Ships.Crew/GiveOrders
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
     (Ship: in out ShipRecord; MemberIndex: Positive; GivenOrder: Crew_Orders;
      ModuleIndex: Natural := 0; CheckPriorities: Boolean := True) with
      Pre =>
      (MemberIndex <= Ship.Crew.Last_Index and
       ModuleIndex <= Ship.Modules.Last_Index),
      Test_Case => ("Test_GiveOrders", Nominal);
      -- ****

      -- ****f* Ships.Crew/UpdateOrders
      -- FUNCTION
      -- Update crew orders based on their orders priorities
      -- PARAMETERS
      -- Ship   - Ship in which crew will be check
      -- Combat - If true, ship is in combat. Default is false
      -- RESULT
      -- Parameter Ship with modified data (crew, modules, cargo)
      -- SOURCE
   procedure UpdateOrders
     (Ship: in out ShipRecord; Combat: Boolean := False) with
      Test_Case => ("Test_UpdateOrders", Robustness);
      -- ****

      -- ****f* Ships.Crew/UpdateMorale
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
     (Ship: in out ShipRecord; MemberIndex: Positive; Value: Integer) with
      Pre => MemberIndex <= Ship.Crew.Last_Index,
      Test_Case => ("Test_UpdateMorale", Nominal);
      -- ****

end Ships.Crew;
