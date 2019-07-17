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

package Ships.Crew is

-- ****f* Ships.Crew/GetSkillLevel
-- FUNCTION
-- Get level of skill of selected crew member
-- SOURCE
   function GetSkillLevel
     (Member: Member_Data; SkillIndex: Positive) return Natural;
-- ****
-- ****f* Ships.Crew/Death
-- FUNCTION
-- Handle crew member death
-- SOURCE
   procedure Death
     (MemberIndex: Positive; Reason: Unbounded_String; Ship: in out ShipRecord;
      CreateBody: Boolean := True) with
      Pre =>
      (MemberIndex <= Ship.Crew.Last_Index and
       Reason /= Null_Unbounded_String);
-- ****
-- ****f* Ships.Crew/DeleteMember
-- FUNCTION
-- Delete selected member from crew list
-- SOURCE
   procedure DeleteMember(MemberIndex: Positive; Ship: in out ShipRecord);
-- ****
-- ****f* Ships.Crew/FindMember
-- FUNCTION
-- Find index of first crew member with selected order
-- SOURCE
   function FindMember
     (Order: Crew_Orders; Crew: Crew_Container.Vector := PlayerShip.Crew)
      return Natural;
-- ****
-- ****f* Ships.Crew/GiveOrders
-- FUNCTION
-- Change order for selected crew member
-- SOURCE
   procedure GiveOrders
     (Ship: in out ShipRecord; MemberIndex: Positive; GivenOrder: Crew_Orders;
      ModuleIndex: Natural := 0; CheckPriorities: Boolean := True) with
      Pre =>
      (MemberIndex <= Ship.Crew.Last_Index and
       ModuleIndex <= Ship.Modules.Last_Index);
-- ****
-- ****f* Ships.Crew/UpdateOrders
-- FUNCTION
-- Update crew orders based on their orders priorities
-- SOURCE
   procedure UpdateOrders(Ship: in out ShipRecord; Combat: Boolean := False);
-- ****
-- ****f* Ships.Crew/UpdateMorale
-- FUNCTION
-- Update morale of selected crew member by value
-- SOURCE
   procedure UpdateMorale
     (Ship: in out ShipRecord; MemberIndex: Positive; Value: Integer) with
      Pre => MemberIndex <= Ship.Crew.Last_Index;
-- ****

end Ships.Crew;
