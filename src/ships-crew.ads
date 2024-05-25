--    Copyright 2017-2024 Bartek thindil Jasicki
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
      Pre => Member_Index in Ship.Crew.First_Index .. Ship.Crew.Last_Index and
      Reason /= Null_Unbounded_String;
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
      Pre => Member_Index in Ship.Crew.First_Index .. Ship.Crew.Last_Index;
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
     (Order: Crew_Orders; Ship_Crew: Crew_Container.Vector := Player_Ship.Crew)
      return Crew_Container.Extended_Index with
      Post => Find_Member'Result <= Ship_Crew.Last_Index;
      -- ****

      -- ****f* SCrew/SCrew.Update_Morale
      -- FUNCTION
      -- Update morale of selected crew member by value
      -- PARAMETERS
      -- Ship         - Ship on which crew member will be have updated morale
      -- Member_Index - Crew index of member to update morale
      -- Amount       - Amount of morale to add or substract
      -- RESULT
      -- Parameter Ship with modified crew info
      -- SOURCE
   procedure Update_Morale
     (Ship: in out Ship_Record; Member_Index: Crew_Container.Extended_Index;
      Amount: Integer) with
      Pre => Member_Index in Ship.Crew.First_Index .. Ship.Crew.Last_Index;
      -- ****

end Ships.Crew;
