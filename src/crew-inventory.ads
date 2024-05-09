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

with Ships; use Ships; --## rule line off REDUCEABLE_SCOPE

-- ****h* Crew/Inventory
-- FUNCTION
-- Provide code for manipulate player ship crew members inventory
-- SOURCE
package Crew.Inventory is
-- ****

      -- ****f* Inventory/Inventory.Free_Inventory
      -- FUNCTION
      -- Return available space in crew member inventory after adding or
      -- extracting Amount
      -- PARAMETERS
      -- Member_Index - Crew index of the member which inventory will be checked
      -- Amount       - Amount of kilogram to add or remove during check
      -- Update_Nim   - If true, update crew and the selected inventory in Nim
      -- RESULT
      -- Amount of available space in kilograms
      -- HISTORY
      -- 7.5 - Renamed to Free_Inventory, changed parameter name to Member_Index
      -- SOURCE
   function Free_Inventory
     (Member_Index: Positive; Amount: Integer; Update_Nim: Boolean := True)
      return Integer with
      Pre => Member_Index <= Player_Ship.Crew.Last_Index;
      -- ****

end Crew.Inventory;
