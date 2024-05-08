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

package body Crew.Inventory is

   function Free_Inventory
     (Member_Index: Positive; Amount: Integer; Update_Nim: Boolean := True)
      return Integer is
      function Free_Ada_Inventory(M_Index, Amnt: Integer) return Integer with
         Import => True,
         Convention => C,
         External_Name => "freeAdaInventory";
   begin
      if Update_Nim then
         Get_Ada_Crew;
         Get_Ada_Crew_Inventory
           (Inventory =>
              Inventory_To_Nim
                (Inventory => Player_Ship.Crew(Member_Index).Inventory),
            Member_Index => Member_Index);
      end if;
      return Free_Ada_Inventory(M_Index => Member_Index, Amnt => Amount);
   end Free_Inventory;

end Crew.Inventory;
