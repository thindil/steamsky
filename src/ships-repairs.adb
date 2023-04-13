--    Copyright 2017-2023 Bartek thindil Jasicki
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

package body Ships.Repairs is

   procedure Repair_Ship(Minutes: Positive) is
      procedure Repair_Ada_Ship(M: Integer) with
         Import => True,
         Convention => C,
         External_Name => ("repairAdaShip");
   begin
      Set_Ship_In_Nim;
      Repair_Ada_Ship(M => Minutes);
      Get_Ship_From_Nim(Ship => Player_Ship);
   end Repair_Ship;

end Ships.Repairs;
