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

with Maps; use Maps;

package body Bases.Cargo is

   function Find_Base_Cargo
     (Proto_Index: Natural;
      Durability: Items_Durability := Items_Durability'Last) return Natural is
      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      function Find_Ada_Base_Cargo
        (P_Index: Natural; D: Integer) return Integer with
         Import => True,
         Convention => C,
         External_Name => "findAdaBaseCargo";
   begin
      Set_Ship_In_Nim;
      Get_Base_Cargo(Base_Index => Base_Index);
      return Find_Ada_Base_Cargo(P_Index => Proto_Index, D => Durability);
   end Find_Base_Cargo;

end Bases.Cargo;
