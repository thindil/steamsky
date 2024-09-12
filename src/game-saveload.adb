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

with Bases; use Bases;
with Maps; use Maps;
with Ships; use Ships;

package body Game.SaveLoad is

   procedure Save_Game(Pretty_Print: Boolean := False) is
      procedure Save_Ada_Game(P_Print: Integer) with
         Import => True,
         Convention => C,
         External_Name => "saveAdaGame";
   begin
      Set_Ship_In_Nim;
      Get_Bases_Loop :
      for I in Bases_Range loop
         Set_Base_In_Nim(Base_Index => I);
      end loop Get_Bases_Loop;
      Get_Map_Y_Loop :
      for Y in Map_Y_Range loop
         Get_Map_X_Loop :
         for X in Map_X_Range loop
            Get_Ada_Map_Cell
              (X => X, Y => Y, Base_Index => Sky_Map(X, Y).Base_Index,
               Visited => (if Sky_Map(X, Y).Visited then 1 else 0),
               Event_Index => Sky_Map(X, Y).Event_Index,
               Mission_Index => Sky_Map(X, Y).Mission_Index);
         end loop Get_Map_X_Loop;
      end loop Get_Map_Y_Loop;
      Save_Ada_Game(P_Print => (if Pretty_Print then 1 else 0));
   end Save_Game;

end Game.SaveLoad;
