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

with Ada.Exceptions;
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

   procedure Load_Game is
      use Ada.Exceptions;

      procedure Load_Ada_Game with
         Import => True,
         Convention => C,
         External_Name => "loadAdaGame";
   begin
      Get_Ada_Save_Name
        (Name => New_String(Str => To_String(Source => Save_Name)));
      Load_Ada_Game;
      Get_Ship_From_Nim(Ship => Player_Ship);
      Get_Bases_Loop :
      for I in Sky_Bases'Range loop
         Get_Base_From_Nim(Base_Index => I);
      end loop Get_Bases_Loop;
      Get_Map_Y_Loop :
      for Y in 1 .. 1_024 loop
         Get_Map_X_Loop :
         for X in 1 .. 1_024 loop
            Set_Map_Cell(X => X, Y => Y);
         end loop Get_Map_X_Loop;
      end loop Get_Map_Y_Loop;
      Set_Game_Date;
   exception
      when An_Exception : others =>
         Player_Ship.Crew.Clear;
         raise Save_Game_Invalid_Data
           with Exception_Message(X => An_Exception);
   end Load_Game;

   procedure Generate_Save_Name(Rename_Save: Boolean := False) is
      New_Name: chars_ptr;
      procedure Generate_Ada_Save_Name(R_Save: Integer) with
         Import => True,
         Convention => C,
         External_Name => "generateAdaSaveName";
      procedure Set_Ada_Save_Name(Name: out chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "setAdaSaveName";
   begin
      Generate_Ada_Save_Name(R_Save => (if Rename_Save then 1 else 0));
      Set_Ada_Save_Name(Name => New_Name);
      Save_Name := To_Unbounded_String(Source => Value(Item => New_Name));
   end Generate_Save_Name;

end Game.SaveLoad;
