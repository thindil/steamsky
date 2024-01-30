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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with Bases;
with Maps; use Maps;

package body Ships.Movement is

   function Dock_Ship
     (Docking: Boolean; Escape: Boolean := False) return String is
      use Bases;
      function Dock_Ada_Ship(D, E: Integer) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "dockAdaShip";
      Message: Unbounded_String;
      Base_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
   begin
      if Base_Index > 0 then
         Set_Base_In_Nim(Base_Index => Base_Index);
      end if;
      Get_Game_Date;
      Set_Ship_In_Nim;
      Message :=
        To_Unbounded_String
          (Source =>
             Value
               (Item =>
                  Dock_Ada_Ship
                    (D => (if Docking then 1 else 0),
                     E => (if Escape then 1 else 0))));
      Get_Ship_From_Nim(Ship => Player_Ship);
      Set_Game_Date;
      return To_String(Source => Message);
   end Dock_Ship;

   function Change_Ship_Speed(Speed_Value: Ship_Speed) return String is
      Result: Unbounded_String;
      function Change_Ada_Ship_Speed(S_Value: Integer) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "changeAdaShipSpeed";
   begin
      Set_Ship_In_Nim;
      Result :=
        To_Unbounded_String
          (Source =>
             Value
               (Item =>
                  Change_Ada_Ship_Speed
                    (S_Value => Ship_Speed'Pos(Speed_Value))));
      Get_Ship_From_Nim(Ship => Player_Ship);
      return To_String(Source => Result);
   end Change_Ship_Speed;

   procedure Wait_In_Place(Minutes: Positive) is
      procedure Wait_Ada_In_Place(M: Positive) with
         Import => True,
         Convention => C,
         External_Name => "waitAdaInPlace";
   begin
      Set_Ship_In_Nim;
      Wait_Ada_In_Place(M => Minutes);
      Get_Ship_From_Nim(Ship => Player_Ship);
   end Wait_In_Place;

end Ships.Movement;
