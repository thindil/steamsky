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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with Bases;
with Maps; use Maps;

package body Ships.Movement is

   function Move_Ship
     (X, Y: Integer; Message: in out Unbounded_String) return Natural is
      Nim_Message: chars_ptr;
      Result: Natural := 0;
      function Move_Ada_Ship
        (Nx, Ny: Integer; Msg: out chars_ptr) return Natural with
         Import => True,
         Convention => C,
         External_Name => "moveAdaShip";
   begin
      Get_Game_Date;
      Set_Ship_In_Nim;
      Get_Ada_Map_Cell
        (X => Player_Ship.Sky_X, Y => Player_Ship.Sky_Y,
         Base_Index =>
           Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index,
         Visited =>
           (if Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Visited then 1
            else 0),
         Event_Index =>
           Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index,
         Mission_Index =>
           Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Mission_Index);
      Result := Move_Ada_Ship(Nx => X, Ny => Y, Msg => Nim_Message);
      Message := To_Unbounded_String(Source => Value(Item => Nim_Message));
      Get_Ship_From_Nim(Ship => Player_Ship);
      Set_Game_Date;
      Set_Map_Cell(X => Player_Ship.Sky_X, Y => Player_Ship.Sky_Y);
      return Result;
   end Move_Ship;

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

   function Real_Speed
     (Ship: Ship_Record; Info_Only: Boolean := False) return Natural is
      function Real_Ada_Speed
        (Of_Player_Ship, I_Only: Integer) return Integer with
         Import => True,
         Convention => C,
         External_Name => "realAdaSpeed";
   begin
      Set_Ship_In_Nim(Ship => Ship);
      return
        Real_Ada_Speed
          (Of_Player_Ship => (if Ship = Player_Ship then 1 else 0),
           I_Only => (if Info_Only then 1 else 0));
   end Real_Speed;

   function Count_Fuel_Needed return Integer is
      function Count_Ada_Fuel_Needed return Integer with
         Import => True,
         Convention => C,
         External_Name => "countAdaFuelNeeded";
   begin
      Set_Ship_In_Nim;
      return Count_Ada_Fuel_Needed;
   end Count_Fuel_Needed;

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
