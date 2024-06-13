--    Copyright 2016-2023 Bartek thindil Jasicki
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

with Interfaces.C.Strings;
with Maps; use Maps;
with Ships; use Ships;

package body Events is

   function Check_For_Event return Boolean is
      Result: Integer;
      function Check_Ada_For_Event return Integer with
         Import => True,
         Convention => C,
         External_Name => "checkAdaForEvent";
   begin
      Set_Ship_In_Nim;
      Result := Check_Ada_For_Event;
      Get_Ship_From_Nim(Ship => Player_Ship);
      Set_Map_Cell(X => Player_Ship.Sky_X, Y => Player_Ship.Sky_Y);
      Set_Events_In_Ada_Loop :
      for I in 1 .. Get_Events_Amount loop
         Set_Event(Index => I);
      end loop Set_Events_In_Ada_Loop;
      if Result = 1 then
         return True;
      end if;
      return False;
   end Check_For_Event;

   procedure Delete_Event(Event_Index: Positive) is
      procedure Delete_Ada_Event(E_Index: Integer) with
         Import => True,
         Convention => C,
         External_Name => "deleteAdaEvent";
   begin
      Delete_Ada_Event(E_Index => Event_Index);
      Sky_Map
        (Get_Event(Index => Event_Index).Sky_X,
         Get_Event(Index => Event_Index).Sky_Y)
        .Event_Index :=
        0;
      Delete_Events_Loop :
      for I in 1 .. Get_Events_Amount loop
         Sky_Map(Get_Event(Index => I).Sky_X, Get_Event(Index => I).Sky_Y)
           .Event_Index :=
           I;
      end loop Delete_Events_Loop;
   end Delete_Event;

   procedure Generate_Traders is
      procedure Generate_Ada_Traders with
         Import => True,
         Convention => C,
         External_Name => "generateAdaTraders";
   begin
      Generate_Ada_Traders;
   end Generate_Traders;

   procedure Generate_Enemies
     (Enemies: in out Positive_Container.Vector;
      Owner: Tiny_String.Bounded_String :=
        Tiny_String.To_Bounded_String(Source => "Any");
      With_Traders: Boolean := True) is
      use Interfaces.C.Strings;

      --## rule off TYPE_INITIAL_VALUES
      type Nim_Ships_Array is array(0 .. 299) of Natural;
      --## rule on TYPE_INITIAL_VALUES
      Nim_Ships: Nim_Ships_Array;
      procedure Generate_Ada_Enemies
        (E: out Nim_Ships_Array; O: chars_ptr; W_Traders: Integer) with
         Import => True,
         Convention => C,
         External_Name => "generateAdaEnemies";
   begin
      Generate_Ada_Enemies
        (E => Nim_Ships,
         O => New_String(Str => Tiny_String.To_String(Source => Owner)),
         W_Traders => (if With_Traders then 1 else 0));
      Convert_Ships_Loop :
      for Ship of Nim_Ships loop
         exit Convert_Ships_Loop when Ship = 0;
         Enemies.Append(New_Item => Ship);
      end loop Convert_Ships_Loop;
   end Generate_Enemies;

   procedure Set_Event(Index: Positive) is
      X, Y, Time, E_Type, Data: Integer;
      procedure Set_Ada_Event(I: Positive; X1, Y1, T, E, D: out Integer) with
         Import => True,
         Convention => C,
         External_Name => "setAdaEvent";
   begin
      Set_Ada_Event
        (I => Index, X1 => X, Y1 => Y, T => Time, E => E_Type, D => Data);
      if X = -1 then
         return;
      end if;
      Sky_Map(X, Y).Event_Index := Index;
   end Set_Event;

   function Get_Event(Index: Positive) return Event_Data is
      X, Y, Time, E_Type, Data: Integer;
      procedure Set_Ada_Event(I: Positive; X1, Y1, T, E, D: out Integer) with
         Import => True,
         Convention => C,
         External_Name => "setAdaEvent";
   begin
      Set_Ada_Event
        (I => Index, X1 => X, Y1 => Y, T => Time, E => E_Type, D => Data);
      if X = -1 then
         return Empty_Event;
      end if;
      Sky_Map(X, Y).Event_Index := Index;
      case E_Type is
         when 1 =>
            return
              (E_Type => ENEMYSHIP, Sky_X => X, Sky_Y => Y, Time => Time,
               Ship_Index => Data);
         when 2 =>
            return
              (E_Type => ATTACKONBASE, Sky_X => X, Sky_Y => Y, Time => Time,
               Ship_Index => Data);
         when 3 =>
            return
              (E_Type => DISEASE, Sky_X => X, Sky_Y => Y, Time => Time,
               Data => Data);
         when 4 =>
            return
              (E_Type => DOUBLEPRICE, Sky_X => X, Sky_Y => Y, Time => Time,
               Item_Index => Data);
         when 6 =>
            return
              (E_Type => FULLDOCKS, Sky_X => X, Sky_Y => Y, Time => Time,
               Data => Data);
         when 7 =>
            return
              (E_Type => ENEMYPATROL, Sky_X => X, Sky_Y => Y, Time => Time,
               Ship_Index => Data);
         when 8 =>
            return
              (E_Type => TRADER, Sky_X => X, Sky_Y => Y, Time => Time,
               Ship_Index => Data);
         when 9 =>
            return
              (E_Type => FRIENDLYSHIP, Sky_X => X, Sky_Y => Y, Time => Time,
               Ship_Index => Data);
         when others =>
            return Empty_Event;
      end case;
   end Get_Event;

end Events;
