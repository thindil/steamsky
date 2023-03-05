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
with Game;
with Statistics;

package body HallOfFame is

   --## rule off TYPE_INITIAL_VALUES
   type Nim_Hall_Of_Fame_Data is record
      Name: chars_ptr;
      Points: Natural := 0;
      Death_Reason: chars_ptr;
   end record;
   --## rule on TYPE_INITIAL_VALUES
   procedure Get_Ada_Hof_Entry
     (Index: Natural; N_Entry: out Nim_Hall_Of_Fame_Data) with
      Import => True,
      Convention => C,
      External_Name => "getAdaHofEntry";

   procedure Load_Hall_Of_Fame is
      use Interfaces.C;
      use Game;

      --## rule off IMPROPER_INITIALIZATION
      Nim_Entry: Nim_Hall_Of_Fame_Data;
      Result: chars_ptr;
      --## rule on IMPROPER_INITIALIZATION
      function Load_Ada_Hall_Of_Fame return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "loadAdaHallOfFame";
   begin
      Result := Load_Ada_Hall_Of_Fame;
      if Strlen(Item => Result) > 0 then
         raise Data_Loading_Error with Value(Item => Result);
      end if;
      Load_Hof_Loop :
      for I in 1 .. 10 loop
         Get_Ada_Hof_Entry(Index => I, N_Entry => Nim_Entry);
         if Strlen(Item => Nim_Entry.Name) > 0 then
            Hall_Of_Fame_Array(I) :=
              (Name =>
                 To_Unbounded_String(Source => Value(Item => Nim_Entry.Name)),
               Points => Nim_Entry.Points,
               Death_Reason =>
                 To_Unbounded_String
                   (Source => Value(Item => Nim_Entry.Death_Reason)));
         end if;
      end loop Load_Hof_Loop;
   end Load_Hall_Of_Fame;

   procedure Update_Hall_Of_Fame
     (Player_Name, Death_Reason: Unbounded_String) is
      use Interfaces.C;
      use Statistics;

      --## rule off IMPROPER_INITIALIZATION
      Nim_Entry: Nim_Hall_Of_Fame_Data;
      --## rule on IMPROPER_INITIALIZATION
      procedure Update_Ada_Hall_Of_Fame(P_Name, D_Reason: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaHallOfFame";
   begin
      Get_Game_Stats;
      Update_Ada_Hall_Of_Fame
        (P_Name => New_String(Str => To_String(Source => Player_Name)),
         D_Reason => New_String(Str => To_String(Source => Death_Reason)));
      Load_Hof_Loop :
      for I in 1 .. 10 loop
         Get_Ada_Hof_Entry(Index => I, N_Entry => Nim_Entry);
         if Strlen(Item => Nim_Entry.Name) > 0 then
            Hall_Of_Fame_Array(I) :=
              (Name =>
                 To_Unbounded_String(Source => Value(Item => Nim_Entry.Name)),
               Points => Nim_Entry.Points,
               Death_Reason =>
                 To_Unbounded_String
                   (Source => Value(Item => Nim_Entry.Death_Reason)));
         end if;
      end loop Load_Hof_Loop;
   end Update_Hall_Of_Fame;

end HallOfFame;
