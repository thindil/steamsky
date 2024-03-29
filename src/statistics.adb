--    Copyright 2016-2024 Bartek thindil Jasicki
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

with Ada.Strings;
with Interfaces.C.Strings;

package body Statistics is

   procedure Clear_Game_Stats is
      procedure Clear_Ada_Game_Stats with
         Import => True,
         Convention => C,
         External_Name => "clearAdaGameStats";
   begin
      Clear_Ada_Game_Stats;
   end Clear_Game_Stats;

   function Get_Game_Points return Natural is
      function Get_Ada_Game_Points return Natural with
         Import => True,
         Convention => C,
         External_Name => "getAdaGamePoints";
   begin
      return Get_Ada_Game_Points;
   end Get_Game_Points;

   function Get_Game_Stats_List
     (Name: String) return Statistics_Container.Vector is
      use Interfaces.C;
      use Interfaces.C.Strings;

      --## rule off TYPE_INITIAL_VALUES
      type Nim_Statistics_Data is record
         Index: chars_ptr;
         Amount: Integer;
      end record;
      type Nim_Stats_List is array(0 .. 511) of Nim_Statistics_Data;
      --## rule on TYPE_INITIAL_VALUES
      --## rule off IMPROPER_INITIALIZATION
      Nim_List: Nim_Stats_List := (others => <>);
      Ada_List: Statistics_Container.Vector;
      --## rule on IMPROPER_INITIALIZATION
      procedure Set_Ada_Game_Stats_List
        (N: chars_ptr; Stats_List: out Nim_Stats_List) with
         Import => True,
         Convention => C,
         External_Name => "setAdaGameStatsList";
   begin
      Set_Ada_Game_Stats_List
        (N => New_String(Str => Name), Stats_List => Nim_List);
      Set_List_Loop :
      for Item of Nim_List loop
         exit Set_List_Loop when Strlen(Item => Item.Index) = 0;
         Ada_List.Append
           (New_Item =>
              (Index =>
                 To_Unbounded_String(Source => Value(Item => Item.Index)),
               Amount => Item.Amount));
      end loop Set_List_Loop;
      return Ada_List;
   end Get_Game_Stats_List;

end Statistics;
