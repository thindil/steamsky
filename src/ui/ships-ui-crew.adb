-- Copyright (c) 2020-2024 Bartek thindil Jasicki
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Ships.UI.Crew.Inventory;
with Table; use Table;

package body Ships.UI.Crew is

   procedure Update_Crew_Info(Page: Positive := 1; Skill: Natural := 0) is

      --## rule off TYPE_INITIAL_VALUES
      type Crew_Array is array(0 .. 50) of Natural;
      --## rule on TYPE_INITIAL_VALUES
      C_Array: Crew_Array := (others => 0);
      N_Width: Nim_Width := (others => 0);
      Index: Natural := 0;
      Crew_Table: Table_Widget (Amount => 9);
      Crew_Indexes: Positive_Container.Vector;
      procedure Update_Ada_Crew_Info
        (P: Positive; S: Natural; M: Crew_Array; W: out Nim_Width;
         Row, Height: out Positive) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaCrewInfo";
   begin
      if Crew_Indexes.Length /= Player_Ship.Crew.Length then
         Crew_Indexes.Clear;
         Update_Crew_Indexes_Loop :
         for I in Player_Ship.Crew.Iterate loop
            Crew_Indexes.Append
              (New_Item => Crew_Container.To_Index(Position => I));
         end loop Update_Crew_Indexes_Loop;
      end if;
      Convert_Crew_Indexes_Loop :
      for C_Index of Crew_Indexes loop
         C_Array(Index) := C_Index;
         Index := Index + 1;
      end loop Convert_Crew_Indexes_Loop;
      Update_Ada_Crew_Info
        (P => Page, S => Skill, M => C_Array, W => N_Width,
         Row => Crew_Table.Row, Height => Crew_Table.Row_Height);
   end Update_Crew_Info;

   procedure Add_Crew_Commands is
      procedure Add_Ada_Commands with
         Import => True,
         Convention => C,
         External_Name => "addAdaCrewCommands";
   begin
      Add_Ada_Commands;
      Ships.UI.Crew.Inventory.Add_Inventory_Commands;
   end Add_Crew_Commands;

end Ships.UI.Crew;
