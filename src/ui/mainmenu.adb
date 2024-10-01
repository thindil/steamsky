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

with Goals.UI;
with MainMenu.Commands;
with Table;
with Utils.UI;

package body MainMenu is

   procedure Create_Main_Menu is
      procedure Create_Ada_Main_Menu with
         Convention => C,
         Import => True,
         External_Name => "createAdaMainMenu";
   begin
      MainMenu.Commands.Add_Commands;
      Utils.UI.Add_Commands;
      Goals.UI.Add_Commands;
      Table.Add_Commands;
      Create_Ada_Main_Menu;
      Show_Main_Menu;
   end Create_Main_Menu;

end MainMenu;
