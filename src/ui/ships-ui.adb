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

with Ships.UI.Crew;
with Ships.UI.Cargo;
with Ships.UI.Modules;

package body Ships.UI is

   procedure Add_Commands is
      procedure Add_Ada_Commands with
         Import => True,
         Convention => C,
         External_Name => "addAdaShipsCommands";
   begin
      Add_Ada_Commands;
      Ships.UI.Modules.Add_Modules_Commands;
      Ships.UI.Crew.Add_Crew_Commands;
      Ships.UI.Cargo.Add_Cargo_Commands;
   end Add_Commands;

end Ships.UI;
