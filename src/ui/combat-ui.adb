-- Copyright (c) 2020-2023 Bartek thindil Jasicki
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

package body Combat.UI is

   procedure Show_Combat_Ui(New_Combat: Boolean := True) is
      procedure Show_Ada_Combat_Ui(N_Combat: Integer) with
         Import => True,
         Convention => C,
         External_Name => "showAdaCombatUi";
   begin
      Show_Ada_Combat_Ui(N_Combat => (if New_Combat then 1 else 0));
   end Show_Combat_Ui;

end Combat.UI;
