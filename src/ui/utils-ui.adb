-- Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Tcl.Tk.Ada.Dialogs; use Tcl.Tk.Ada.Dialogs;

package body Utils.UI is

   procedure ShowMessage(Text, Icon: String) is
      Dummy: constant String := MessageBox
                ("-message {" & Text & "} -icon " & Icon &" -type ok");
   begin
      null;
   end ShowMessage;

end Utils.UI;
