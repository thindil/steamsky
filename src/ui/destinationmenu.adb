-- Copyright (c) 2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Dialogs; use Dialogs;
with Utils.UI; use Utils.UI;

package body DestinationMenu is

   function Show_Destination_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      DestinationDialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => "gameframe.destinationmenu", Title => "Set destination",
           Parent_Name => ".gameframe");
      Button: Ttk_Button :=
        Create(DestinationDialog & ".set", "-text {Set destination}");
   begin
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -padx 5");
      Button :=
        Create
          (DestinationDialog & ".setandmove",
           "-text {Set destination and move}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -padx 5");
      Add_Close_Button
        (DestinationDialog & ".button", "Close",
         "CloseDialog " & DestinationDialog);
      Show_Dialog(DestinationDialog, ".gameframe");
      return TCL_OK;
   end Show_Destination_Menu_Command;

   procedure AddCommands is
   begin
      Add_Command("ShowDestinationMenu", Show_Destination_Menu_Command'Access);
   end AddCommands;

end DestinationMenu;
