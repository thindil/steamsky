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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Cargv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
with Config; use Config;
with Game; use Game;
with Utils.UI; use Utils.UI;

package body Help.UI is

   -- ****f* HUI/Show_Help_Command
   -- FUNCTION
   -- Show help window to the playera
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Help_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Help_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      HelpWindow: Tk_Toplevel;
      X, Y: Integer;
   begin
      Tcl_EvalFile
         (Interp,
         To_String(DataDirectory) & "ui" & Dir_Separator & "help.tcl");
      HelpWindow.Interp := Interp;
      HelpWindow.Name := New_String(".help");
      X :=
        (Positive'Value(Winfo_Get(HelpWindow, "vrootwidth")) -
         GameSettings.WindowWidth) /
        2;
      if X < 0 then
         X := 0;
      end if;
      Y :=
        (Positive'Value(Winfo_Get(HelpWindow, "vrootheight")) -
         GameSettings.WindowHeight) /
        2;
      if Y < 0 then
         Y := 0;
      end if;
      Wm_Set
        (HelpWindow, "geometry",
         Trim(Positive'Image(GameSettings.WindowWidth), Left) & "x" &
         Trim(Positive'Image(GameSettings.WindowHeight), Left) & "+" &
         Trim(Positive'Image(X), Left) & "+" & Trim(Positive'Image(Y), Left));
      Bind(HelpWindow, "<Escape>", "{destroy .help}");
      return TCL_OK;
   end Show_Help_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowHelp", Show_Help_Command'Access);
   end AddCommands;

end Help.UI;
