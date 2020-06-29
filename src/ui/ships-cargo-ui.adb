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

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Factions; use Factions;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Missions; use Missions;
with Utils.UI; use Utils.UI;

package body Ships.Cargo.UI is

   -- ****f* CUI2/Show_Cargo_Info_Command
   -- FUNCTION
   -- Show information about the player's ship
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Cargo_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Cargo_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argv);
      Label: Ttk_Label;
      Paned: Ttk_PanedWindow;
      CargoCanvas: Tk_Canvas;
      CargoFrame: Ttk_Frame;
      CloseButton: Ttk_Button;
   begin
      Paned.Interp := Interp;
      Paned.Name := New_String(".paned");
      CloseButton.Interp := Interp;
      CloseButton.Name := New_String(".header.closebutton");
      CargoFrame.Interp := Interp;
      CargoFrame.Name := New_String(Widget_Image(Paned) & ".cargoframe");
      CargoCanvas.Interp := Interp;
      CargoCanvas.Name := New_String(Widget_Image(CargoFrame) & ".canvas");
      Label.Interp := Interp;
      Label.Name :=
        New_String(Widget_Image(CargoCanvas) & ".cargo.type.label");
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "cargo.tcl");
         Bind(CargoFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(Label, "ismapped") = "1" and Argc = 1 then
         Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp 1}");
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp 8}");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      CargoFrame.Name := New_String(Widget_Image(CargoCanvas) & ".cargo");
      configure
        (CargoCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (CargoCanvas, "window",
         "[expr " & Winfo_Get(CargoFrame, "reqwidth") & " / 2] [expr " &
         Winfo_Get(CargoFrame, "reqheight") & " / 2] -window " &
         Widget_Image(CargoFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (CargoCanvas, "-scrollregion [list " & BBox(CargoCanvas, "all") & "]");
      ShowScreen("cargoframe");
      return TCL_OK;
   end Show_Cargo_Info_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowCargoInfo", Show_Cargo_Info_Command'Access);
   end AddCommands;

end Ships.Cargo.UI;
