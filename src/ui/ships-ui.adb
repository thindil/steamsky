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

with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Maps.UI; use Maps.UI;
with Utils.UI; use Utils.UI;

package body Ships.UI is

   -- ****f* SUI2/Show_Ship_Info_Command
   -- FUNCTION
   -- Show information about the player's ship
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Ship_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Ship_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      Label: Ttk_Label;
      Paned: Ttk_PanedWindow;
      ShipInfoCanvas: Tk_Canvas;
      ShipInfoFrame: Ttk_Frame;
      NameEntry: Ttk_Entry;
   begin
      Paned.Interp := Interp;
      Paned.Name := New_String(".paned");
      ShipInfoFrame.Interp := Interp;
      ShipInfoFrame.Name := New_String(Widget_Image(Paned) & ".shipinfoframe");
      ShipInfoCanvas.Interp := Interp;
      ShipInfoCanvas.Name :=
        New_String(Widget_Image(ShipInfoFrame) & ".canvas");
      Label.Interp := Interp;
      Label.Name :=
        New_String(Widget_Image(ShipInfoCanvas) & ".shipinfo.left.info");
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "shipinfo.tcl");
         Bind(ShipInfoFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(Label, "ismapped") = "1" then
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      ShipInfoFrame.Name := New_String(Widget_Image(ShipInfoCanvas) & ".shipinfo");
      NameEntry.Interp := Get_Context;
      NameEntry.Name := New_String(Widget_Image(ShipInfoFrame) & ".left.name");
      Delete(NameEntry, "0", "end");
      Insert(NameEntry, "0", To_String(PlayerShip.Name));
      configure
        (ShipInfoCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (ShipInfoCanvas, "window",
         "[expr " & Winfo_Get(ShipInfoFrame, "reqwidth") & " / 2] [expr " &
         Winfo_Get(ShipInfoFrame, "reqheight") & " / 2] -window " &
         Widget_Image(ShipInfoFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (ShipInfoCanvas,
         "-scrollregion [list " & BBox(ShipInfoCanvas, "all") & "]");
      ShowScreen("shipinfoframe");
      return TCL_OK;
   end Show_Ship_Info_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowShipInfo", Show_Ship_Info_Command'Access);
   end AddCommands;

end Ships.UI;
