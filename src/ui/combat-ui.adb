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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Utils.UI; use Utils.UI;

package body Combat.UI is

   procedure ShowCombatUI is
      Label: Ttk_Label;
      Paned: Ttk_PanedWindow;
      CombatCanvas: Tk_Canvas;
      CombatFrame: Ttk_Frame;
   begin
      Paned.Interp := Get_Context;
      Paned.Name := New_String(".paned");
      CombatFrame.Interp := Get_Context;
      CombatFrame.Name := New_String(Widget_Image(Paned) & ".combatframe");
      CombatCanvas.Interp := Get_Context;
      CombatCanvas.Name := New_String(Widget_Image(CombatFrame) & ".canvas");
      Label.Interp := Get_Context;
      Label.Name :=
        New_String(Widget_Image(CombatCanvas) & ".combat.info.info.label");
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "combat.tcl");
         Bind(CombatFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
      end if;
      configure
        (CombatCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      CombatFrame.Name := New_String(Widget_Image(CombatCanvas) & ".combat");
      Canvas_Create
        (CombatCanvas, "window",
         "[expr " & Winfo_Get(CombatFrame, "reqwidth") & " / 2] [expr " &
         Winfo_Get(CombatFrame, "reqheight") & " / 2] -window " &
         Widget_Image(CombatFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (CombatCanvas,
         "-scrollregion [list " & BBox(CombatCanvas, "all") & "]");
      ShowScreen("combatframe");
   end ShowCombatUI;

end Combat.UI;
