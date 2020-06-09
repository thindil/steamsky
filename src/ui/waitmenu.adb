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

with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox; use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
with Utils.UI; use Utils.UI;

package body WaitMenu is

   function Show_Wait_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      MessageDialog: constant Tk_Toplevel :=
        Create(".wait", "-class Dialog");
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Interp);
      WaitFrame: constant Ttk_Frame := Create(".wait.frame");
      Button: Ttk_Button;
      AmountBox: constant Ttk_Spinbox := Create(".wait.frame.amount", "-from 1.0 -to 1440.0 -width 6");
      AmountLabel: constant Ttk_Label := Create(".wait.frame.mins", "-text minutes.");
   begin
      Tcl.Tk.Ada.Busy.Busy(MainWindow);
      Wm_Set(MessageDialog, "title", "{Steam Sky - Message}");
      Wm_Set(MessageDialog, "transient", ".");
      if Tcl_GetVar(Interp, "tcl_platform(os)") = "Linux" then
         Wm_Set(MessageDialog, "attributes", "-type dialog");
      end if;
      Tcl.Tk.Ada.Pack.Pack(WaitFrame, "-expand true -fill both");
      Button := Create(".wait.frame.wait1", "-text {Wait 1 minute}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3");
      Button := Create(".wait.frame.wait5", "-text {Wait 5 minutes}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3");
      Button := Create(".wait.frame.wait10", "-text {Wait 10 minutes}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3");
      Button := Create(".wait.frame.wait15", "-text {Wait 15 minutes}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3");
      Button := Create(".wait.frame.wait30", "-text {Wait 30 minutes}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3");
      Button := Create(".wait.frame.wait1h", "-text {Wait 1 hour}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3");
      Button := Create(".wait.frame.wait", "-text Wait");
      Tcl.Tk.Ada.Grid.Grid(Button);
      Tcl.Tk.Ada.Grid.Grid(AmountBox, "-row 6 -column 1");
      Set(AmountBox, "1");
      Tcl.Tk.Ada.Grid.Grid(AmountLabel, "-row 6 -column 2");
      return TCL_OK;
   end Show_Wait_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowWait", Show_Wait_Command'Access);
   end AddCommands;

end WaitMenu;
