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
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
with Crew; use Crew;
with Game; use Game;
with Maps.UI; use Maps.UI;
with Ships; use Ships;
with Ships.Movement; use Ships.Movement;
with Utils.UI; use Utils.UI;

package body WaitMenu is

   SteamSky_Wait_Error: exception;

   function Show_Wait_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      WaitDialog: Tk_Toplevel := Get_Widget(".wait", Interp);
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Interp);
      WaitFrame: Ttk_Frame;
      Button: Ttk_Button;
      AmountBox: Ttk_SpinBox;
      AmountLabel: Ttk_Label;
      NeedHealing, NeedRest: Boolean := False;
      Width, Height: Positive;
      X, Y: Integer;
   begin
      if Winfo_Get(WaitDialog, "exists") = "1" then
         Button := Get_Widget(".wait.frame.close");
         if Invoke(Button) /= "" then
            raise SteamSky_Wait_Error with "Can't hide wait menu";
         end if;
         return TCL_OK;
      end if;
      WaitDialog := Create(".wait", "-class Dialog");
      WaitFrame := Create(".wait.frame");
      Tcl.Tk.Ada.Busy.Busy(MainWindow);
      Wm_Set(WaitDialog, "title", "{Steam Sky - Message}");
      Wm_Set(WaitDialog, "transient", ".");
      if Tcl_GetVar(Interp, "tcl_platform(os)") = "Linux" then
         Wm_Set(WaitDialog, "attributes", "-type dialog");
      end if;
      Tcl.Tk.Ada.Pack.Pack(WaitFrame, "-expand true -fill both");
      Button :=
        Create(".wait.frame.wait1", "-text {Wait 1 minute} -command {Wait 1}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3");
      Button :=
        Create
          (".wait.frame.wait5", "-text {Wait 5 minutes} -command {Wait 5}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3");
      Button :=
        Create
          (".wait.frame.wait10", "-text {Wait 10 minutes} -command {Wait 10}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3");
      Button :=
        Create
          (".wait.frame.wait15", "-text {Wait 15 minutes} -command {Wait 15}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3");
      Button :=
        Create
          (".wait.frame.wait30", "-text {Wait 30 minutes} -command {Wait 30}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3");
      Button :=
        Create(".wait.frame.wait1h", "-text {Wait 1 hour} -command {Wait 60}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3");
      Button :=
        Create(".wait.frame.wait", "-text Wait  -command {Wait amount}");
      Tcl.Tk.Ada.Grid.Grid(Button);
      AmountBox :=
        Create
          (".wait.frame.amount",
           "-from 1.0 -to 1440.0 -width 6 -validate key -validatecommand {ValidateSpinbox %S %s 1440}");
      Tcl.Tk.Ada.Grid.Grid(AmountBox, "-row 6 -column 1");
      Set(AmountBox, "1");
      AmountLabel := Create(".wait.frame.mins", "-text minutes.");
      Tcl.Tk.Ada.Grid.Grid(AmountLabel, "-row 6 -column 2");
      Width :=
        Positive'Value(Winfo_Get(Button, "reqwidth")) +
        Positive'Value(Winfo_Get(AmountBox, "reqwidth")) +
        Positive'Value(Winfo_Get(AmountLabel, "reqwidth"));
      Height := Positive'Value(Winfo_Get(Button, "reqheight")) * 8;
      for I in PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
         if PlayerShip.Crew(I).Tired > 0 and
           PlayerShip.Crew(I).Order = Rest then
            NeedRest := True;
         end if;
         if PlayerShip.Crew(I).Health < 100 and
           PlayerShip.Crew(I).Health > 0 and
           PlayerShip.Crew(I).Order = Rest then
            Modules_Loop :
            for Module of PlayerShip.Modules loop
               if Module.MType = CABIN then
                  for Owner of Module.Owner loop
                     if Owner = I then
                        NeedHealing := True;
                        exit Modules_Loop;
                     end if;
                  end loop;
               end if;
            end loop Modules_Loop;
         end if;
      end loop;
      if NeedRest then
         Button :=
           Create
             (".wait.frame.rest",
              "-text {Wait until crew is rested} -command {Wait rest}");
         Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3");
         Height := Height + Positive'Value(Winfo_Get(AmountBox, "reqheight"));
      end if;
      if NeedHealing then
         Button :=
           Create
             (".wait.frame.heal",
              "-text {Wait until crew is healed} -command {Wait heal}");
         Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3");
         Height := Height + Positive'Value(Winfo_Get(AmountBox, "reqheight"));
      end if;
      Button :=
        Create
          (".wait.frame.close", "-text {Close} -command {CloseDialog .wait}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3");
      X :=
        (Positive'Value(Winfo_Get(WaitDialog, "vrootwidth")) - Width) / 2;
      if X < 0 then
         X := 0;
      end if;
      Y :=
        (Positive'Value(Winfo_Get(WaitDialog, "vrootheight")) - Height) / 2;
      if Y < 0 then
         Y := 0;
      end if;
      Wm_Set
        (WaitDialog, "geometry",
         Trim(Positive'Image(Width), Left) & "x" &
         Trim(Positive'Image(Height), Left) & "+" &
         Trim(Positive'Image(X), Left) & "+" & Trim(Positive'Image(Y), Left));
      return TCL_OK;
   end Show_Wait_Command;

   -- ****o* WaitMenu/Wait_Command
   -- FUNCTION
   -- Wait the selected amount of time
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Wait
   -- SOURCE
   function Wait_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Wait_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      TimeNeeded: Natural := 0;
      CloseButton: constant Ttk_Button :=
        Get_Widget(".wait.frame.close", Interp);
      AmountBox: constant Ttk_SpinBox :=
        Get_Widget(".wait.frame.amount", Interp);
   begin
      if CArgv.Arg(Argv, 1) = "1" then
         UpdateGame(1);
         WaitInPlace(1);
      elsif CArgv.Arg(Argv, 1) = "5" then
         UpdateGame(5);
         WaitInPlace(5);
      elsif CArgv.Arg(Argv, 1) = "10" then
         UpdateGame(10);
         WaitInPlace(10);
      elsif CArgv.Arg(Argv, 1) = "15" then
         UpdateGame(15);
         WaitInPlace(15);
      elsif CArgv.Arg(Argv, 1) = "30" then
         UpdateGame(30);
         WaitInPlace(30);
      elsif CArgv.Arg(Argv, 1) = "60" then
         UpdateGame(60);
         WaitInPlace(60);
      elsif CArgv.Arg(Argv, 1) = "rest" then
         WaitForRest;
      elsif CArgv.Arg(Argv, 1) = "heal" then
         for I in PlayerShip.Crew.Iterate loop
            if PlayerShip.Crew(I).Health < 100 and
              PlayerShip.Crew(I).Health > 0 and
              PlayerShip.Crew(I).Order = Rest then
               Modules_Loop :
               for Module of PlayerShip.Modules loop
                  if Module.MType = CABIN then
                     for Owner of Module.Owner loop
                        if Owner = Crew_Container.To_Index(I) then
                           if TimeNeeded <
                             (100 - PlayerShip.Crew(I).Health) * 15 then
                              TimeNeeded :=
                                (100 - PlayerShip.Crew(I).Health) * 15;
                           end if;
                           exit Modules_Loop;
                        end if;
                     end loop;
                  end if;
               end loop Modules_Loop;
            end if;
         end loop;
         if TimeNeeded > 0 then
            UpdateGame(TimeNeeded);
            WaitInPlace(TimeNeeded);
         else
            return TCL_OK;
         end if;
      elsif CArgv.Arg(Argv, 1) = "amount" then
         UpdateGame(Positive'Value(Get(AmountBox)));
         WaitInPlace(Positive'Value(Get(AmountBox)));
      end if;
      UpdateHeader;
      DrawMap;
      if Invoke(CloseButton) /= "" then
         raise SteamSky_Wait_Error with "Can't hide wait menu";
      end if;
      return TCL_OK;
   end Wait_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowWait", Show_Wait_Command'Access);
      AddCommand("Wait", Wait_Command'Access);
   end AddCommands;

end WaitMenu;
