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

with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Place;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
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
      WaitDialog: Ttk_Frame := Get_Widget(".gameframe.wait", Interp);
      Button: Ttk_Button;
      AmountBox: Ttk_SpinBox;
      AmountLabel: Ttk_Label;
      NeedHealing, NeedRest: Boolean := False;
      Frame: Ttk_Frame := Get_Widget(".gameframe.header");
   begin
      if Winfo_Get(WaitDialog, "exists") = "1" then
         Button := Get_Widget(WaitDialog & ".frame.close");
         if Invoke(Button) /= "" then
            raise SteamSky_Wait_Error with "Can't hide wait menu";
         end if;
         return TCL_OK;
      end if;
      Tcl.Tk.Ada.Busy.Busy(Frame);
      Frame := Get_Widget(".gameframe.paned");
      Tcl.Tk.Ada.Busy.Busy(Frame);
      WaitDialog := Create(".gameframe.wait", "-style Dialog.TFrame");
      Button :=
        Create
          (WaitDialog & ".wait1", "-text {Wait 1 minute} -command {Wait 1}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3");
      Button :=
        Create
          (WaitDialog & ".wait5", "-text {Wait 5 minutes} -command {Wait 5}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3");
      Button :=
        Create
          (WaitDialog & ".wait10",
           "-text {Wait 10 minutes} -command {Wait 10}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3");
      Button :=
        Create
          (WaitDialog & ".wait15",
           "-text {Wait 15 minutes} -command {Wait 15}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3");
      Button :=
        Create
          (WaitDialog & ".wait30",
           "-text {Wait 30 minutes} -command {Wait 30}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3");
      Button :=
        Create
          (WaitDialog & ".wait1h", "-text {Wait 1 hour} -command {Wait 60}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3");
      Button :=
        Create(WaitDialog & ".wait", "-text Wait  -command {Wait amount}");
      Tcl.Tk.Ada.Grid.Grid(Button);
      AmountBox :=
        Create
          (WaitDialog & ".amount",
           "-from 1.0 -to 1440.0 -width 6 -validate key -validatecommand {ValidateSpinbox %S %s 1440}");
      Tcl.Tk.Ada.Grid.Grid(AmountBox, "-row 6 -column 1");
      Set(AmountBox, "1");
      AmountLabel := Create(WaitDialog & ".mins", "-text minutes.");
      Tcl.Tk.Ada.Grid.Grid(AmountLabel, "-row 6 -column 2");
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
             (WaitDialog & ".rest",
              "-text {Wait until crew is rested} -command {Wait rest}");
         Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3");
      end if;
      if NeedHealing then
         Button :=
           Create
             (WaitDialog & ".heal",
              "-text {Wait until crew is healed} -command {Wait heal}");
         Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3");
      end if;
      Button :=
        Create
          (WaitDialog & ".close",
           "-text {Close} -command {CloseDialog " & WaitDialog & "}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3");
      Focus(Button);
      Tcl.Tk.Ada.Place.Place(WaitDialog, "-in .gameframe -relx 0.3 -rely 0.3");
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
        Get_Widget(".gameframe.wait.close", Interp);
      AmountBox: constant Ttk_SpinBox :=
        Get_Widget(".gameframe.wait.amount", Interp);
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
