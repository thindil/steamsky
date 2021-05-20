-- Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Tcl.Ada; use Tcl.Ada;
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
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Crew; use Crew;
with Game; use Game;
with Maps.UI; use Maps.UI;
with Ships; use Ships;
with Ships.Movement; use Ships.Movement;
with Utils.UI; use Utils.UI;

package body WaitMenu is

   function Show_Wait_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      WaitDialog: Ttk_Frame := Get_Widget(".gameframe.wait", Interp);
      Button: Ttk_Button;
      AmountBox: Ttk_SpinBox;
      AmountLabel: Ttk_Label;
      NeedHealing, NeedRest: Boolean := False;
      Frame: Ttk_Frame := Get_Widget(".gameframe.header");
      Dialog_Header: Ttk_Label;
   begin
      if Winfo_Get(WaitDialog, "exists") = "1" then
         Button := Get_Widget(WaitDialog & ".frame.close");
         if Invoke(Button) /= "" then
            return TCL_ERROR;
         end if;
         return TCL_OK;
      end if;
      Tcl.Tk.Ada.Busy.Busy(Frame);
      Frame := Get_Widget(".gameframe.paned");
      Tcl.Tk.Ada.Busy.Busy(Frame);
      WaitDialog := Create(".gameframe.wait", "-style Dialog.TFrame");
      Dialog_Header :=
        Create
          (WaitDialog & ".header",
           "-text {Wait in place} -wraplength 275 -style Header.TLabel");
      Tcl.Tk.Ada.Grid.Grid(Dialog_Header, "-sticky we -columnspan 3");
      Button :=
        Create
          (WaitDialog & ".wait1", "-text {Wait 1 minute} -command {Wait 1}");
      Tcl.Tk.Ada.Grid.Grid
        (Button, "-sticky we -columnspan 3 -padx 5 -pady {5 0}");
      Bind(Button, "<Escape>", "{CloseDialog " & WaitDialog & ";break}");
      Add(Button, "Wait in place for 1 minute");
      Button :=
        Create
          (WaitDialog & ".wait5", "-text {Wait 5 minutes} -command {Wait 5}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3 -padx 5");
      Add(Button, "Wait in place for 5 minutes");
      Button :=
        Create
          (WaitDialog & ".wait10",
           "-text {Wait 10 minutes} -command {Wait 10}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3 -padx 5");
      Bind(Button, "<Escape>", "{CloseDialog " & WaitDialog & ";break}");
      Button :=
        Create
          (WaitDialog & ".wait15",
           "-text {Wait 15 minutes} -command {Wait 15}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3 -padx 5");
      Add(Button, "Wait in place for 15 minutes");
      Button :=
        Create
          (WaitDialog & ".wait30",
           "-text {Wait 30 minutes} -command {Wait 30}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3 -padx 5");
      Bind(Button, "<Escape>", "{CloseDialog " & WaitDialog & ";break}");
      Add(Button, "Wait in place for 30 minutes");
      Button :=
        Create
          (WaitDialog & ".wait1h", "-text {Wait 1 hour} -command {Wait 60}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3 -padx 5");
      Add(Button, "Wait in place for 1 hour");
      Bind(Button, "<Escape>", "{CloseDialog " & WaitDialog & ";break}");
      Button :=
        Create(WaitDialog & ".wait", "-text Wait  -command {Wait amount}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-padx {5 0}");
      Bind(Button, "<Escape>", "{CloseDialog " & WaitDialog & ";break}");
      Add
        (Button,
         "Wait in place for the selected amount of minutes:" & LF &
         "from 1 to 1440 (the whole day)");
      AmountBox :=
        Create
          (WaitDialog & ".amount",
           "-from 1.0 -to 1440 -width 6 -validate key -validatecommand {ValidateSpinbox %W %P}");
      Tcl.Tk.Ada.Grid.Grid(AmountBox, "-row 6 -column 1");
      Bind(AmountBox, "<Escape>", "{CloseDialog " & WaitDialog & ";break}");
      Set(AmountBox, "1");
      Add
        (AmountBox,
         "Wait in place for the selected amount of minutes:" & LF &
         "from 1 to 1440 (the whole day)");
      AmountLabel :=
        Create(WaitDialog & ".mins", "-text minutes. -takefocus 0");
      Tcl.Tk.Ada.Grid.Grid(AmountLabel, "-row 6 -column 2 -padx {0 5}");
      Check_Crew_Rest_Loop :
      for I in PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
         if PlayerShip.Crew(I).Tired > 0 and
           PlayerShip.Crew(I).Order = Rest then
            NeedRest := True;
         end if;
         if PlayerShip.Crew(I).Health in 1 .. 99 and
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
      end loop Check_Crew_Rest_Loop;
      if NeedRest then
         Button :=
           Create
             (WaitDialog & ".rest",
              "-text {Wait until crew is rested} -command {Wait rest}");
         Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3 -padx 5");
         Bind(Button, "<Escape>", "{CloseDialog " & WaitDialog & ";break}");
         Add(Button, "Wait in place until the whole ship's crew is rested.");
      end if;
      if NeedHealing then
         Button :=
           Create
             (WaitDialog & ".heal",
              "-text {Wait until crew is healed} -command {Wait heal}");
         Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -columnspan 3 -padx 5");
         Bind(Button, "<Escape>", "{CloseDialog " & WaitDialog & ";break}");
         Add
           (Button,
            "Wait in place until the whole ship's crew is healed." & LF &
            "Can take a large amount of time.");
      end if;
      Button :=
        Create
          (WaitDialog & ".close",
           "-text {Close} -command {CloseDialog " & WaitDialog & "}");
      Tcl.Tk.Ada.Grid.Grid
        (Button, "-sticky we -columnspan 3 -padx 5 -pady {0 5}");
      Bind(Button, "<Escape>", "{CloseDialog " & WaitDialog & ";break}");
      Add(Button, "Close dialog \[Escape\]");
      Tcl.Tk.Ada.Place.Place
        (WaitDialog, "-in .gameframe -relx 0.3 -rely 0.15");
      Focus(Button);
      Bind(Button, "<Tab>", "{focus " & WaitDialog & ".wait1;break}");
      return TCL_OK;
   end Show_Wait_Command;

   -- ****o* WaitMenu/WaitMenu.Wait_Command
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Wait_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      TimeNeeded: Natural := 0;
      CloseButton: constant Ttk_Button :=
        Get_Widget(".gameframe.wait.close", Interp);
      AmountBox: constant Ttk_SpinBox :=
        Get_Widget(".gameframe.wait.amount", Interp);
      CurrentFrame: Ttk_Frame :=
        Get_Widget(".gameframe.paned.shipinfoframe", Interp);
   begin
      if CArgv.Arg(Argv, 1) = "1" then
         Update_Game(1);
         WaitInPlace(1);
      elsif CArgv.Arg(Argv, 1) = "5" then
         Update_Game(5);
         WaitInPlace(5);
      elsif CArgv.Arg(Argv, 1) = "10" then
         Update_Game(10);
         WaitInPlace(10);
      elsif CArgv.Arg(Argv, 1) = "15" then
         Update_Game(15);
         WaitInPlace(15);
      elsif CArgv.Arg(Argv, 1) = "30" then
         Update_Game(30);
         WaitInPlace(30);
      elsif CArgv.Arg(Argv, 1) = "60" then
         Update_Game(60);
         WaitInPlace(60);
      elsif CArgv.Arg(Argv, 1) = "rest" then
         WaitForRest;
      elsif CArgv.Arg(Argv, 1) = "heal" then
         Check_Crew_Heal_Loop :
         for I in PlayerShip.Crew.Iterate loop
            if PlayerShip.Crew(I).Health in 1 .. 99 and
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
         end loop Check_Crew_Heal_Loop;
         if TimeNeeded > 0 then
            Update_Game(TimeNeeded);
            WaitInPlace(TimeNeeded);
         else
            return TCL_OK;
         end if;
      elsif CArgv.Arg(Argv, 1) = "amount" then
         Update_Game(Positive'Value(Get(AmountBox)));
         WaitInPlace(Positive'Value(Get(AmountBox)));
      end if;
      UpdateHeader;
      UpdateMessages;
      if Winfo_Get(CurrentFrame, "exists") = "1"
        and then Winfo_Get(CurrentFrame, "ismapped") = "1" then
         Tcl_Eval(Interp, "ShowShipInfo 1");
      else
         CurrentFrame := Get_Widget(".gameframe.paned.knowledgeframe", Interp);
         if Winfo_Get(CurrentFrame, "exists") = "1"
           and then Winfo_Get(CurrentFrame, "ismapped") = "1" then
            Tcl_Eval(Interp, "ShowKnowledge 1");
         else
            DrawMap;
         end if;
      end if;
      if Invoke(CloseButton) /= "" then
         return TCL_ERROR;
      end if;
      return TCL_OK;
   end Wait_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowWait", Show_Wait_Command'Access);
      AddCommand("Wait", Wait_Command'Access);
   end AddCommands;

end WaitMenu;
