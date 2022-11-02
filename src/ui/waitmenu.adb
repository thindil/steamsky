-- Copyright (c) 2020-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with CoreUI; use CoreUI;
with Crew; use Crew;
with Dialogs; use Dialogs;
with Game; use Game;
with Maps.UI; use Maps.UI;
with Ships; use Ships;
with Ships.Movement; use Ships.Movement;
with Utils.UI; use Utils.UI;

package body WaitMenu is

   function Show_Wait_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Wait_Dialog: Ttk_Frame :=
        Get_Widget(pathName => ".gameframe.wait", Interp => Interp);
      Button: Ttk_Button;
      Amount_Box: Ttk_SpinBox;
      Amount_Label: Ttk_Label;
      Need_Healing, Need_Rest: Boolean := False;
      procedure Add_Button(Time: Positive) is
      begin
         Button :=
           Create
             (pathName =>
                Wait_Dialog & ".wait" &
                Trim(Source => Positive'Image(Time), Side => Left),
              options =>
                "-text {Wait" & Positive'Image(Time) & " minute" &
                (if Time > 1 then "s" else "") & "} -command {Wait" &
                Positive'Image(Time) & "}");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button,
            Options =>
              "-sticky we -columnspan 3 -padx 5" &
              (if Time = 1 then " -pady {5 0}" else ""));
         Bind
           (Widgt => Button, Sequence => "<Escape>",
            Script => "{CloseDialog " & Wait_Dialog & ";break}");
         Add
           (Widget => Button,
            Message =>
              "Wait in place for" & Positive'Image(Time) & " minute" &
              (if Time > 1 then "s" else ""));
      end Add_Button;
   begin
      if Winfo_Get(Widgt => Wait_Dialog, Info => "exists") = "1" then
         Button := Get_Widget(pathName => Wait_Dialog & ".frame.close");
         if Invoke(Buttn => Button) /= "" then
            return TCL_ERROR;
         end if;
         return TCL_OK;
      end if;
      Wait_Dialog :=
        Create_Dialog
          (Name => ".gameframe.wait", Title => "Wait in place", Columns => 3);
      Add_Button(Time => 1);
      Add_Button(Time => 5);
      Add_Button(Time => 10);
      Add_Button(Time => 15);
      Add_Button(Time => 30);
      Button :=
        Create
          (pathName => Wait_Dialog & ".wait1h",
           options => "-text {Wait 1 hour} -command {Wait 60}");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Button, Options => "-sticky we -columnspan 3 -padx 5");
      Add(Widget => Button, Message => "Wait in place for 1 hour");
      Bind
        (Widgt => Button, Sequence => "<Escape>",
         Script => "{CloseDialog " & Wait_Dialog & ";break}");
      Button :=
        Create
          (pathName => Wait_Dialog & ".wait",
           options => "-text Wait -command {Wait amount}");
      Tcl.Tk.Ada.Grid.Grid(Slave => Button, Options => "-padx {5 0}");
      Bind
        (Widgt => Button, Sequence => "<Escape>",
         Script => "{CloseDialog " & Wait_Dialog & ";break}");
      Add
        (Widget => Button,
         Message =>
           "Wait in place for the selected amount of minutes:" & LF &
           "from 1 to 1440 (the whole day)");
      Amount_Box :=
        Create
          (pathName => Wait_Dialog & ".amount",
           options =>
             "-from 1 -to 1440 -width 6 -validate key -validatecommand {ValidateSpinbox %W %P " &
             Button & "}");
      Tcl.Tk.Ada.Grid.Grid(Slave => Amount_Box, Options => "-row 7 -column 1");
      Bind
        (Widgt => Amount_Box, Sequence => "<Escape>",
         Script => "{CloseDialog " & Wait_Dialog & ";break}");
      Set(SpinBox => Amount_Box, Value => "1");
      Add
        (Widget => Amount_Box,
         Message =>
           "Wait in place for the selected amount of minutes:" & LF &
           "from 1 to 1440 (the whole day)");
      Amount_Label :=
        Create
          (pathName => Wait_Dialog & ".mins",
           options => "-text minutes. -takefocus 0");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Amount_Label, Options => "-row 7 -column 2 -padx {0 5}");
      Check_Crew_Rest_Loop :
      for I in Player_Ship.Crew.First_Index .. Player_Ship.Crew.Last_Index loop
         if Player_Ship.Crew(I).Tired > 0 and
           Player_Ship.Crew(I).Order = REST then
            Need_Rest := True;
         end if;
         if Player_Ship.Crew(I).Health in 1 .. 99 and
           Player_Ship.Crew(I).Order = REST then
            Modules_Loop :
            for Module of Player_Ship.Modules loop
               if Module.M_Type = CABIN then
                  Owners_Loop :
                  for Owner of Module.Owner loop
                     if Owner = I then
                        Need_Healing := True;
                        exit Modules_Loop;
                     end if;
                  end loop Owners_Loop;
               end if;
            end loop Modules_Loop;
         end if;
      end loop Check_Crew_Rest_Loop;
      if Need_Rest then
         Button :=
           Create
             (pathName => Wait_Dialog & ".rest",
              options =>
                "-text {Wait until crew is rested} -command {Wait rest}");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button, Options => "-sticky we -columnspan 3 -padx 5");
         Bind
           (Widgt => Button, Sequence => "<Escape>",
            Script => "{CloseDialog " & Wait_Dialog & ";break}");
         Add
           (Widget => Button,
            Message => "Wait in place until the whole ship's crew is rested.");
      end if;
      if Need_Healing then
         Button :=
           Create
             (pathName => Wait_Dialog & ".heal",
              options =>
                "-text {Wait until crew is healed} -command {Wait heal}");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button, Options => "-sticky we -columnspan 3 -padx 5");
         Bind
           (Widgt => Button, Sequence => "<Escape>",
            Script => "{CloseDialog " & Wait_Dialog & ";break}");
         Add
           (Widget => Button,
            Message =>
              "Wait in place until the whole ship's crew is healed." & LF &
              "Can take a large amount of time.");
      end if;
      Button :=
        Create
          (pathName => Wait_Dialog & ".close",
           options =>
             "-text {Close} -command {CloseDialog " & Wait_Dialog & "}");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Button,
         Options => "-sticky we -columnspan 3 -padx 5 -pady {0 5}");
      Bind
        (Widgt => Button, Sequence => "<Escape>",
         Script => "{CloseDialog " & Wait_Dialog & ";break}");
      Add(Widget => Button, Message => "Close dialog \[Escape\]");
      Focus(Widgt => Button);
      Bind
        (Widgt => Button, Sequence => "<Tab>",
         Script => "{focus " & Wait_Dialog & ".wait1;break}");
      Show_Dialog(Dialog => Wait_Dialog, Relative_Y => 0.15);
      return TCL_OK;
   end Show_Wait_Command;

   -- ****o* WaitMenu/WaitMenu.Wait_Command
   -- FUNCTION
   -- Wait the selected amount of time
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Wait
   -- SOURCE
   function Wait_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Wait_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Time_Needed: Natural := 0;
      Dialog_Close_Button: constant Ttk_Button :=
        Get_Widget(pathName => ".gameframe.wait.close", Interp => Interp);
      Amount_Box: constant Ttk_SpinBox :=
        Get_Widget(pathName => ".gameframe.wait.amount", Interp => Interp);
      Current_Frame: Ttk_Frame :=
        Get_Widget
          (pathName => Main_Paned & ".shipinfoframe", Interp => Interp);
   begin
      if CArgv.Arg(Argv => Argv, N => 1) = "1" then
         Update_Game(Minutes => 1);
         Wait_In_Place(Minutes => 1);
      elsif CArgv.Arg(Argv => Argv, N => 1) = "5" then
         Update_Game(Minutes => 5);
         Wait_In_Place(Minutes => 5);
      elsif CArgv.Arg(Argv => Argv, N => 1) = "10" then
         Update_Game(Minutes => 10);
         Wait_In_Place(Minutes => 10);
      elsif CArgv.Arg(Argv => Argv, N => 1) = "15" then
         Update_Game(Minutes => 15);
         Wait_In_Place(Minutes => 15);
      elsif CArgv.Arg(Argv => Argv, N => 1) = "30" then
         Update_Game(Minutes => 30);
         Wait_In_Place(Minutes => 30);
      elsif CArgv.Arg(Argv => Argv, N => 1) = "60" then
         Update_Game(Minutes => 60);
         Wait_In_Place(Minutes => 60);
      elsif CArgv.Arg(Argv => Argv, N => 1) = "rest" then
         Wait_For_Rest;
      elsif CArgv.Arg(Argv => Argv, N => 1) = "heal" then
         Check_Crew_Heal_Loop :
         for I in Player_Ship.Crew.Iterate loop
            if Player_Ship.Crew(I).Health in 1 .. 99 and
              Player_Ship.Crew(I).Order = REST then
               Modules_Loop :
               for Module of Player_Ship.Modules loop
                  if Module.M_Type = CABIN then
                     Owners_Loop :
                     for Owner of Module.Owner loop
                        if Owner = Crew_Container.To_Index(Position => I) then
                           if Time_Needed <
                             (100 - Player_Ship.Crew(I).Health) * 15 then
                              Time_Needed :=
                                (100 - Player_Ship.Crew(I).Health) * 15;
                           end if;
                           exit Modules_Loop;
                        end if;
                     end loop Owners_Loop;
                  end if;
               end loop Modules_Loop;
            end if;
         end loop Check_Crew_Heal_Loop;
         if Time_Needed > 0 then
            Update_Game(Minutes => Time_Needed);
            Wait_In_Place(Minutes => Time_Needed);
         else
            return TCL_OK;
         end if;
      elsif CArgv.Arg(Argv => Argv, N => 1) = "amount" then
         Update_Game(Minutes => Positive'Value(Get(Widgt => Amount_Box)));
         Wait_In_Place(Minutes => Positive'Value(Get(Widgt => Amount_Box)));
      end if;
      Update_Header;
      Update_Messages;
      if Winfo_Get(Widgt => Current_Frame, Info => "exists") = "1"
        and then Winfo_Get(Widgt => Current_Frame, Info => "ismapped") =
          "1" then
         Tcl_Eval(interp => Interp, strng => "ShowShipInfo 1");
      else
         Current_Frame :=
           Get_Widget
             (pathName => Main_Paned & ".knowledgeframe", Interp => Interp);
         if Winfo_Get(Widgt => Current_Frame, Info => "exists") = "1"
           and then Winfo_Get(Widgt => Current_Frame, Info => "ismapped") =
             "1" then
            Tcl_Eval(interp => Interp, strng => "ShowKnowledge 1");
         else
            Draw_Map;
         end if;
      end if;
      if Invoke(Buttn => Dialog_Close_Button) /= "" then
         return TCL_ERROR;
      end if;
      return TCL_OK;
   end Wait_Command;

   procedure Add_Commands is
   begin
      Add_Command(Name => "ShowWait", Ada_Command => Show_Wait_Command'Access);
      Add_Command(Name => "Wait", Ada_Command => Wait_Command'Access);
   end Add_Commands;

end WaitMenu;
