-- Copyright (c) 2020-2024 Bartek thindil Jasicki
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
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with CoreUI;
with Crew; use Crew;
with Game;
with Maps.UI;
with Ships; use Ships;
with Ships.Movement;
with Utils.UI; use Utils.UI;

package body WaitMenu is

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
      use CoreUI;
      use Game;
      use Maps.UI;
      use Ships.Movement;

      Time_Needed: Natural := 0;
      Dialog_Close_Button: constant Ttk_Button :=
        Get_Widget(pathName => ".gameframe.wait.close", Interp => Interp);
      Amount_Box: constant Ttk_SpinBox :=
        Get_Widget(pathName => ".gameframe.wait.amount", Interp => Interp);
      Amount_Combo: constant Ttk_ComboBox :=
        Get_Widget(pathName => ".gameframe.wait.mins", Interp => Interp);
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
         if Time_Needed = 0 then
            return TCL_OK;
         end if;
         Update_Game(Minutes => Time_Needed);
         Wait_In_Place(Minutes => Time_Needed);
      elsif CArgv.Arg(Argv => Argv, N => 1) = "amount" then
         Time_Needed := Positive'Value(Get(Widgt => Amount_Box));
         if Current(ComboBox => Amount_Combo) = "1" then
            Time_Needed := Time_Needed * 60;
         end if;
         if Current(ComboBox => Amount_Combo) = "2" then
            Time_Needed := Time_Needed * 1_440;
         end if;
         Update_Game(Minutes => Time_Needed);
         Wait_In_Place(Minutes => Time_Needed);
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
      procedure Add_Ada_Commands with
         Import => True,
         Convention => C,
         External_Name => "addAdaWaitCommands";
   begin
      Add_Ada_Commands;
      Add_Command(Name => "Wait", Ada_Command => Wait_Command'Access);
   end Add_Commands;

end WaitMenu;
