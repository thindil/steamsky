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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Event; use Tcl.Tk.Ada.Event;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases; use Bases;
with Config; use Config;
with CoreUI; use CoreUI;
with Factions; use Factions;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Missions; use Missions;
with Ships.UI.Crew;
with Ships.UI.Cargo;
with Ships.UI.Modules;
with Utils.UI; use Utils.UI;

package body Ships.UI is

   function Show_Ship_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argv);
      use Tiny_String;

      Ship_Info_Frame: Ttk_Frame :=
        Get_Widget
          (pathName => Main_Paned & ".shipinfoframe", Interp => Interp);
      Label: Ttk_Label;
      Upgrade_Info, Progress_Bar_Style: Unbounded_String;
      Max_Upgrade: Integer;
      Upgrade_Percent: Float;
      Upgrade_Progress: Ttk_ProgressBar;
      Cancel_Button: Ttk_Button;
      Ship_Canvas: constant Tk_Canvas :=
        Get_Widget(pathName => Ship_Info_Frame & ".general.canvas");
      Type_Box: constant Ttk_ComboBox :=
        Get_Widget
          (pathName =>
             Ship_Info_Frame & ".cargo.canvas.frame.selecttype.combo",
           Interp => Interp);
      Button: Ttk_Button :=
        Get_Widget
          (pathName =>
             Main_Paned & ".shipinfoframe.general.canvas.frame.rename");
   begin
      if Winfo_Get(Widgt => Ship_Info_Frame, Info => "exists") = "0" then
         Tcl_EvalFile
           (interp => Get_Context,
            fileName =>
              To_String(Source => Data_Directory) & "ui" & Dir_Separator &
              "shipinfo.tcl");
         configure(Widgt => Button, options => "-image editicon");
         Button :=
           Get_Widget
             (pathName =>
                Main_Paned & ".shipinfoframe.general.canvas.frame.showhome");
         configure(Widgt => Button, options => "-image showicon");
         Button :=
           Get_Widget
             (pathName =>
                Main_Paned &
                ".shipinfoframe.general.canvas.frame.cancelupgrade");
         configure(Widgt => Button, options => "-image cancelicon");
         Button :=
           Get_Widget
             (pathName =>
                Main_Paned &
                ".shipinfoframe.general.canvas.frame.cancelpriority");
         configure(Widgt => Button, options => "-image removeicon");
         Button :=
           Get_Widget
             (pathName =>
                Main_Paned &
                ".shipinfoframe.general.canvas.frame.canceldestination");
         configure(Widgt => Button, options => "-image removeicon");
      elsif Winfo_Get(Widgt => Ship_Info_Frame, Info => "ismapped") = "1" and
        Argc = 1 then
         Tcl_Eval(interp => Interp, strng => "InvokeButton " & Close_Button);
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
         Unbind_Accelerators_Loop :
         for Accel of General_Accelerators loop
            Unbind_From_Main_Window
              (Interp => Interp,
               Sequence => "<" & To_String(Source => Accel) & ">");
         end loop Unbind_Accelerators_Loop;
         return TCL_OK;
      end if;
      Bind_To_Main_Window
        (Interp => Interp,
         Sequence => "<" & To_String(Source => General_Accelerators(1)) & ">",
         Script => "{InvokeButton " & Ship_Canvas & ".frame.maxmin}");
      Bind_To_Main_Window
        (Interp => Interp,
         Sequence => "<" & To_String(Source => General_Accelerators(3)) & ">",
         Script =>
           "{InvokeButton " & Ship_Info_Frame &
           ".modules.canvas.frame.maxmin}");
      Bind_To_Main_Window
        (Interp => Interp,
         Sequence => "<" & To_String(Source => General_Accelerators(2)) & ">",
         Script =>
           "{InvokeButton " & Ship_Info_Frame & ".crew.canvas.frame.maxmin}");
      Bind_To_Main_Window
        (Interp => Interp, Sequence => "<" & To_String(Source => General_Accelerators(4)) & ">",
         Script => "{InvokeButton " & Ship_Info_Frame & ".cargo.canvas.frame.maxmin}");
      Tcl.Tk.Ada.Grid.Grid(Slave => Close_Button, Options => "-row 0 -column 1");
      Ship_Info_Frame.Name :=
        New_String(Str => Main_Paned & ".shipinfoframe.general.canvas.frame");
      Label := Get_Widget(pathName => Ship_Info_Frame & ".name");
      configure(Widgt => Label, options => "-text {Name: " & To_String(Source => Player_Ship.Name) & "}");
      Label.Name := New_String(Str => Ship_Info_Frame & ".upgradelabel");
      Upgrade_Progress := Get_Widget(pathName => Ship_Info_Frame & ".upgrade");
      Cancel_Button := Get_Widget(pathName => Ship_Info_Frame & ".cancelupgrade");
      -- Show or hide upgrade module info
      if Player_Ship.Upgrade_Module = 0 then
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Label);
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Upgrade_Progress);
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Cancel_Button);
      else
         Upgrade_Info :=
           To_Unbounded_String(Source => "Upgrade:") &
           To_String
             (Source => Player_Ship.Modules(Player_Ship.Upgrade_Module).Name) &
           " ";
         case Player_Ship.Modules(Player_Ship.Upgrade_Module).Upgrade_Action is
            when DURABILITY =>
               Append(Source => Upgrade_Info, New_Item => "(durability)");
               Max_Upgrade :=
                 BaseModules_Container.Element
                   (Container => Modules_List,
                    Index =>
                      Player_Ship.Modules(Player_Ship.Upgrade_Module)
                        .Proto_Index)
                   .Durability;
            when MAX_VALUE =>
               case BaseModules_Container.Element
                 (Container => Modules_List,
                  Index =>
                    Player_Ship.Modules(Player_Ship.Upgrade_Module)
                      .Proto_Index)
                 .M_Type is
                  when ENGINE =>
                     Append(Upgrade_Info, "(power)");
                     Max_Upgrade :=
                       BaseModules_Container.Element
                         (Container => Modules_List,
                          Index =>
                            Player_Ship.Modules(Player_Ship.Upgrade_Module)
                              .Proto_Index)
                         .Max_Value /
                       20;
                  when CABIN =>
                     Append(Upgrade_Info, "(quality)");
                     Max_Upgrade :=
                       BaseModules_Container.Element
                         (Container => Modules_List,
                          Index =>
                            Player_Ship.Modules(Player_Ship.Upgrade_Module)
                              .Proto_Index)
                         .Max_Value;
                  when GUN | BATTERING_RAM =>
                     Append(Upgrade_Info, "(damage)");
                     Max_Upgrade :=
                       BaseModules_Container.Element
                         (Container => Modules_List,
                          Index =>
                            Player_Ship.Modules(Player_Ship.Upgrade_Module)
                              .Proto_Index)
                         .Max_Value *
                       2;
                  when HULL =>
                     Append(Upgrade_Info, "(enlarge)");
                     Max_Upgrade :=
                       BaseModules_Container.Element
                         (Container => Modules_List,
                          Index =>
                            Player_Ship.Modules(Player_Ship.Upgrade_Module)
                              .Proto_Index)
                         .Max_Value *
                       40;
                  when HARPOON_GUN =>
                     Append(Upgrade_Info, "(strength)");
                     Max_Upgrade :=
                       BaseModules_Container.Element
                         (Container => Modules_List,
                          Index =>
                            Player_Ship.Modules(Player_Ship.Upgrade_Module)
                              .Proto_Index)
                         .Max_Value *
                       10;
                  when others =>
                     null;
               end case;
            when VALUE =>
               case BaseModules_Container.Element
                 (Container => Modules_List,
                  Index =>
                    Player_Ship.Modules(Player_Ship.Upgrade_Module)
                      .Proto_Index)
                 .M_Type is
                  when ENGINE =>
                     Append(Upgrade_Info, "(fuel usage)");
                     Max_Upgrade :=
                       BaseModules_Container.Element
                         (Container => Modules_List,
                          Index =>
                            Player_Ship.Modules(Player_Ship.Upgrade_Module)
                              .Proto_Index)
                         .Value *
                       20;
                  when others =>
                     null;
               end case;
            when others =>
               null;
         end case;
         Max_Upgrade :=
           Integer
             (Float(Max_Upgrade) *
              Float(New_Game_Settings.Upgrade_Cost_Bonus));
         if Max_Upgrade = 0 then
            Max_Upgrade := 1;
         end if;
         Upgrade_Percent :=
           1.0 -
           (Float
              (Player_Ship.Modules(Player_Ship.Upgrade_Module)
                 .Upgrade_Progress) /
            Float(Max_Upgrade));
         Progress_Bar_Style :=
           (if Upgrade_Percent > 0.74 then
              To_Unbounded_String(" -style green.Horizontal.TProgressbar")
            elsif Upgrade_Percent > 0.24 then
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar")
            else To_Unbounded_String(" -style Horizontal.TProgressbar"));
         configure
           (Upgrade_Progress,
            "-value" & Float'Image(Upgrade_Percent) &
            To_String(Progress_Bar_Style));
         configure(Label, "-text {" & To_String(Upgrade_Info) & "}");
         Tcl.Tk.Ada.Grid.Grid(Label);
         Tcl.Tk.Ada.Grid.Grid(Upgrade_Progress);
         Tcl.Tk.Ada.Grid.Grid(Cancel_Button);
      end if;
      -- Show or hide repair priority info
      Label.Name := New_String(Ship_Info_Frame & ".repairlabel");
      Cancel_Button.Name := New_String(Ship_Info_Frame & ".cancelpriority");
      if Player_Ship.Repair_Module = 0 then
         Tcl.Tk.Ada.Grid.Grid_Remove(Label);
         Tcl.Tk.Ada.Grid.Grid_Remove(Cancel_Button);
      else
         configure
           (Label,
            "-text {Repair first: " &
            To_String(Player_Ship.Modules(Player_Ship.Repair_Module).Name) &
            "}");
         Tcl.Tk.Ada.Grid.Grid(Label);
         Tcl.Tk.Ada.Grid.Grid(Cancel_Button);
      end if;
      -- Show or hide destination info
      Label.Name := New_String(Ship_Info_Frame & ".destinationlabel");
      Cancel_Button.Name := New_String(Ship_Info_Frame & ".canceldestination");
      if Player_Ship.Destination_X = 0 and Player_Ship.Destination_Y = 0 then
         Tcl.Tk.Ada.Grid.Grid_Remove(Label);
         Tcl.Tk.Ada.Grid.Grid_Remove(Cancel_Button);
      else
         if Sky_Map(Player_Ship.Destination_X, Player_Ship.Destination_Y)
             .Base_Index >
           0 then
            configure
              (Label,
               "-text {Destination: " &
               Tiny_String.To_String
                 (Sky_Bases
                    (Sky_Map
                       (Player_Ship.Destination_X, Player_Ship.Destination_Y)
                       .Base_Index)
                    .Name) &
               "}");
         else
            configure
              (Label,
               "-text {Destination: X:" &
               Positive'Image(Player_Ship.Destination_X) & " Y:" &
               Positive'Image(Player_Ship.Destination_Y) & "}");
         end if;
         Tcl.Tk.Ada.Grid.Grid(Label);
         Tcl.Tk.Ada.Grid.Grid(Cancel_Button);
      end if;
      Label.Name := New_String(Ship_Info_Frame & ".homelabel");
      configure
        (Label,
         "-text {Home: " & To_String(Sky_Bases(Player_Ship.Home_Base).Name) &
         "}");
      Label.Name := New_String(Ship_Info_Frame & ".weight");
      configure
        (Label,
         "-text {Weight:" & Integer'Image(Count_Ship_Weight(Player_Ship)) &
         "kg}");
      Tcl_Eval(Get_Context, "update");
      configure
        (Ship_Canvas, "-scrollregion [list " & BBox(Ship_Canvas, "all") & "]");
      Xview_Move_To(Ship_Canvas, "0.0");
      Yview_Move_To(Ship_Canvas, "0.0");
      -- Setting ship modules info
      Ships.UI.Modules.UpdateModulesInfo;
      -- Setting crew info
      Ships.UI.Crew.UpdateCrewInfo;
      -- Setting cargo info
      Set(Type_Box, "All");
      Generate(Type_Box, "<<ComboboxSelected>>");
      -- Show ship info
      Show_Screen("shipinfoframe");
      return TCL_OK;
   end Show_Ship_Info_Command;

   -- ****o* SUI2/SUI2.Set_Ship_Name_Command
   -- FUNCTION
   -- Change name of the player's ship
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetShipName shipname
   -- Shipname is the new name for the player's ship
   -- SOURCE
   function Set_Ship_Name_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Ship_Name_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData);
      NameEntry: constant Ttk_Label :=
        Get_Widget
          (Main_Paned & ".shipinfoframe.general.canvas.frame.name", Interp);
   begin
      if Argc = 1 then
         return TCL_OK;
      end if;
      Player_Ship.Name := Tiny_String.To_Bounded_String(CArgv.Arg(Argv, 1));
      configure(NameEntry, "-text {Name: " & CArgv.Arg(Argv, 1) & "}");
      return TCL_OK;
   end Set_Ship_Name_Command;

   -- ****o* SUI2/SUI2.Ship_Max_Min_Command
   -- FUNCTION
   -- Maximize or minimize the selected section of ship info
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShipMaxMin framename
   -- Framename is name of the frame to maximize or minimize
   -- SOURCE
   function Ship_Max_Min_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Ship_Max_Min_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      type Frame_Info is record
         Name: Unbounded_String;
         Column: Natural range 0 .. 1;
         Row: Natural range 0 .. 1;
      end record;
      Frames: constant array(1 .. 4) of Frame_Info :=
        ((To_Unbounded_String("general"), 0, 0),
         (To_Unbounded_String("modules"), 0, 1),
         (To_Unbounded_String("crew"), 1, 0),
         (To_Unbounded_String("cargo"), 1, 1));
      Frame: Ttk_Frame := Get_Widget(Main_Paned & ".shipinfoframe", Interp);
      Button: constant Ttk_Button :=
        Get_Widget
          (Frame & "." & CArgv.Arg(Argv, 1) & ".canvas.frame.maxmin", Interp);
   begin
      if CArgv.Arg(Argv, 2) /= "show" then
         Show_Frames_Loop :
         for FrameInfo of Frames loop
            Frame.Name :=
              New_String
                (Main_Paned & ".shipinfoframe." & To_String(FrameInfo.Name));
            if To_String(FrameInfo.Name) /= CArgv.Arg(Argv, 1) then
               Tcl.Tk.Ada.Grid.Grid(Frame);
            else
               Tcl.Tk.Ada.Grid.Grid_Configure
                 (Frame,
                  "-columnspan 1 -rowspan 1 -column" &
                  Natural'Image(FrameInfo.Column) & " -row" &
                  Natural'Image(FrameInfo.Row));
            end if;
         end loop Show_Frames_Loop;
         configure
           (Button,
            "-image movemapupicon -command {ShipMaxMin " & CArgv.Arg(Argv, 1) &
            " show}");
      else
         Hide_Frames_Loop :
         for FrameInfo of Frames loop
            Frame.Name :=
              New_String
                (Main_Paned & ".shipinfoframe." & To_String(FrameInfo.Name));
            if To_String(FrameInfo.Name) /= CArgv.Arg(Argv, 1) then
               Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
            else
               Tcl.Tk.Ada.Grid.Grid_Configure
                 (Frame, "-columnspan 2 -rowspan 2 -row 0 -column 0");
            end if;
         end loop Hide_Frames_Loop;
         configure
           (Button,
            "-image movemapdownicon -command {ShipMaxMin " &
            CArgv.Arg(Argv, 1) & " hide}");
      end if;
      return TCL_OK;
   end Ship_Max_Min_Command;

   procedure Add_Commands is
   begin
      Add_Command("ShowShipInfo", Show_Ship_Info_Command'Access);
      Add_Command("SetShipName", Set_Ship_Name_Command'Access);
      Add_Command("ShipMaxMin", Ship_Max_Min_Command'Access);
      Ships.UI.Modules.AddCommands;
      Ships.UI.Crew.AddCommands;
      Ships.UI.Cargo.AddCommands;
   end Add_Commands;

end Ships.UI;
