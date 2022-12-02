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
         configure(Widgt => Button, options => "-image cancelicon");
         Button :=
           Get_Widget
             (pathName =>
                Main_Paned &
                ".shipinfoframe.general.canvas.frame.canceldestination");
         configure(Widgt => Button, options => "-image cancelicon");
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
        (Interp => Interp,
         Sequence => "<" & To_String(Source => General_Accelerators(4)) & ">",
         Script =>
           "{InvokeButton " & Ship_Info_Frame & ".cargo.canvas.frame.maxmin}");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Close_Button, Options => "-row 0 -column 1");
      Ship_Info_Frame.Name :=
        New_String(Str => Main_Paned & ".shipinfoframe.general.canvas.frame");
      Label := Get_Widget(pathName => Ship_Info_Frame & ".name");
      configure
        (Widgt => Label,
         options =>
           "-text {Name: " & To_String(Source => Player_Ship.Name) & "}");
      Label.Name := New_String(Str => Ship_Info_Frame & ".upgradelabel");
      Upgrade_Progress := Get_Widget(pathName => Ship_Info_Frame & ".upgrade");
      Cancel_Button :=
        Get_Widget(pathName => Ship_Info_Frame & ".cancelupgrade");
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
                     Append(Source => Upgrade_Info, New_Item => "(power)");
                     Max_Upgrade :=
                       BaseModules_Container.Element
                         (Container => Modules_List,
                          Index =>
                            Player_Ship.Modules(Player_Ship.Upgrade_Module)
                              .Proto_Index)
                         .Max_Value /
                       20;
                  when CABIN =>
                     Append(Source => Upgrade_Info, New_Item => "(quality)");
                     Max_Upgrade :=
                       BaseModules_Container.Element
                         (Container => Modules_List,
                          Index =>
                            Player_Ship.Modules(Player_Ship.Upgrade_Module)
                              .Proto_Index)
                         .Max_Value;
                  when GUN | BATTERING_RAM =>
                     Append(Source => Upgrade_Info, New_Item => "(damage)");
                     Max_Upgrade :=
                       BaseModules_Container.Element
                         (Container => Modules_List,
                          Index =>
                            Player_Ship.Modules(Player_Ship.Upgrade_Module)
                              .Proto_Index)
                         .Max_Value *
                       2;
                  when HULL =>
                     Append(Source => Upgrade_Info, New_Item => "(enlarge)");
                     Max_Upgrade :=
                       BaseModules_Container.Element
                         (Container => Modules_List,
                          Index =>
                            Player_Ship.Modules(Player_Ship.Upgrade_Module)
                              .Proto_Index)
                         .Max_Value *
                       40;
                  when HARPOON_GUN =>
                     Append(Source => Upgrade_Info, New_Item => "(strength)");
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
                     Append
                       (Source => Upgrade_Info, New_Item => "(fuel usage)");
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
              To_Unbounded_String
                (Source => " -style green.Horizontal.TProgressbar")
            elsif Upgrade_Percent > 0.24 then
              To_Unbounded_String
                (Source => " -style yellow.Horizontal.TProgressbar")
            else To_Unbounded_String
                (Source => " -style Horizontal.TProgressbar"));
         configure
           (Widgt => Upgrade_Progress,
            options =>
              "-value" & Float'Image(Upgrade_Percent) &
              To_String(Source => Progress_Bar_Style));
         configure
           (Widgt => Label,
            options => "-text {" & To_String(Source => Upgrade_Info) & "}");
         Tcl.Tk.Ada.Grid.Grid(Slave => Label);
         Tcl.Tk.Ada.Grid.Grid(Slave => Upgrade_Progress);
         Tcl.Tk.Ada.Grid.Grid(Slave => Cancel_Button);
      end if;
      -- Show or hide repair priority info
      Label.Name := New_String(Str => Ship_Info_Frame & ".repairlabel");
      Cancel_Button.Name :=
        New_String(Str => Ship_Info_Frame & ".cancelpriority");
      if Player_Ship.Repair_Module = 0 then
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Label);
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Cancel_Button);
      else
         configure
           (Widgt => Label,
            options =>
              "-text {Repair first: " &
              To_String
                (Source =>
                   Player_Ship.Modules(Player_Ship.Repair_Module).Name) &
              "}");
         Tcl.Tk.Ada.Grid.Grid(Slave => Label);
         Tcl.Tk.Ada.Grid.Grid(Slave => Cancel_Button);
      end if;
      -- Show or hide destination info
      Label.Name := New_String(Str => Ship_Info_Frame & ".destinationlabel");
      Cancel_Button.Name :=
        New_String(Str => Ship_Info_Frame & ".canceldestination");
      if Player_Ship.Destination_X = 0 and Player_Ship.Destination_Y = 0 then
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Label);
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Cancel_Button);
      else
         if Sky_Map(Player_Ship.Destination_X, Player_Ship.Destination_Y)
             .Base_Index >
           0 then
            configure
              (Widgt => Label,
               options =>
                 "-text {Destination: " &
                 Tiny_String.To_String
                   (Source =>
                      Sky_Bases
                        (Sky_Map
                           (Player_Ship.Destination_X,
                            Player_Ship.Destination_Y)
                           .Base_Index)
                        .Name) &
                 "}");
         else
            configure
              (Widgt => Label,
               options =>
                 "-text {Destination: X:" &
                 Positive'Image(Player_Ship.Destination_X) & " Y:" &
                 Positive'Image(Player_Ship.Destination_Y) & "}");
         end if;
         Tcl.Tk.Ada.Grid.Grid(Slave => Label);
         Tcl.Tk.Ada.Grid.Grid(Slave => Cancel_Button);
      end if;
      Label.Name := New_String(Str => Ship_Info_Frame & ".homelabel");
      configure
        (Widgt => Label,
         options =>
           "-text {Home: " &
           To_String(Source => Sky_Bases(Player_Ship.Home_Base).Name) & "}");
      Label.Name := New_String(Str => Ship_Info_Frame & ".weight");
      configure
        (Widgt => Label,
         options =>
           "-text {Weight:" &
           Integer'Image(Count_Ship_Weight(Ship => Player_Ship)) & "kg}");
      Tcl_Eval(interp => Get_Context, strng => "update");
      configure
        (Widgt => Ship_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Ship_Canvas, TagOrId => "all") & "]");
      Xview_Move_To(CanvasWidget => Ship_Canvas, Fraction => "0.0");
      Yview_Move_To(CanvasWidget => Ship_Canvas, Fraction => "0.0");
      -- Setting ship modules info
      Ships.UI.Modules.Update_Modules_Info;
      -- Setting crew info
      Ships.UI.Crew.Update_Crew_Info;
      -- Setting cargo info
      Set(ComboBox => Type_Box, Value => "All");
      Generate(Window => Type_Box, EventName => "<<ComboboxSelected>>");
      -- Show ship info
      Show_Screen(New_Screen_Name => "shipinfoframe");
      return TCL_OK;
   end Show_Ship_Info_Command;

   -- ****o* SUI2/SUI2.Set_Ship_Name_Command
   -- FUNCTION
   -- Change name of the player's ship
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetShipName shipname
   -- Shipname is the new name for the player's ship
   -- SOURCE
   function Set_Ship_Name_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Ship_Name_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data);
      Name_Entry: constant Ttk_Label :=
        Get_Widget
          (pathName => Main_Paned & ".shipinfoframe.general.canvas.frame.name",
           Interp => Interp);
   begin
      if Argc = 1 then
         return TCL_OK;
      end if;
      Player_Ship.Name :=
        Tiny_String.To_Bounded_String
          (Source => CArgv.Arg(Argv => Argv, N => 1));
      configure
        (Widgt => Name_Entry,
         options => "-text {Name: " & CArgv.Arg(Argv => Argv, N => 1) & "}");
      return TCL_OK;
   end Set_Ship_Name_Command;

   -- ****o* SUI2/SUI2.Ship_Max_Min_Command
   -- FUNCTION
   -- Maximize or minimize the selected section of ship info
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShipMaxMin framename
   -- Framename is name of the frame to maximize or minimize
   -- SOURCE
   function Ship_Max_Min_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Ship_Max_Min_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      type Frame_Info is record
         Name: Unbounded_String;
         Column: Natural range 0 .. 1;
         Row: Natural range 0 .. 1;
      end record;
      Frames: constant array(1 .. 4) of Frame_Info :=
        (1 =>
           (Name => To_Unbounded_String(Source => "general"), Column => 0,
            Row => 0),
         2 =>
           (Name => To_Unbounded_String(Source => "modules"), Column => 0,
            Row => 1),
         3 =>
           (Name => To_Unbounded_String(Source => "crew"), Column => 1,
            Row => 0),
         4 =>
           (Name => To_Unbounded_String(Source => "cargo"), Column => 1,
            Row => 1));
      Frame: Ttk_Frame :=
        Get_Widget
          (pathName => Main_Paned & ".shipinfoframe", Interp => Interp);
      Button: constant Ttk_Button :=
        Get_Widget
          (pathName =>
             Frame & "." & CArgv.Arg(Argv => Argv, N => 1) &
             ".canvas.frame.maxmin",
           Interp => Interp);
   begin
      if CArgv.Arg(Argv => Argv, N => 2) /= "show" then
         Show_Frames_Loop :
         for FrameInfo of Frames loop
            Frame.Name :=
              New_String
                (Str =>
                   Main_Paned & ".shipinfoframe." &
                   To_String(Source => FrameInfo.Name));
            if To_String(Source => FrameInfo.Name) /=
              CArgv.Arg(Argv => Argv, N => 1) then
               Tcl.Tk.Ada.Grid.Grid(Slave => Frame);
            else
               Tcl.Tk.Ada.Grid.Grid_Configure
                 (Slave => Frame,
                  Options =>
                    "-columnspan 1 -rowspan 1 -column" &
                    Natural'Image(FrameInfo.Column) & " -row" &
                    Natural'Image(FrameInfo.Row));
            end if;
         end loop Show_Frames_Loop;
         configure
           (Widgt => Button,
            options =>
              "-image movemapupicon -command {ShipMaxMin " &
              CArgv.Arg(Argv => Argv, N => 1) & " show}");
      else
         Hide_Frames_Loop :
         for FrameInfo of Frames loop
            Frame.Name :=
              New_String
                (Str =>
                   Main_Paned & ".shipinfoframe." &
                   To_String(Source => FrameInfo.Name));
            if To_String(Source => FrameInfo.Name) /=
              CArgv.Arg(Argv => Argv, N => 1) then
               Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Frame);
            else
               Tcl.Tk.Ada.Grid.Grid_Configure
                 (Slave => Frame,
                  Options => "-columnspan 2 -rowspan 2 -row 0 -column 0");
            end if;
         end loop Hide_Frames_Loop;
         configure
           (Widgt => Button,
            options =>
              "-image movemapdownicon -command {ShipMaxMin " &
              CArgv.Arg(Argv => Argv, N => 1) & " hide}");
      end if;
      return TCL_OK;
   end Ship_Max_Min_Command;

   procedure Add_Commands is
   begin
      Add_Command
        (Name => "ShowShipInfo", Ada_Command => Show_Ship_Info_Command'Access);
      Add_Command
        (Name => "SetShipName", Ada_Command => Set_Ship_Name_Command'Access);
      Add_Command
        (Name => "ShipMaxMin", Ada_Command => Ship_Max_Min_Command'Access);
      Ships.UI.Modules.Add_Commands;
      Ships.UI.Crew.Add_Commands;
      Ships.UI.Cargo.Add_Commands;
   end Add_Commands;

end Ships.UI;
