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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.UTF_Encoding.Wide_Strings;
use Ada.Strings.UTF_Encoding.Wide_Strings;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkMenuButton; use Tcl.Tk.Ada.Widgets.TtkMenuButton;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Bases; use Bases;
with Config; use Config;
with Factions; use Factions;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Missions; use Missions;
with ShipModules; use ShipModules;
with Ships.UI.Modules;
with Utils.UI; use Utils.UI;
with Themes; use Themes;

package body Ships.UI is

   function Show_Ship_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argv);
      Label: Ttk_Label;
      Paned: Ttk_PanedWindow;
      ShipInfoFrame, Item: Ttk_Frame;
      UpgradeInfo, ProgressBarStyle: Unbounded_String;
      MaxUpgrade: Integer;
      UpgradePercent: Float;
      UpgradeProgress: Ttk_ProgressBar;
      CloseButton, CancelButton: Ttk_Button;
      Tokens: Slice_Set;
      Rows, Row: Natural := 0;
      ShipCanvas: Tk_Canvas;
      Button: Ttk_MenuButton;
   begin
      Paned.Interp := Interp;
      Paned.Name := New_String(".paned");
      CloseButton.Interp := Interp;
      CloseButton.Name := New_String(".header.closebutton");
      ShipInfoFrame.Interp := Interp;
      ShipInfoFrame.Name := New_String(Widget_Image(Paned) & ".shipinfoframe");
      if Winfo_Get(ShipInfoFrame, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "shipinfo.tcl");
      elsif Winfo_Get(ShipInfoFrame, "ismapped") = "1" and Argc = 1 then
         Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp repair}");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      ShipInfoFrame.Name :=
        New_String
          (Widget_Image(Paned) & ".shipinfoframe.general.canvas.frame");
      Label.Interp := Interp;
      Label.Name := New_String(Widget_Image(ShipInfoFrame) & ".name");
      configure(Label, "-text {Name: " & To_String(PlayerShip.Name) & "}");
      Label.Name := New_String(Widget_Image(ShipInfoFrame) & ".upgradelabel");
      UpgradeProgress.Interp := Interp;
      UpgradeProgress.Name :=
        New_String(Widget_Image(ShipInfoFrame) & ".upgrade");
      CancelButton.Interp := Interp;
      CancelButton.Name :=
        New_String(Widget_Image(ShipInfoFrame) & ".cancelupgrade");
      -- Show or hide upgrade module info
      if PlayerShip.UpgradeModule = 0 then
         Tcl.Tk.Ada.Grid.Grid_Remove(Label);
         Tcl.Tk.Ada.Grid.Grid_Remove(UpgradeProgress);
         Tcl.Tk.Ada.Grid.Grid_Remove(CancelButton);
      else
         UpgradeInfo :=
           "Upgrade:" & PlayerShip.Modules(PlayerShip.UpgradeModule).Name &
           " ";
         case PlayerShip.Modules(PlayerShip.UpgradeModule).UpgradeAction is
            when DURABILITY =>
               Append(UpgradeInfo, "(durability)");
               MaxUpgrade :=
                 Modules_List
                   (PlayerShip.Modules(PlayerShip.UpgradeModule).ProtoIndex)
                   .Durability;
            when MAX_VALUE =>
               case Modules_List
                 (PlayerShip.Modules(PlayerShip.UpgradeModule).ProtoIndex)
                 .MType is
                  when ENGINE =>
                     Append(UpgradeInfo, "(power)");
                     MaxUpgrade :=
                       Modules_List
                         (PlayerShip.Modules(PlayerShip.UpgradeModule)
                            .ProtoIndex)
                         .MaxValue /
                       20;
                  when CABIN =>
                     Append(UpgradeInfo, "(quality)");
                     MaxUpgrade :=
                       Modules_List
                         (PlayerShip.Modules(PlayerShip.UpgradeModule)
                            .ProtoIndex)
                         .MaxValue;
                  when GUN | BATTERING_RAM =>
                     Append(UpgradeInfo, "(damage)");
                     MaxUpgrade :=
                       Modules_List
                         (PlayerShip.Modules(PlayerShip.UpgradeModule)
                            .ProtoIndex)
                         .MaxValue *
                       2;
                  when HULL =>
                     Append(UpgradeInfo, "(enlarge)");
                     MaxUpgrade :=
                       Modules_List
                         (PlayerShip.Modules(PlayerShip.UpgradeModule)
                            .ProtoIndex)
                         .MaxValue *
                       40;
                  when HARPOON_GUN =>
                     Append(UpgradeInfo, "(strength)");
                     MaxUpgrade :=
                       Modules_List
                         (PlayerShip.Modules(PlayerShip.UpgradeModule)
                            .ProtoIndex)
                         .MaxValue *
                       10;
                  when others =>
                     null;
               end case;
            when VALUE =>
               case Modules_List
                 (PlayerShip.Modules(PlayerShip.UpgradeModule).ProtoIndex)
                 .MType is
                  when ENGINE =>
                     Append(UpgradeInfo, "(fuel usage)");
                     MaxUpgrade :=
                       Modules_List
                         (PlayerShip.Modules(PlayerShip.UpgradeModule)
                            .ProtoIndex)
                         .Value *
                       20;
                  when others =>
                     null;
               end case;
            when others =>
               null;
         end case;
         MaxUpgrade :=
           Integer(Float(MaxUpgrade) * NewGameSettings.UpgradeCostBonus);
         if MaxUpgrade = 0 then
            MaxUpgrade := 1;
         end if;
         UpgradePercent :=
           1.0 -
           (Float
              (PlayerShip.Modules(PlayerShip.UpgradeModule).UpgradeProgress) /
            Float(MaxUpgrade));
         if UpgradePercent > 0.74 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style green.Horizontal.TProgressbar");
         elsif UpgradePercent > 0.24 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar");
         else
            ProgressBarStyle :=
              To_Unbounded_String(" -style Horizontal.TProgressbar");
         end if;
         configure
           (UpgradeProgress,
            "-value" & Float'Image(UpgradePercent) &
            To_String(ProgressBarStyle));
         configure(Label, "-text {" & To_String(UpgradeInfo) & "}");
         Tcl.Tk.Ada.Grid.Grid(Label);
         Tcl.Tk.Ada.Grid.Grid(UpgradeProgress);
         Tcl.Tk.Ada.Grid.Grid(CancelButton);
      end if;
      -- Show or hide repair priority info
      Label.Name := New_String(Widget_Image(ShipInfoFrame) & ".repairlabel");
      CancelButton.Name :=
        New_String(Widget_Image(ShipInfoFrame) & ".cancelpriority");
      if PlayerShip.RepairModule = 0 then
         Tcl.Tk.Ada.Grid.Grid_Remove(Label);
         Tcl.Tk.Ada.Grid.Grid_Remove(CancelButton);
      else
         configure
           (Label,
            "-text {Repair first: " &
            To_String(PlayerShip.Modules(PlayerShip.RepairModule).Name) & "}");
         Tcl.Tk.Ada.Grid.Grid(Label);
         Tcl.Tk.Ada.Grid.Grid(CancelButton);
      end if;
      -- Show or hide destination info
      Label.Name :=
        New_String(Widget_Image(ShipInfoFrame) & ".destinationlabel");
      CancelButton.Name :=
        New_String(Widget_Image(ShipInfoFrame) & ".canceldestination");
      if PlayerShip.DestinationX = 0 and PlayerShip.DestinationY = 0 then
         Tcl.Tk.Ada.Grid.Grid_Remove(Label);
         Tcl.Tk.Ada.Grid.Grid_Remove(CancelButton);
      else
         if SkyMap(PlayerShip.DestinationX, PlayerShip.DestinationY)
             .BaseIndex >
           0 then
            configure
              (Label,
               "-text {Destination: " &
               To_String
                 (SkyBases
                    (SkyMap(PlayerShip.DestinationX, PlayerShip.DestinationY)
                       .BaseIndex)
                    .Name) &
               "}");
         else
            configure
              (Label,
               "-text {Destination: X:" &
               Positive'Image(PlayerShip.DestinationX) & " Y:" &
               Positive'Image(PlayerShip.DestinationY) & "}");
         end if;
         Tcl.Tk.Ada.Grid.Grid(Label);
         Tcl.Tk.Ada.Grid.Grid(CancelButton);
      end if;
      Label.Name := New_String(Widget_Image(ShipInfoFrame) & ".homelabel");
      configure
        (Label,
         "-text {Home: " & To_String(SkyBases(PlayerShip.HomeBase).Name) &
         "}");
      Label.Name := New_String(Widget_Image(ShipInfoFrame) & ".weight");
      configure
        (Label,
         "-text {Weight:" & Integer'Image(CountShipWeight(PlayerShip)) &
         "kg}");
      Tcl_Eval(Get_Context, "update");
      ShipCanvas.Interp := Interp;
      ShipCanvas.Name :=
        New_String(Widget_Image(Paned) & ".shipinfoframe.general.canvas");
      configure
        (ShipCanvas, "-scrollregion [list " & BBox(ShipCanvas, "all") & "]");
      Xview_Move_To(ShipCanvas, "0.0");
      Yview_Move_To(ShipCanvas, "0.0");
      -- Setting ship modules info
      ShipInfoFrame.Name := New_String(Widget_Image(Paned) & ".shipinfoframe");
      ShipInfoFrame.Name :=
        New_String(Widget_Image(ShipInfoFrame) & ".modules.canvas.frame");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(ShipInfoFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      for I in 2 .. (Rows - 1) loop
         Create
           (Tokens,
            Tcl.Tk.Ada.Grid.Grid_Slaves
              (ShipInfoFrame, "-row" & Positive'Image(I)),
            " ");
         for J in 1 .. Slice_Count(Tokens) loop
            Item.Interp := Interp;
            Item.Name := New_String(Slice(Tokens, J));
            Destroy(Item);
         end loop;
      end loop;
      Row := 2;
      for Module of PlayerShip.Modules loop
         Ships.UI.Modules.ShowModuleOptions(Row - 1);
         Button :=
           Create
             (Widget_Image(ShipInfoFrame) & ".name" &
              Trim(Natural'Image(Row), Left),
              "-text {" & To_String(Module.Name) & "} -menu .modulemenu" &
              Trim(Positive'Image(Row - 1), Left));
         Add(Button, "Show available module's options");
         Tcl.Tk.Ada.Grid.Grid
           (Button, "-row" & Natural'Image(Row) & " -sticky w");
         UpgradePercent :=
           (Float(Module.Durability) / Float(Module.MaxDurability));
         if UpgradePercent = 1.0 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style green.Horizontal.TProgressbar");
         elsif UpgradePercent > 0.24 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar");
         else
            ProgressBarStyle :=
              To_Unbounded_String(" -style Horizontal.TProgressbar");
         end if;
         UpgradeProgress :=
           Create
             (Widget_Image(ShipInfoFrame) & ".durability" &
              Trim(Natural'Image(Row), Left),
              "-value {" & Float'Image(UpgradePercent) &
              "} -maximum 1.0 -length 150" & To_String(ProgressBarStyle));
         Add
           (UpgradeProgress, "The current durability of the selected module.");
         Tcl.Tk.Ada.Grid.Grid
           (UpgradeProgress, "-row" & Natural'Image(Row) & " -column 1");
         Row := Row + 1;
      end loop;
      Tcl_Eval(Get_Context, "update");
      ShipCanvas.Interp := Interp;
      ShipCanvas.Name :=
        New_String(Widget_Image(Paned) & ".shipinfoframe.modules.canvas");
      configure
        (ShipCanvas, "-scrollregion [list " & BBox(ShipCanvas, "all") & "]");
      Xview_Move_To(ShipCanvas, "0.0");
      Yview_Move_To(ShipCanvas, "0.0");
      -- Setting crew info
      UpdateCrewInfo;
      -- Show ship info
      ShowScreen("shipinfoframe");
      return TCL_OK;
   end Show_Ship_Info_Command;

   -- ****o* SUI2/Set_Ship_Name_Command
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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Ship_Name_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData);
      NameEntry: Ttk_Label;
   begin
      if Argc = 1 then
         return TCL_OK;
      end if;
      NameEntry.Interp := Interp;
      NameEntry.Name :=
        New_String(".paned.shipinfoframe.general.canvas.frame.name");
      PlayerShip.Name := To_Unbounded_String(CArgv.Arg(Argv, 1));
      configure(NameEntry, "-text {Name: " & CArgv.Arg(Argv, 1) & "}");
      return TCL_OK;
   end Set_Ship_Name_Command;

   -- ****o* SUI2/Ship_Max_Min_Command
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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Ship_Max_Min_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      type Frame_Info is record
         Name: Unbounded_String;
         Column: Natural range 0 .. 1;
         Row: Natural range 0 .. 1;
      end record;
      Frames: constant array(1 .. 3) of Frame_Info :=
        ((To_Unbounded_String("general"), 0, 0),
         (To_Unbounded_String("modules"), 0, 1),
         (To_Unbounded_String("crew"), 1, 0));
      Frame: Ttk_Frame;
      Button: Ttk_Button;
   begin
      Frame.Interp := Interp;
      Frame.Name := New_String(".paned.shipinfoframe");
      Button.Interp := Interp;
      Button.Name :=
        New_String
          (Widget_Image(Frame) & "." & CArgv.Arg(Argv, 1) &
           ".canvas.frame.maxmin");
      if CArgv.Arg(Argv, 2) /= "show" then
         for FrameInfo of Frames loop
            Frame.Name :=
              New_String(".paned.shipinfoframe." & To_String(FrameInfo.Name));
            if To_String(FrameInfo.Name) /= CArgv.Arg(Argv, 1) then
               Tcl.Tk.Ada.Grid.Grid(Frame);
            else
               Tcl.Tk.Ada.Grid.Grid_Configure
                 (Frame,
                  "-columnspan 1 -rowspan 1 -column" &
                  Natural'Image(FrameInfo.Column) & " -row" &
                  Natural'Image(FrameInfo.Row));
            end if;
         end loop;
         configure
           (Button,
            "-text ""[format %c 0xf106]"" -command {ShipMaxMin " &
            CArgv.Arg(Argv, 1) & " show}");
      else
         for FrameInfo of Frames loop
            Frame.Name :=
              New_String(".paned.shipinfoframe." & To_String(FrameInfo.Name));
            if To_String(FrameInfo.Name) /= CArgv.Arg(Argv, 1) then
               Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
            else
               Tcl.Tk.Ada.Grid.Grid_Configure
                 (Frame, "-columnspan 2 -rowspan 2 -row 0 -column 0");
            end if;
         end loop;
         configure
           (Button,
            "-text ""[format %c 0xf107]"" -command {ShipMaxMin " &
            CArgv.Arg(Argv, 1) & " hide}");
      end if;
      return TCL_OK;
   end Ship_Max_Min_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowShipInfo", Show_Ship_Info_Command'Access);
      AddCommand("SetShipName", Set_Ship_Name_Command'Access);
      AddCommand("ShipMaxMin", Ship_Max_Min_Command'Access);
      Ships.UI.Modules.AddCommands;
   end AddCommands;

   procedure UpdateCrewInfo is
      Label: Ttk_Label;
      CrewInfoFrame, Item: Ttk_Frame;
      UpgradeProgress: Ttk_ProgressBar;
      Tokens: Slice_Set;
      Rows: Natural := 0;
      ShipCanvas: Tk_Canvas;
      ProgressBarStyle: Unbounded_String;
      CrewButton: Ttk_MenuButton;
      Row: Positive := 1;
      NeedRepair, NeedClean: Boolean := False;
      Button: Ttk_Button;
   begin
      CrewInfoFrame.Interp := Get_Context;
      CrewInfoFrame.Name :=
        New_String(CrewInfoFrame & ".paned.shipinfoframe.crew.canvas.frame");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(CrewInfoFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      for I in 1 .. (Rows - 1) loop
         Create
           (Tokens,
            Tcl.Tk.Ada.Grid.Grid_Slaves
              (CrewInfoFrame, "-row" & Positive'Image(I)),
            " ");
         for J in 1 .. Slice_Count(Tokens) loop
            Item.Interp := Get_Context;
            Item.Name := New_String(Slice(Tokens, J));
            Destroy(Item);
         end loop;
      end loop;
      for Module of PlayerShip.Modules loop
         if Module.Durability < Module.MaxDurability then
            NeedRepair := True;
         end if;
         if (Module.Durability > 0 and Module.MType = CABIN)
           and then Module.Cleanliness < Module.Quality then
            NeedClean := True;
         end if;
         exit when NeedClean and NeedRepair;
      end loop;
      if NeedClean then
         Button :=
           Create
             (CrewInfoFrame & ".clean",
              "-text {" &
              Encode
                ("" &
                 Themes_List(To_String(GameSettings.InterfaceTheme))
                   .CleanIcon) &
              "} -style Header.Toolbutton");
         Tcl.Tk.Ada.Grid.Grid(Button);
         Row := 2;
      end if;
      Label := Create(CrewInfoFrame & ".name", "-text {Name}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-row" & Natural'Image(Row));
      Label := Create(CrewInfoFrame & ".order", "-text {Order}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-row" & Natural'Image(Row) & " -column 1");
      Label := Create(CrewInfoFrame & ".health", "-text {Health}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-row" & Natural'Image(Row) & " -column 2");
      Label := Create(CrewInfoFrame & ".tired", "-text {Fatigue}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-row" & Natural'Image(Row) & " -column 3");
      Row := Row + 1;
      for Member of PlayerShip.Crew loop
         CrewButton :=
           Create
             (CrewInfoFrame & ".name" & Trim(Natural'Image(Row), Left),
              "-text {" & To_String(Member.Name) & "}");
         Add(CrewButton, "Show available crew member's options");
         Tcl.Tk.Ada.Grid.Grid
           (CrewButton, "-row" & Natural'Image(Row) & " -sticky w");
         Label :=
           Create
             (CrewInfoFrame & ".order" & Trim(Natural'Image(Row), Left),
              "-text {" & Crew_Orders'Image(Member.Order)(1) &
              To_Lower
                (Crew_Orders'Image(Member.Order)
                   (2 .. Crew_Orders'Image(Member.Order)'Last)) &
              "}");
         Add(Label, "The current order for the selected crew member.");
         Tcl.Tk.Ada.Grid.Grid
           (Label, "-row" & Natural'Image(Row) & " -column 1");
         if Member.Health > 74 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style green.Horizontal.TProgressbar");
         elsif Member.Health > 24 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar");
         else
            ProgressBarStyle :=
              To_Unbounded_String(" -style Horizontal.TProgressbar");
         end if;
         UpgradeProgress :=
           Create
             (CrewInfoFrame & ".health" & Trim(Natural'Image(Row), Left),
              "-value {" & Natural'Image(Member.Health) & "}" &
              To_String(ProgressBarStyle));
         Add
           (UpgradeProgress,
            "The current health level of the selected crew member.");
         Tcl.Tk.Ada.Grid.Grid
           (UpgradeProgress, "-row" & Natural'Image(Row) & " -column 2");
         if Member.Tired - Member.Attributes(ConditionIndex)(1) < 25 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style green.Horizontal.TProgressbar");
         elsif Member.Tired - Member.Attributes(ConditionIndex)(1) > 24 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar");
         else
            ProgressBarStyle :=
              To_Unbounded_String(" -style Horizontal.TProgressbar");
         end if;
         UpgradeProgress :=
           Create
             (CrewInfoFrame & ".fatigue" & Trim(Natural'Image(Row), Left),
              "-value {" &
              Integer'Image
                (Member.Tired - Member.Attributes(ConditionIndex)(1)) &
              "}" & To_String(ProgressBarStyle));
         Add
           (UpgradeProgress,
            "The current tired level of the selected crew member.");
         Tcl.Tk.Ada.Grid.Grid
           (UpgradeProgress, "-row" & Natural'Image(Row) & " -column 3");
         if Member.Thirst < 25 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style green.Horizontal.TProgressbar");
         elsif Member.Thirst > 24 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar");
         else
            ProgressBarStyle :=
              To_Unbounded_String(" -style Horizontal.TProgressbar");
         end if;
         UpgradeProgress :=
           Create
             (CrewInfoFrame & ".thirst" & Trim(Natural'Image(Row), Left),
              "-value {" & Natural'Image(Member.Thirst) & "}" &
              To_String(ProgressBarStyle));
         Add
           (UpgradeProgress,
            "The current thirst level of the selected crew member.");
         Tcl.Tk.Ada.Grid.Grid
           (UpgradeProgress, "-row" & Natural'Image(Row) & " -column 4");
         if Member.Hunger < 25 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style green.Horizontal.TProgressbar");
         elsif Member.Hunger > 24 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar");
         else
            ProgressBarStyle :=
              To_Unbounded_String(" -style Horizontal.TProgressbar");
         end if;
         UpgradeProgress :=
           Create
             (CrewInfoFrame & ".hunger" & Trim(Natural'Image(Row), Left),
              "-value {" & Natural'Image(Member.Hunger) & "}" &
              To_String(ProgressBarStyle));
         Add
           (UpgradeProgress,
            "The current hunger level of the selected crew member.");
         Tcl.Tk.Ada.Grid.Grid
           (UpgradeProgress, "-row" & Natural'Image(Row) & " -column 5");
         if Member.Morale(1) > 49 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style green.Horizontal.TProgressbar");
         elsif Member.Morale(1) > 24 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar");
         else
            ProgressBarStyle :=
              To_Unbounded_String(" -style Horizontal.TProgressbar");
         end if;
         UpgradeProgress :=
           Create
             (CrewInfoFrame & ".morale" & Trim(Natural'Image(Row), Left),
              "-value {" & Natural'Image(Member.Morale(1)) & "}" &
              To_String(ProgressBarStyle));
         Add
           (UpgradeProgress,
            "The current morale level of the selected crew member.");
         Tcl.Tk.Ada.Grid.Grid
           (UpgradeProgress, "-row" & Natural'Image(Row) & " -column 6");
         Row := Row + 1;
      end loop;
      Tcl_Eval(Get_Context, "update");
      ShipCanvas.Interp := Get_Context;
      ShipCanvas.Name := New_String(".paned.shipinfoframe.crew.canvas");
      configure
        (ShipCanvas, "-scrollregion [list " & BBox(ShipCanvas, "all") & "]");
      Xview_Move_To(ShipCanvas, "0.0");
      Yview_Move_To(ShipCanvas, "0.0");
   end UpdateCrewInfo;

end Ships.UI;
