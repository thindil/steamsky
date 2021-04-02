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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Event; use Tcl.Tk.Ada.Event;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases; use Bases;
with Config; use Config;
with Factions; use Factions;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Missions; use Missions;
with ShipModules; use ShipModules;
with Ships.UI.Crew;
with Ships.UI.Cargo;
with Ships.UI.Modules;
with Table; use Table;
with Utils.UI; use Utils.UI;

package body Ships.UI is

   -- ****iv* SUI2/SUI2.ModulesTable
   -- FUNCTION
   -- Table with info about the available items to trade
   -- SOURCE
   ModulesTable: Table_Widget (2);
   -- ****

   function Show_Ship_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argv);
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(".gameframe.paned", Interp);
      ShipInfoFrame: Ttk_Frame := Get_Widget(Paned & ".shipinfoframe", Interp);
      Label: Ttk_Label;
      UpgradeInfo, ProgressBarStyle: Unbounded_String;
      MaxUpgrade: Integer;
      UpgradePercent: Float;
      UpgradeProgress: Ttk_ProgressBar;
      CloseButton: constant Ttk_Button :=
        Get_Widget(".gameframe.header.closebutton", Interp);
      CancelButton: Ttk_Button;
      Row: Natural := 0;
      ShipCanvas: Tk_Canvas :=
        Get_Widget(Paned & ".shipinfoframe.general.canvas");
      TypeBox: constant Ttk_ComboBox :=
        Get_Widget
          (".gameframe.paned.shipinfoframe.cargo.canvas.frame.selecttype.combo",
           Interp);
   begin
      if Winfo_Get(ShipInfoFrame, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(Data_Directory) & "ui" & Dir_Separator & "shipinfo.tcl");
         ShipInfoFrame := Get_Widget(ShipInfoFrame & ".modules.canvas.frame");
         ModulesTable :=
           CreateTable
             (Widget_Image(ShipInfoFrame),
              (To_Unbounded_String("Name"), To_Unbounded_String("Durability")),
              False);
      elsif Winfo_Get(ShipInfoFrame, "ismapped") = "1" and Argc = 1 then
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         Tcl_Eval(Interp, "InvokeButton " & CloseButton);
         Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp repair}");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      ShipInfoFrame.Name :=
        New_String
          (Widget_Image(Paned) & ".shipinfoframe.general.canvas.frame");
      Label := Get_Widget(ShipInfoFrame & ".name");
      configure(Label, "-text {Name: " & To_String(PlayerShip.Name) & "}");
      Label.Name := New_String(ShipInfoFrame & ".upgradelabel");
      UpgradeProgress := Get_Widget(ShipInfoFrame & ".upgrade");
      CancelButton := Get_Widget(ShipInfoFrame & ".cancelupgrade");
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
           Integer
             (Float(MaxUpgrade) * Float(NewGameSettings.UpgradeCostBonus));
         if MaxUpgrade = 0 then
            MaxUpgrade := 1;
         end if;
         UpgradePercent :=
           1.0 -
           (Float
              (PlayerShip.Modules(PlayerShip.UpgradeModule).UpgradeProgress) /
            Float(MaxUpgrade));
         ProgressBarStyle :=
           (if UpgradePercent > 0.74 then
              To_Unbounded_String(" -style green.Horizontal.TProgressbar")
            elsif UpgradePercent > 0.24 then
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar")
            else To_Unbounded_String(" -style Horizontal.TProgressbar"));
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
      Label.Name := New_String(ShipInfoFrame & ".repairlabel");
      CancelButton.Name := New_String(ShipInfoFrame & ".cancelpriority");
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
      Label.Name := New_String(ShipInfoFrame & ".destinationlabel");
      CancelButton.Name := New_String(ShipInfoFrame & ".canceldestination");
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
      Label.Name := New_String(ShipInfoFrame & ".homelabel");
      configure
        (Label,
         "-text {Home: " & To_String(SkyBases(PlayerShip.HomeBase).Name) &
         "}");
      Label.Name := New_String(ShipInfoFrame & ".weight");
      configure
        (Label,
         "-text {Weight:" & Integer'Image(CountShipWeight(PlayerShip)) &
         "kg}");
      Tcl_Eval(Get_Context, "update");
      configure
        (ShipCanvas, "-scrollregion [list " & BBox(ShipCanvas, "all") & "]");
      Xview_Move_To(ShipCanvas, "0.0");
      Yview_Move_To(ShipCanvas, "0.0");
      -- Setting ship modules info
      Row := 2;
      ClearTable(ModulesTable);
      Show_Modules_Menu_Loop :
      for Module of PlayerShip.Modules loop
         AddButton
           (ModulesTable, To_String(Module.Name),
            "Show available module's options",
            "ShowModuleMenu" & Positive'Image(Row - 1), 1);
         AddProgressBar
           (ModulesTable, Module.Durability, Module.MaxDurability,
            "Show available module's options",
            "ShowModuleMenu" & Positive'Image(Row - 1), 2, True);
         Row := Row + 1;
      end loop Show_Modules_Menu_Loop;
      UpdateTable(ModulesTable);
      Tcl_Eval(Get_Context, "update");
      ShipCanvas.Name := New_String(Paned & ".shipinfoframe.modules.canvas");
      configure
        (ShipCanvas, "-scrollregion [list " & BBox(ShipCanvas, "all") & "]");
      Xview_Move_To(ShipCanvas, "0.0");
      Yview_Move_To(ShipCanvas, "0.0");
      -- Setting crew info
      Ships.UI.Crew.UpdateCrewInfo;
      -- Setting cargo info
      Set(TypeBox, "All");
      Generate(TypeBox, "<<ComboboxSelected>>");
      -- Show ship info
      ShowScreen("shipinfoframe");
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
          (".gameframe.paned.shipinfoframe.general.canvas.frame.name", Interp);
   begin
      if Argc = 1 then
         return TCL_OK;
      end if;
      PlayerShip.Name := To_Unbounded_String(CArgv.Arg(Argv, 1));
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
      Frame: Ttk_Frame := Get_Widget(".gameframe.paned.shipinfoframe", Interp);
      Button: constant Ttk_Button :=
        Get_Widget
          (Frame & "." & CArgv.Arg(Argv, 1) & ".canvas.frame.maxmin", Interp);
   begin
      if CArgv.Arg(Argv, 2) /= "show" then
         Show_Frames_Loop :
         for FrameInfo of Frames loop
            Frame.Name :=
              New_String
                (".gameframe.paned.shipinfoframe." &
                 To_String(FrameInfo.Name));
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
            "-text ""[format %c 0xf106]"" -command {ShipMaxMin " &
            CArgv.Arg(Argv, 1) & " show}");
      else
         Hide_Frames_Loop :
         for FrameInfo of Frames loop
            Frame.Name :=
              New_String
                (".gameframe.paned.shipinfoframe." &
                 To_String(FrameInfo.Name));
            if To_String(FrameInfo.Name) /= CArgv.Arg(Argv, 1) then
               Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
            else
               Tcl.Tk.Ada.Grid.Grid_Configure
                 (Frame, "-columnspan 2 -rowspan 2 -row 0 -column 0");
            end if;
         end loop Hide_Frames_Loop;
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
      Ships.UI.Crew.AddCommands;
      Ships.UI.Cargo.AddCommands;
   end AddCommands;

end Ships.UI;
