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
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.String_Split; use GNAT.String_Split;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkMenuButton; use Tcl.Tk.Ada.Widgets.TtkMenuButton;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.GetString; use Tcl.Tklib.Ada.GetString;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Bases; use Bases;
with Config; use Config;
with Crafts; use Crafts;
with Factions; use Factions;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Missions; use Missions;
with ShipModules; use ShipModules;
with Ships.Crew; use Ships.Crew;
with Ships.Upgrade; use Ships.Upgrade;
with Utils.UI; use Utils.UI;

package body Ships.UI is

   -- ****if* SUI2/ShowModuleOptions
   -- FUNCTION
   -- Show available options for the selected module
   -- PARAMETERS
   -- ModuleIndex - Index of the player ship module which options will be show
   -- SOURCE
   procedure ShowModuleOptions(ModuleIndex: Positive) is
      -- ****
      ButtonsFrame: Ttk_Frame;
      Button: Ttk_Button;
      MaxValue: Positive;
      IsPassenger: Boolean := False;
      MenuButton: Ttk_MenuButton;
      ModuleIndexString: constant String :=
        Trim(Positive'Image(ModuleIndex), Left);
      AssignMenu: Tk_Menu;
   begin
      ButtonsFrame :=
        Create
          (".paned.shipinfoframe.modules.canvas.frame.actions" &
           ModuleIndexString);
      Button :=
        Create
          (Widget_Image(ButtonsFrame) & ".rename" & ModuleIndexString,
           "-text ""[format %c 0xf044]"" -style Header.Toolbutton -command {RenameModule " &
           ModuleIndexString & "}");
      Add(Button, "Rename module");
      Tcl.Tk.Ada.Grid.Grid(Button);
      MaxValue :=
        Natural
          (Float
             (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                .Durability) *
           1.5);
      if PlayerShip.Modules(ModuleIndex).MaxDurability < MaxValue then
         Button :=
           Create
             (Widget_Image(ButtonsFrame) & ".upgradedurability" &
              ModuleIndexString,
              "-text ""[format %c 0xf6e3]"" -style Header.Toolbutton -command {SetUpgrade 1 " &
              ModuleIndexString & "}");
         Add(Button, "Start upgrading module durability");
         Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 1");
      end if;
      -- Set crew members list
      AssignMenu.Interp := Get_Context;
      AssignMenu.Name := New_String(".shipinfocrewmenu" & ModuleIndexString);
      if Winfo_Get(AssignMenu, "exists") = "0" then
         AssignMenu :=
           Create(".shipinfocrewmenu" & ModuleIndexString, "-tearoff false");
      end if;
      Delete(AssignMenu, "0", "end");
      for I in PlayerShip.Crew.Iterate loop
         if PlayerShip.Modules(ModuleIndex).Owner.Contains
             (Crew_Container.To_Index(I)) then
            Menu.Add
              (AssignMenu, "command",
               "-label {" & To_String(PlayerShip.Crew(I).Name) &
               "(current)} -command {AssignModule crew " & ModuleIndexString &
               Positive'Image(Crew_Container.To_Index(I)) & "}");
         else
            Menu.Add
              (AssignMenu, "command",
               "-label {" & To_String(PlayerShip.Crew(I).Name) &
               "} -command {AssignModule crew " & ModuleIndexString &
               Positive'Image(Crew_Container.To_Index(I)) & "}");
         end if;
      end loop;
      case Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex).MType is
         when ENGINE =>
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            if PlayerShip.Modules(ModuleIndex).Power < MaxValue then
               Button :=
                 Create
                   (Widget_Image(ButtonsFrame) & ".upgradepower" &
                    ModuleIndexString,
                    "-text ""[format %c 0xf546]"" -style Header.Toolbutton -command {SetUpgrade 2 " &
                    ModuleIndexString & "}");
               Add(Button, "Start upgrading engine power");
               Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 2");
            end if;
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .Value) /
                 2.0);
            if PlayerShip.Modules(ModuleIndex).FuelUsage > MaxValue then
               Button :=
                 Create
                   (Widget_Image(ButtonsFrame) & ".reducefuel" &
                    ModuleIndexString,
                    "-text ""[format %c 0xf55d]"" -style Header.Toolbutton -command {SetUpgrade 3 " &
                    ModuleIndexString & "}");
               Add
                 (Button, "Start working on reduce fuel usage of this engine");
               Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 3");
            end if;
            if not PlayerShip.Modules(ModuleIndex).Disabled then
               Button :=
                 Create
                   (Widget_Image(ButtonsFrame) & ".turnoff" &
                    ModuleIndexString,
                    "-text ""[format %c 0xf28d]"" -style Header.Toolbutton -command {DisableEngine " &
                    ModuleIndexString & "}");
               Add(Button, "Turn off engine so it stop using fuel");
               Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 4");
            else
               Button :=
                 Create
                   (Widget_Image(ButtonsFrame) & ".turnoff" &
                    ModuleIndexString,
                    "-text ""[format %c 0xf144]"" -style Header.Toolbutton -command {DisableEngine " &
                    ModuleIndexString & "}");
               Add(Button, "Turn on engine so ship will be fly faster");
               Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 4");
            end if;
         when CABIN =>
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            if PlayerShip.Modules(ModuleIndex).Quality < MaxValue then
               Button :=
                 Create
                   (Widget_Image(ButtonsFrame) & ".upgradequality" &
                    ModuleIndexString,
                    "-text ""[format %c 0xf5aa]"" -style Header.Toolbutton -command {SetUpgrade 2 " &
                    ModuleIndexString & "}");
               Add(Button, "Start upgrading cabin quality");
               Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 2");
            end if;
            Missions_Loop :
            for Mission of AcceptedMissions loop
               if Mission.MType = Passenger then
                  for Owner of PlayerShip.Modules(ModuleIndex).Owner loop
                     if Mission.Data = Owner then
                        IsPassenger := True;
                        exit Missions_Loop;
                     end if;
                  end loop;
               end if;
            end loop Missions_Loop;
            if not IsPassenger then
               MenuButton :=
                 Create
                   (Widget_Image(ButtonsFrame) & ".assigncrew" &
                    ModuleIndexString,
                    "-text ""[format %c 0xf007]"" -style Header.Toolbutton -menu .shipinfocrewmenu" &
                    ModuleIndexString);
               Add(MenuButton, "Assign a crew member as owner of cabin");
               Tcl.Tk.Ada.Grid.Grid(MenuButton, "-row 0 -column 3");
            end if;
         when GUN | HARPOON_GUN =>
            declare
               CurrentValue: Positive;
            begin
               if PlayerShip.Modules(ModuleIndex).MType = GUN then
                  CurrentValue := PlayerShip.Modules(ModuleIndex).Damage;
               else
                  CurrentValue := PlayerShip.Modules(ModuleIndex).Duration;
               end if;
               MaxValue :=
                 Natural
                   (Float
                      (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                         .MaxValue) *
                    1.5);
               if CurrentValue < MaxValue then
                  Button :=
                    Create
                      (Widget_Image(ButtonsFrame) & ".upgradequality" &
                       ModuleIndexString,
                       "-text ""[format %c 0xf666]"" -style Header.Toolbutton -command {SetUpgrade 2 " &
                       ModuleIndexString & "}");
                  if PlayerShip.Modules(ModuleIndex).MType = GUN then
                     Add(Button, "Start upgrading damage of gun");
                  else
                     Add(Button, "Start upgrading strength of gun");
                  end if;
                  Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 2");
               end if;
            end;
            MenuButton :=
              Create
                (Widget_Image(ButtonsFrame) & ".assigncrew" &
                 ModuleIndexString,
                 "-text ""[format %c 0xf007]"" -style Header.Toolbutton -menu .shipinfocrewmenu" &
                 ModuleIndexString);
            Add(MenuButton, "Assign a crew member as gunner");
            Tcl.Tk.Ada.Grid.Grid(MenuButton, "-row 0 -column 3");
            declare
               AmmoIndex: Natural;
               AmmoMenu: Tk_Menu;
               NotEmpty: Boolean := False;
            begin
               if PlayerShip.Modules(ModuleIndex).MType = GUN then
                  AmmoIndex := PlayerShip.Modules(ModuleIndex).AmmoIndex;
               else
                  AmmoIndex := PlayerShip.Modules(ModuleIndex).HarpoonIndex;
               end if;
               AmmoMenu.Interp := Get_Context;
               AmmoMenu.Name := New_String(".shipinfoammomenu");
               Delete(AmmoMenu, "0", "end");
               for I in
                 PlayerShip.Cargo.First_Index ..
                   PlayerShip.Cargo.Last_Index loop
                  if Items_List(PlayerShip.Cargo(I).ProtoIndex).IType =
                    Items_Types
                      (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                         .Value) and
                    I /= AmmoIndex then
                     Menu.Add
                       (AmmoMenu, "command",
                        "-label {" &
                        To_String
                          (Items_List(PlayerShip.Cargo(I).ProtoIndex).Name) &
                        "}");
                     NotEmpty := True;
                  end if;
               end loop;
               if NotEmpty then
                  MenuButton :=
                    Create
                      (Widget_Image(ButtonsFrame) & ".assignammo" &
                       ModuleIndexString,
                       "-text ""[format %c 0xf1e2]"" -style Header.Toolbutton -menu .shipinfoammomenu");
                  Add(MenuButton, "Assign an ammo to gun");
                  Tcl.Tk.Ada.Grid.Grid(MenuButton, "-row 0 -column 4");
               end if;
            end;
         when BATTERING_RAM =>
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            if PlayerShip.Modules(ModuleIndex).Damage2 < MaxValue then
               Button :=
                 Create
                   (Widget_Image(ButtonsFrame) & ".upgradequality" &
                    ModuleIndexString,
                    "-text ""[format %c 0xf666]"" -style Header.Toolbutton -command {SetUpgrade 2 " &
                    ModuleIndexString & "}");
               Add(Button, "Start upgrading damage of battering ram");
               Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 2");
            end if;
         when HULL =>
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            if PlayerShip.Modules(ModuleIndex).MaxModules < MaxValue then
               Button :=
                 Create
                   (Widget_Image(ButtonsFrame) & ".upgradequality" &
                    ModuleIndexString,
                    "-text ""[format %c 0xf568]"" -style Header.Toolbutton -command {SetUpgrade 2 " &
                    ModuleIndexString & "}");
               Add
                 (Button,
                  "Start enlarging hull so it can have more modules installed");
               Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 2");
            end if;
         when ALCHEMY_LAB .. GREENHOUSE =>
            if PlayerShip.Modules(ModuleIndex).CraftingIndex /=
              Null_Unbounded_String then
               MenuButton :=
                 Create
                   (Widget_Image(ButtonsFrame) & ".assigncrew" &
                    ModuleIndexString,
                    "-text ""[format %c 0xf007]"" -style Header.Toolbutton -menu .shipinfocrewmenu" &
                    ModuleIndexString);
               Add(MenuButton, "Assign selected crew member as worker");
               Tcl.Tk.Ada.Grid.Grid(MenuButton, "-row 0 -column 3");
            end if;
         when MEDICAL_ROOM =>
            for Member of PlayerShip.Crew loop
               if Member.Health < 100 and
                 FindItem
                     (Inventory => PlayerShip.Cargo,
                      ItemType =>
                        Factions_List(PlayerShip.Crew(1).Faction)
                          .HealingTools) >
                   0 then
                  MenuButton :=
                    Create
                      (Widget_Image(ButtonsFrame) & ".assigncrew" &
                       ModuleIndexString,
                       "-text ""[format %c 0xf007]"" -style Header.Toolbutton -menu .shipinfocrewmenu" &
                       ModuleIndexString);
                  Add(Button, "Assign selected crew member as medic");
                  Tcl.Tk.Ada.Grid.Grid(MenuButton, "-row 0 -column 3");
                  exit;
               end if;
            end loop;
         when TRAINING_ROOM =>
            MenuButton :=
              Create
                (Widget_Image(ButtonsFrame) & ".assignskill" &
                 ModuleIndexString,
                 "-text ""[format %c 0xf19d]"" -style Header.Toolbutton -menu .shipinfoammomenu");
            Add
              (MenuButton,
               "Assign a skill which will be trained in the training room");
            Tcl.Tk.Ada.Grid.Grid(MenuButton, "-row 0 -column 4");
         when others =>
            null;
      end case;
      Button :=
        Create
          (Widget_Image(ButtonsFrame) & ".showinfo" & ModuleIndexString,
           "-text ""[format %c 0xf05a]"" -style Header.Toolbutton -command {ShowModuleInfo}");
      Add(Button, "Show detailed information about the module");
      Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 5");
      Tcl.Tk.Ada.Grid.Grid
        (ButtonsFrame,
         "-row" & Positive'Image(ModuleIndex + 1) & " -column 2 -sticky w");
   end ShowModuleOptions;

   -- ****o* SUI2/Show_Ship_Info_Command
   -- FUNCTION
   -- Show information about the player's ship
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowShipInfo
   -- SOURCE
   function Show_Ship_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Ship_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argv);
      Label: Ttk_Label;
      Paned: Ttk_PanedWindow;
      ShipInfoFrame, Item: Ttk_Frame;
      UpgradeInfo: Unbounded_String;
      MaxUpgrade: Integer;
      UpgradePercent: Float;
      UpgradeProgress: Ttk_ProgressBar;
      CloseButton, CancelButton: Ttk_Button;
      Tokens: Slice_Set;
      Rows, Row: Natural := 0;
      ShipCanvas: Tk_Canvas;
      CrewMenu: Tk_Menu;
   begin
      Paned.Interp := Interp;
      Paned.Name := New_String(".paned");
      CloseButton.Interp := Interp;
      CloseButton.Name := New_String(".header.closebutton");
      ShipInfoFrame.Interp := Interp;
      ShipInfoFrame.Name := New_String(Widget_Image(Paned) & ".shipinfoframe");
      CrewMenu.Interp := Interp;
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
      -- Set skills list
      CrewMenu.Name := New_String(".shipinfoskillsmenu");
      Delete(CrewMenu, "0", "end");
      declare
         SkillText, ProtoIndex: Unbounded_String;
      begin
         for I in Skills_List.First_Index .. Skills_List.Last_Index loop
            SkillText := Skills_List(I).Name;
            if Skills_List(I).Tool /= Null_Unbounded_String then
               Append(SkillText, " Tool: ");
               ProtoIndex := FindProtoItem(ItemType => Skills_List(I).Tool);
               if Items_List(ProtoIndex).ShowType /= Null_Unbounded_String then
                  Append(SkillText, Items_List(ProtoIndex).ShowType);
               else
                  Append(SkillText, Items_List(ProtoIndex).IType);
               end if;
            end if;
            Menu.Add
              (CrewMenu, "command", "-label {" & To_String(SkillText) & "}");
         end loop;
      end;
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
         configure(UpgradeProgress, "-value" & Float'Image(UpgradePercent));
         if UpgradePercent < 0.11 then
            Append(UpgradeInfo, " (started)");
         elsif UpgradePercent < 0.31 then
            Append(UpgradeInfo, " (designing)");
         elsif UpgradePercent < 0.51 then
            Append(UpgradeInfo, " (base upgrades)");
         elsif UpgradePercent < 0.80 then
            Append(UpgradeInfo, " (advanced upgrades)");
         else
            Append(UpgradeInfo, " (final upgrades)");
         end if;
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
         Label :=
           Create
             (Widget_Image(ShipInfoFrame) & ".name" &
              Trim(Natural'Image(Row), Left),
              "-text {" & To_String(Module.Name) & "}");
         Tcl.Tk.Ada.Grid.Grid
           (Label, "-row" & Natural'Image(Row) & " -sticky w");
         UpgradeProgress :=
           Create
             (Widget_Image(ShipInfoFrame) & ".durability" &
              Trim(Natural'Image(Row), Left),
              "-value {" &
              Float'Image
                (Float(Module.Durability) / Float(Module.MaxDurability)) &
              "} -maximum 1.0");
         Tcl.Tk.Ada.Grid.Grid
           (UpgradeProgress, "-row" & Natural'Image(Row) & " -column 1");
         ShowModuleOptions(Row - 1);
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
      ShipInfoFrame.Name := New_String(Widget_Image(Paned) & ".shipinfoframe");
      ShipInfoFrame.Name := New_String(Widget_Image(ShipInfoFrame) & ".crew");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(ShipInfoFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      for I in 1 .. (Rows - 1) loop
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
      Row := 1;
      for Member of PlayerShip.Crew loop
         Label :=
           Create
             (Widget_Image(ShipInfoFrame) & ".name" &
              Trim(Natural'Image(Row), Left),
              "-text {" & To_String(Member.Name) & "}");
         Tcl.Tk.Ada.Grid.Grid(Label, "-row" & Natural'Image(Row));
         Label :=
           Create
             (Widget_Image(ShipInfoFrame) & ".order" &
              Trim(Natural'Image(Row), Left),
              "-text {" & To_Lower(Crew_Orders'Image(Member.Order)) & "}");
         Tcl.Tk.Ada.Grid.Grid
           (Label, "-row" & Natural'Image(Row) & " -column 1");
         UpgradeProgress :=
           Create
             (Widget_Image(ShipInfoFrame) & ".health" &
              Trim(Natural'Image(Row), Left),
              "-value {" & Natural'Image(Member.Health) & "}");
         Tcl.Tk.Ada.Grid.Grid
           (UpgradeProgress, "-row" & Natural'Image(Row) & " -column 2");
         UpgradeProgress :=
           Create
             (Widget_Image(ShipInfoFrame) & ".fatigue" &
              Trim(Natural'Image(Row), Left),
              "-value {" &
              Integer'Image
                (Member.Tired - Member.Attributes(ConditionIndex)(1)) &
              "}");
         Tcl.Tk.Ada.Grid.Grid
           (UpgradeProgress, "-row" & Natural'Image(Row) & " -column 3");
         UpgradeProgress :=
           Create
             (Widget_Image(ShipInfoFrame) & ".thirst" &
              Trim(Natural'Image(Row), Left),
              "-value {" & Natural'Image(Member.Thirst) & "}");
         Tcl.Tk.Ada.Grid.Grid
           (UpgradeProgress, "-row" & Natural'Image(Row) & " -column 4");
         UpgradeProgress :=
           Create
             (Widget_Image(ShipInfoFrame) & ".hunger" &
              Trim(Natural'Image(Row), Left),
              "-value {" & Natural'Image(Member.Hunger) & "}");
         Tcl.Tk.Ada.Grid.Grid
           (UpgradeProgress, "-row" & Natural'Image(Row) & " -column 5");
         UpgradeProgress :=
           Create
             (Widget_Image(ShipInfoFrame) & ".morale" &
              Trim(Natural'Image(Row), Left),
              "-value {" & Natural'Image(Member.Morale(1)) & "}");
         Tcl.Tk.Ada.Grid.Grid
           (UpgradeProgress, "-row" & Natural'Image(Row) & " -column 6");
         Row := Row + 1;
      end loop;
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

   -- ****if* SUI2/ModuleIndex
   -- FUNCTION
   -- Index of the currently selected module
   -- SOURCE
   ModuleIndex: Positive;
   -- ****

   -- ****o* SUI2/Show_Module_Info_Command
   -- FUNCTION
   -- Show information about the selected module and set option for it
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowModuleInfo
   -- SOURCE
   function Show_Module_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Module_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      Module: ModuleData;
      MaxValue: Positive;
      HaveAmmo: Boolean;
      Mamount, MaxUpgrade: Natural := 0;
      DamagePercent, UpgradePercent: Float;
      ModulesView: Ttk_Tree_View;
      ProgressBar: Ttk_ProgressBar;
      Label: Ttk_Label;
      ModuleText: Tk_Text;
      ModuleInfo: Unbounded_String;
      procedure AddOwnersInfo(OwnersName: String) is
         HaveOwner: Boolean := False;
      begin
         Insert(ModuleText, "end", "{" & LF & OwnersName & "}");
         if Module.Owner.Length > 1 then
            Insert(ModuleText, "end", "s");
         end if;
         Insert
           (ModuleText, "end",
            "{ (max" & Count_Type'Image(Module.Owner.Length) & "): }");
         for I in Module.Owner.First_Index .. Module.Owner.Last_Index loop
            if Module.Owner(I) > 0 then
               if HaveOwner then
                  Insert(ModuleText, "end", "{, }");
               end if;
               HaveOwner := True;
               Insert
                 (ModuleText, "end",
                  To_String(PlayerShip.Crew(Module.Owner(I)).Name));
            end if;
         end loop;
         if not HaveOwner then
            Insert(ModuleText, "end", "{none}");
         end if;
      end AddOwnersInfo;
   begin
      ModulesView.Interp := Interp;
      ModulesView.Name := New_String(".paned.shipinfoframe.modules.modules");
      ModuleIndex := Positive'Value(Selection(ModulesView));
      Module := PlayerShip.Modules(ModuleIndex);
      Label.Interp := Interp;
      Label.Name := New_String(".paned.shipinfoframe.cargo.damagelbl");
      ProgressBar.Interp := Interp;
      ProgressBar.Name := New_String(".paned.shipinfoframe.cargo.damage");
      if Module.Durability < Module.MaxDurability then
         Tcl.Tk.Ada.Grid.Grid(Label);
         DamagePercent :=
           (Float(Module.Durability) / Float(Module.MaxDurability));
         if DamagePercent < 1.0 and DamagePercent > 0.79 then
            configure(Label, "-text {Slightly damaged}");
         elsif DamagePercent < 0.8 and DamagePercent > 0.49 then
            configure(Label, "-text {Damaged}");
         elsif DamagePercent < 0.5 and DamagePercent > 0.19 then
            configure(Label, "-text {Heavily damaged}");
         elsif DamagePercent < 0.2 and DamagePercent > 0.0 then
            configure(Label, "-text {Almost destroyed}");
         elsif DamagePercent = 0.0 then
            configure(Label, "-text {Destroyed}");
         end if;
         configure(ProgressBar, "-value {" & Float'Image(DamagePercent) & "}");
         MaxValue :=
           Positive(Float(Modules_List(Module.ProtoIndex).Durability) * 1.5);
         if Module.MaxDurability = MaxValue then
            configure
              (Label, "-text {" & cget(Label, "-text") & " (max upgrade)}");
         end if;
         Tcl.Tk.Ada.Grid.Grid(ProgressBar);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(Label);
         Tcl.Tk.Ada.Grid.Grid_Remove(ProgressBar);
      end if;
      Label.Name := New_String(".paned.shipinfoframe.cargo.cleanlbl");
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      ProgressBar.Name := New_String(".paned.shipinfoframe.cargo.clean");
      Tcl.Tk.Ada.Grid.Grid_Remove(ProgressBar);
      Label.Name := New_String(".paned.shipinfoframe.cargo.qualitylbl");
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      ProgressBar.Name := New_String(".paned.shipinfoframe.cargo.quality");
      Tcl.Tk.Ada.Grid.Grid_Remove(ProgressBar);
      Label.Name := New_String(".paned.shipinfoframe.cargo.upgradelbl");
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      ProgressBar.Name := New_String(".paned.shipinfoframe.cargo.upgrade");
      Tcl.Tk.Ada.Grid.Grid_Remove(ProgressBar);
      ModuleText.Interp := Interp;
      ModuleText.Name := New_String(".paned.shipinfoframe.cargo.info");
      configure(ModuleText, "-state normal");
      Delete(ModuleText, "1.0", "end");
      Insert
        (ModuleText, "end",
         "{Weight: " & Integer'Image(Module.Weight) & " kg" & LF &
         "Repair/Upgrade material: }");
      for Item of Items_List loop
         if Item.IType = Modules_List(Module.ProtoIndex).RepairMaterial then
            if Mamount > 0 then
               Insert(ModuleText, "end", "{ or }");
            end if;
            if FindItem
                (Inventory => PlayerShip.Cargo, ItemType => Item.IType) =
              0 then
               Insert
                 (ModuleText, "end",
                  "{" & To_String(Item.Name) & "} [list red]");
            else
               Insert(ModuleText, "end", "{" & To_String(Item.Name) & "}");
            end if;
            Mamount := Mamount + 1;
         end if;
      end loop;
      Insert
        (ModuleText, "end",
         "{" & LF & "Repair/Upgrade skill: " &
         To_String
           (Skills_List(Modules_List(Module.ProtoIndex).RepairSkill).Name) &
         "/" &
         To_String
           (Attributes_List
              (Skills_List(Modules_List(Module.ProtoIndex).RepairSkill)
                 .Attribute)
              .Name) &
         "}");
      case Module.MType is
         when ENGINE =>
            Insert
              (ModuleText, "end",
               "{" & LF & "Max power:" & Integer'Image(Module.Power) & "}");
            MaxValue :=
              Positive(Float(Modules_List(Module.ProtoIndex).MaxValue) * 1.5);
            if Module.Power = MaxValue then
               Insert(ModuleText, "end", " (max upgrade)");
            end if;
            if Module.Disabled then
               Insert(ModuleText, "end", " (disabled)");
            end if;
            Insert
              (ModuleText, "end",
               "{" & LF & "Fuel usage:" & Integer'Image(Module.FuelUsage) &
               "}");
            MaxValue :=
              Positive(Float(Modules_List(Module.ProtoIndex).Value) / 2.0);
            if Module.FuelUsage = MaxValue then
               Insert(ModuleText, "end", " (max upgrade)");
            end if;
         when CARGO_ROOM =>
            Insert
              (ModuleText, "end",
               "{" & LF & "Max cargo:" &
               Integer'Image(Modules_List(Module.ProtoIndex).MaxValue) &
               " kg}");
         when HULL =>
            ProgressBar.Name := New_String(".paned.shipinfoframe.cargo.clean");
            Label.Name := New_String(".paned.shipinfoframe.cargo.cleanlbl");
            DamagePercent :=
              Float(Module.InstalledModules) / Float(Module.MaxModules);
            configure(ProgressBar, "-value" & Float'Image(DamagePercent));
            configure
              (Label,
               "-text {Modules installed:" &
               Integer'Image(Module.InstalledModules) & " /" &
               Integer'Image(Module.MaxModules) & "}");
            MaxValue :=
              Positive(Float(Modules_List(Module.ProtoIndex).MaxValue) * 1.5);
            if Module.MaxModules = MaxValue then
               configure
                 (Label, "-text {" & cget(Label, "-text") & " (max upgrade)}");
            end if;
            Tcl.Tk.Ada.Grid.Grid(Label);
            Tcl.Tk.Ada.Grid.Grid(ProgressBar);
         when CABIN =>
            AddOwnersInfo("Owner");
            ProgressBar.Name := New_String(".paned.shipinfoframe.cargo.clean");
            Label.Name := New_String(".paned.shipinfoframe.cargo.cleanlbl");
            if Module.Cleanliness /= Module.Quality then
               DamagePercent :=
                 1.0 - (Float(Module.Cleanliness) / Float(Module.Quality));
               if DamagePercent > 0.0 and DamagePercent < 0.2 then
                  configure(Label, "-text {Bit dusty}");
               elsif DamagePercent > 0.19 and DamagePercent < 0.5 then
                  configure(Label, "-text {Dusty}");
               elsif DamagePercent > 0.49 and DamagePercent < 0.8 then
                  configure(Label, "-text {Dirty}");
               elsif DamagePercent > 0.79 and DamagePercent < 1.0 then
                  configure(Label, "-text {Very dirty}");
               else
                  configure(Label, "-text {Ruined}");
               end if;
               configure(ProgressBar, "-value" & Float'Image(DamagePercent));
               Tcl.Tk.Ada.Grid.Grid(Label);
               Tcl.Tk.Ada.Grid.Grid(ProgressBar);
            end if;
            ProgressBar.Name :=
              New_String(".paned.shipinfoframe.cargo.quality");
            Label.Name := New_String(".paned.shipinfoframe.cargo.qualitylbl");
            configure
              (ProgressBar,
               "-value" & Float'Image(Float(Module.Quality) / 100.0));
            configure
              (Label,
               "-text {Quality: (" & GetCabinQuality(Module.Quality) & ")}");
            MaxValue :=
              Positive(Float(Modules_List(Module.ProtoIndex).MaxValue) * 1.5);
            if Module.Quality = MaxValue then
               configure
                 (Label, "-text {" & cget(Label, "-text") & " (max upgrade)}");
            end if;
            Tcl.Tk.Ada.Grid.Grid(Label);
            Tcl.Tk.Ada.Grid.Grid(ProgressBar);
         when GUN | HARPOON_GUN =>
            Insert(ModuleText, "end", "{" & LF & "Strength:}");
            if Modules_List(Module.ProtoIndex).MType = GUN then
               Insert
                 (ModuleText, "end",
                  "{" & Positive'Image(Module.Damage) & "}");
            else
               Insert
                 (ModuleText, "end",
                  "{" & Positive'Image(Module.Duration) & "}");
            end if;
            Insert(ModuleText, "end", "{" & LF & "Ammunition: }");
            HaveAmmo := False;
            declare
               AmmoIndex: Natural;
            begin
               if Module.MType = GUN then
                  AmmoIndex := Module.AmmoIndex;
               else
                  AmmoIndex := Module.HarpoonIndex;
               end if;
               if
                 (AmmoIndex >= PlayerShip.Cargo.First_Index and
                  AmmoIndex <= PlayerShip.Cargo.Last_Index)
                 and then
                   Items_List(PlayerShip.Cargo(AmmoIndex).ProtoIndex).IType =
                   Items_Types(Modules_List(Module.ProtoIndex).Value) then
                  Insert
                    (ModuleText, "end",
                     "{" &
                     To_String
                       (Items_List(PlayerShip.Cargo(AmmoIndex).ProtoIndex)
                          .Name) &
                     " (assigned)}");
                  HaveAmmo := True;
               end if;
            end;
            if not HaveAmmo then
               Mamount := 0;
               for I in Items_List.Iterate loop
                  if Items_List(I).IType =
                    Items_Types(Modules_List(Module.ProtoIndex).Value) then
                     if Mamount > 0 then
                        Insert(ModuleText, "end", "{ or }");
                     end if;
                     if FindItem(PlayerShip.Cargo, Objects_Container.Key(I)) >
                       0 then
                        Insert
                          (ModuleText, "end",
                           "{" & To_String(Items_List(I).Name) & "}");
                     else
                        Insert
                          (ModuleText, "end",
                           "{" & To_String(Items_List(I).Name) &
                           "} [list red]");
                     end if;
                     Mamount := Mamount + 1;
                  end if;
               end loop;
            end if;
            Insert(ModuleText, "end", "{" & LF & "}");
            if Module.Owner(1) > 0 then
               Insert
                 (ModuleText, "end",
                  "{Gunner: " &
                  To_String(PlayerShip.Crew(Module.Owner(1)).Name) & "}");
            else
               Insert(ModuleText, "end", "Gunner: none");
            end if;
            if Module.MType = GUN then
               Insert(ModuleText, "end", "{" & LF & "}");
               if Modules_List(Module.ProtoIndex).Speed > 0 then
                  Insert
                    (ModuleText, "end",
                     "{Max fire rate:" &
                     Positive'Image(Modules_List(Module.ProtoIndex).Speed) &
                     "/round}");
               else
                  Insert
                    (ModuleText, "end",
                     "{Max fire rate: 1/" &
                     Trim
                       (Integer'Image
                          (abs (Modules_List(Module.ProtoIndex).Speed)),
                        Left) &
                     " rounds}");
               end if;
            end if;
         when TURRET =>
            if Module.GunIndex > 0 then
               Insert
                 (ModuleText, "end",
                  "{" & LF & "Weapon: " &
                  To_String(PlayerShip.Modules(Module.GunIndex).Name) & "}");
            else
               Insert(ModuleText, "end", "{Weapon: none}");
            end if;
         when WORKSHOP =>
            AddOwnersInfo("Worker");
            Insert(ModuleText, "end", "{" & LF & "}");
            if Module.CraftingIndex /= Null_Unbounded_String then
               if Length(Module.CraftingIndex) > 6
                 and then Slice(Module.CraftingIndex, 1, 5) = "Study" then
                  Insert
                    (ModuleText, "end",
                     "{Studying " &
                     To_String
                       (Items_List
                          (Unbounded_Slice
                             (Module.CraftingIndex, 7,
                              Length(Module.CraftingIndex)))
                          .Name) &
                     "}");
               elsif Length(Module.CraftingIndex) > 12
                 and then Slice(Module.CraftingIndex, 1, 11) =
                   "Deconstruct" then
                  Insert
                    (ModuleText, "end",
                     "{Deconstructing " &
                     To_String
                       (Items_List
                          (Unbounded_Slice
                             (Module.CraftingIndex, 13,
                              Length(Module.CraftingIndex)))
                          .Name) &
                     "}");
               else
                  Insert
                    (ModuleText, "end",
                     "{Manufacturing:" &
                     Positive'Image(Module.CraftingAmount) & "x " &
                     To_String
                       (Items_List
                          (Recipes_List(Module.CraftingIndex).ResultIndex)
                          .Name) &
                     "}");
               end if;
               Insert
                 (ModuleText, "end",
                  "{" & LF & "Time to complete current:" &
                  Positive'Image(Module.CraftingTime) & " mins}");
            else
               Insert(ModuleText, "end", "{Manufacturing: nothing}");
            end if;
         when MEDICAL_ROOM =>
            AddOwnersInfo("Medic");
         when TRAINING_ROOM =>
            if Module.TrainedSkill > 0 then
               Insert
                 (ModuleText, "end",
                  "{Set for training " &
                  To_String(Skills_List(Module.TrainedSkill).Name) & ".}");
            else
               Insert(ModuleText, "end", "{Must be set for training.}");
            end if;
            Insert(ModuleText, "end", "{" & LF & "}");
            AddOwnersInfo("Trainee");
         when BATTERING_RAM =>
            Insert
              (ModuleText, "end",
               "Strength:" & Positive'Image(Module.Damage2) & "}");
         when others =>
            null;
      end case;
      if Modules_List(Module.ProtoIndex).Size > 0 then
         Insert
           (ModuleText, "end",
            "{" & LF & "Size:" &
            Natural'Image(Modules_List(Module.ProtoIndex).Size) & "}");
      end if;
      if Modules_List(Module.ProtoIndex).Description /=
        Null_Unbounded_String then
         Insert
           (ModuleText, "end",
            "{" & LF & LF &
            To_String(Modules_List(Module.ProtoIndex).Description) & "}");
      end if;
      if Module.UpgradeAction /= NONE then
         ModuleInfo := To_Unbounded_String("Upgrading: ");
         case Module.UpgradeAction is
            when DURABILITY =>
               Append(ModuleInfo, "durability");
               MaxUpgrade := Modules_List(Module.ProtoIndex).Durability;
            when MAX_VALUE =>
               case Modules_List(Module.ProtoIndex).MType is
                  when ENGINE =>
                     Append(ModuleInfo, "power");
                     MaxUpgrade :=
                       Modules_List(Module.ProtoIndex).MaxValue / 20;
                  when CABIN =>
                     Append(ModuleInfo, "quality");
                     MaxUpgrade := Modules_List(Module.ProtoIndex).MaxValue;
                  when GUN | BATTERING_RAM =>
                     Append(ModuleInfo, "damage");
                     MaxUpgrade :=
                       Modules_List(Module.ProtoIndex).MaxValue * 2;
                  when HULL =>
                     Append(ModuleInfo, "enlarge");
                     MaxUpgrade :=
                       Modules_List(Module.ProtoIndex).MaxValue * 40;
                  when HARPOON_GUN =>
                     Append(ModuleInfo, "strength");
                     MaxUpgrade :=
                       Modules_List(Module.ProtoIndex).MaxValue * 10;
                  when others =>
                     null;
               end case;
            when VALUE =>
               case Modules_List(Module.ProtoIndex).MType is
                  when ENGINE =>
                     Append(ModuleInfo, "fuel usage");
                     MaxUpgrade := Modules_List(Module.ProtoIndex).Value * 20;
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
         ProgressBar.Name := New_String(".paned.shipinfoframe.cargo.upgrade");
         UpgradePercent :=
           1.0 - (Float(Module.UpgradeProgress) / Float(MaxUpgrade));
         configure(ProgressBar, "-value" & Float'Image(UpgradePercent));
         if UpgradePercent < 0.11 then
            Append(ModuleInfo, " (started)");
         elsif UpgradePercent < 0.31 then
            Append(ModuleInfo, " (designing)");
         elsif UpgradePercent < 0.51 then
            Append(ModuleInfo, " (base upgrades)");
         elsif UpgradePercent < 0.80 then
            Append(ModuleInfo, " (advanced upgrades)");
         else
            Append(ModuleInfo, " (final upgrades)");
         end if;
         Label.Name := New_String(".paned.shipinfoframe.cargo.upgradelbl");
         configure(Label, "-text {" & To_String(ModuleInfo) & "}");
         Tcl.Tk.Ada.Grid.Grid(Label);
         Tcl.Tk.Ada.Grid.Grid(ProgressBar);
      end if;
      configure(ModuleText, "-state disabled");
      return TCL_OK;
   end Show_Module_Info_Command;

   -- ****o* SUI2/Set_Upgrade_Command
   -- FUNCTION
   -- Set the selected upgrade for the selected module
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetUpgrade
   -- SOURCE
   function Set_Upgrade_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Upgrade_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
   begin
      StartUpgrading
        (Positive'Value(CArgv.Arg(Argv, 2)),
         Positive'Value(CArgv.Arg(Argv, 1)));
      UpdateOrders(PlayerShip);
      UpdateMessages;
      return Show_Ship_Info_Command(ClientData, Interp, Argc, Argv);
   end Set_Upgrade_Command;

   -- ****o* SUI2/Assign_Module_Command
   -- FUNCTION
   -- Assing member, ammo or skill to module
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- AssignModule
   -- SOURCE
   function Assign_Module_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Assign_Module_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      ModuleIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 2));
      AssignIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 3));
      Assigned: Boolean;
   begin
      if CArgv.Arg(Argv, 1) = "crew" then
         case Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex).MType is
            when CABIN =>
               Modules_Loop :
               for Module of PlayerShip.Modules loop
                  if Module.MType = CABIN then
                     for Owner of Module.Owner loop
                        if Owner = AssignIndex then
                           Owner := 0;
                           exit Modules_Loop;
                        end if;
                     end loop;
                  end if;
               end loop Modules_Loop;
               Assigned := False;
               for Owner of PlayerShip.Modules(ModuleIndex).Owner loop
                  if Owner = 0 then
                     Owner := AssignIndex;
                     Assigned := True;
                     exit;
                  end if;
               end loop;
               if not Assigned then
                  PlayerShip.Modules(ModuleIndex).Owner(1) := AssignIndex;
               end if;
               AddMessage
                 ("You assigned " &
                  To_String(PlayerShip.Modules(ModuleIndex).Name) & " to " &
                  To_String(PlayerShip.Crew(AssignIndex).Name) & ".",
                  OrderMessage);
            when GUN | HARPOON_GUN =>
               GiveOrders(PlayerShip, AssignIndex, Gunner, ModuleIndex);
            when ALCHEMY_LAB .. GREENHOUSE =>
               GiveOrders(PlayerShip, AssignIndex, Craft, ModuleIndex);
            when MEDICAL_ROOM =>
               GiveOrders(PlayerShip, AssignIndex, Heal, ModuleIndex);
            when others =>
               null;
         end case;
         UpdateHeader;
      elsif CArgv.Arg(Argv, 1) = "ammo" then
--         for I in PlayerShip.Cargo.Iterate loop
--            if Items_List(PlayerShip.Cargo(I).ProtoIndex).Name =
--              To_Unbounded_String(Get(ComboBox)) then
--               AssignIndex := Inventory_Container.To_Index(I);
--               exit;
--            end if;
--         end loop;
         if PlayerShip.Modules(ModuleIndex).MType = GUN then
            PlayerShip.Modules(ModuleIndex).AmmoIndex := AssignIndex;
         else
            PlayerShip.Modules(ModuleIndex).HarpoonIndex := AssignIndex;
         end if;
         AddMessage
           ("You assigned " &
            To_String
              (Items_List(PlayerShip.Cargo(AssignIndex).ProtoIndex).Name) &
            " to " & To_String(PlayerShip.Modules(ModuleIndex).Name) & ".",
            OrderMessage);
      elsif CArgv.Arg(Argv, 1) = "skill" then
--         for I in Skills_List.Iterate loop
--            if Skills_List(I).Name = To_Unbounded_String(Get(ComboBox)) then
--               AssignIndex := SkillsData_Container.To_Index(I);
--               exit;
--            end if;
--         end loop;
         PlayerShip.Modules(ModuleIndex).TrainedSkill := AssignIndex;
         AddMessage
           ("You prepared " & To_String(PlayerShip.Modules(ModuleIndex).Name) &
            " for training " & To_String(Skills_List(AssignIndex).Name) & ".",
            OrderMessage);
      end if;
      UpdateMessages;
      return Show_Ship_Info_Command(ClientData, Interp, Argc, Argv);
   end Assign_Module_Command;

   -- ****o* SUI2/Disable_Engine_Command
   -- FUNCTION
   -- Enable or disable selected engine
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DisableEngine
   -- SOURCE
   function Disable_Engine_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Disable_Engine_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(Argc);
      CanDisable: Boolean := False;
      ModuleIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
   begin
      if not PlayerShip.Modules(ModuleIndex).Disabled then
         for I in PlayerShip.Modules.Iterate loop
            if PlayerShip.Modules(I).MType = ENGINE
              and then
              (not PlayerShip.Modules(I).Disabled and
               Modules_Container.To_Index(I) /= ModuleIndex) then
               CanDisable := True;
               exit;
            end if;
         end loop;
         if not CanDisable then
            ShowMessage
              ("You can't disable this engine because it is your last working engine.");
            return TCL_OK;
         end if;
         PlayerShip.Modules(ModuleIndex).Disabled := True;
         AddMessage
           ("You disabled " & To_String(PlayerShip.Modules(ModuleIndex).Name) &
            ".",
            OrderMessage);
      else
         PlayerShip.Modules(ModuleIndex).Disabled := False;
         AddMessage
           ("You enabled " & To_String(PlayerShip.Modules(ModuleIndex).Name) &
            ".",
            OrderMessage);
      end if;
      UpdateMessages;
      return Show_Ship_Info_Command(ClientData, Interp, 2, Argv);
   end Disable_Engine_Command;

   -- ****o* SUI2/Stop_Upgrading_Command
   -- FUNCTION
   -- Stop the current ship upgrade
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- StopUpgrading
   -- SOURCE
   function Stop_Upgrading_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Stop_Upgrading_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(Argc);
   begin
      PlayerShip.UpgradeModule := 0;
      for I in PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
         if PlayerShip.Crew(I).Order = Upgrading then
            GiveOrders(PlayerShip, I, Rest);
            exit;
         end if;
      end loop;
      AddMessage("You stopped current upgrade.", OrderMessage);
      UpdateMessages;
      return Show_Ship_Info_Command(ClientData, Interp, 2, Argv);
   end Stop_Upgrading_Command;

   -- ****o* SUI2/Set_Repair_Command
   -- FUNCTION
   -- Set or remove the repair priority from the selected module
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetRepair action
   -- Action can be assing or remove. If assing, then assing the currently
   -- selected module as the repair first, otherwise clear current priority
   -- setting
   -- SOURCE
   function Set_Repair_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Repair_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
   begin
      if CArgv.Arg(Argv, 1) = "assign" then
         PlayerShip.RepairModule := ModuleIndex;
         AddMessage
           ("You assigned " & To_String(PlayerShip.Modules(ModuleIndex).Name) &
            " as repair priority.",
            OrderMessage);
      else
         PlayerShip.RepairModule := 0;
         AddMessage("You removed repair priority.", OrderMessage);
      end if;
      UpdateMessages;
      return Show_Ship_Info_Command(ClientData, Interp, Argc, Argv);
   end Set_Repair_Command;

   -- ****o* SUI2/Reset_Destination_Command
   -- FUNCTION
   -- Reset the current destination point for the player's ship
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ResetDestination
   -- SOURCE
   function Reset_Destination_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Reset_Destination_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(Argc);
   begin
      PlayerShip.DestinationX := 0;
      PlayerShip.DestinationY := 0;
      return Show_Ship_Info_Command(ClientData, Interp, 2, Argv);
   end Reset_Destination_Command;

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
      FramesNames: constant array(1 .. 3) of Unbounded_String :=
        (To_Unbounded_String("general"), To_Unbounded_String("modules"),
         To_Unbounded_String("crew"));
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
         for Name of FramesNames loop
            if To_String(Name) /= CArgv.Arg(Argv, 1) then
               Frame.Name :=
                 New_String(".paned.shipinfoframe." & To_String(Name));
               Tcl.Tk.Ada.Grid.Grid(Frame);
            end if;
         end loop;
         configure
           (Button,
            "-text ""[format %c 0xf106]"" -command {ShipMaxMin " &
            CArgv.Arg(Argv, 1) & " show}");
      else
         for Name of FramesNames loop
            if To_String(Name) /= CArgv.Arg(Argv, 1) then
               Frame.Name :=
                 New_String(".paned.shipinfoframe." & To_String(Name));
               Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
            end if;
         end loop;
         configure
           (Button,
            "-text ""[format %c 0xf107]"" -command {ShipMaxMin " &
            CArgv.Arg(Argv, 1) & " hide}");
      end if;
      return TCL_OK;
   end Ship_Max_Min_Command;

   -- ****o* SUI2/Rename_Module_Command
   -- FUNCTION
   -- Change name of the selected player's ship module
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RenameModule moduleindex
   -- Moduleindex is the index of the module which name will be changed
   -- SOURCE
   function Rename_Module_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Rename_Module_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Label: Ttk_Label;
      ModuleIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
   begin
      Label.Interp := Interp;
      Label.Name :=
        New_String
          (".paned.shipinfoframe.modules.canvas.frame.name" &
           Trim(Positive'Image(ModuleIndex + 1), Left));
      if Tk_Get_String
          (Interp, ".gs", "text",
           "{Enter a new name for the " & cget(Label, "-text") & "}") =
        "0" then
         return TCL_OK;
      end if;
      PlayerShip.Modules(ModuleIndex).Name :=
        To_Unbounded_String(Tcl_GetVar(Interp, "text"));
      configure(Label, "-text $text");
      return TCL_OK;
   end Rename_Module_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowShipInfo", Show_Ship_Info_Command'Access);
      AddCommand("SetShipName", Set_Ship_Name_Command'Access);
      AddCommand("ShowModuleInfo", Show_Module_Info_Command'Access);
      AddCommand("SetUpgrade", Set_Upgrade_Command'Access);
      AddCommand("AssignModule", Assign_Module_Command'Access);
      AddCommand("DisableEngine", Disable_Engine_Command'Access);
      AddCommand("StopUpgrading", Stop_Upgrading_Command'Access);
      AddCommand("SetRepair", Set_Repair_Command'Access);
      AddCommand("ResetDestination", Reset_Destination_Command'Access);
      AddCommand("ShipMaxMin", Ship_Max_Min_Command'Access);
      AddCommand("RenameModule", Rename_Module_Command'Access);
   end AddCommands;

end Ships.UI;
