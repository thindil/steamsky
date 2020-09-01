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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
use Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkMenuButton; use Tcl.Tk.Ada.Widgets.TtkMenuButton;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.GetString; use Tcl.Tklib.Ada.GetString;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
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

package body Ships.UI.Modules is

   procedure ShowModuleOptions(ModuleIndex: Positive) is
      ButtonsFrame: Ttk_Frame;
      Button: Ttk_Button;
      MaxValue: Positive;
      IsPassenger: Boolean := False;
      MenuButton: Ttk_MenuButton;
      ModuleIndexString: constant String :=
        Trim(Positive'Image(ModuleIndex), Left);
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
      if PlayerShip.RepairModule /= ModuleIndex then
         Button :=
           Create
             (Widget_Image(ButtonsFrame) & ".repair" & ModuleIndexString,
              "-text ""[format %c 0xf54a]"" -style Header.Toolbutton -command {SetRepair assign " &
              ModuleIndexString & "}");
         Add(Button, "Repair selected module as first when damaged");
         Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 1");
      end if;
      MaxValue :=
        Natural
          (Float
             (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                .Durability) *
           1.5);
      if PlayerShip.Modules(ModuleIndex).UpgradeAction = DURABILITY and
        PlayerShip.UpgradeModule = ModuleIndex then
         MaxValue := 1;
      end if;
      if PlayerShip.Modules(ModuleIndex).MaxDurability < MaxValue then
         Button :=
           Create
             (Widget_Image(ButtonsFrame) & ".upgradedurability" &
              ModuleIndexString,
              "-text ""[format %c 0xf6e3]"" -style Header.Toolbutton -command {SetUpgrade 1 " &
              ModuleIndexString & "}");
         Add(Button, "Start upgrading module durability");
         Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 2");
      end if;
      case Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex).MType is
         when ENGINE =>
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            if PlayerShip.Modules(ModuleIndex).UpgradeAction = MAX_VALUE and
              PlayerShip.UpgradeModule = ModuleIndex then
               MaxValue := 1;
            end if;
            if PlayerShip.Modules(ModuleIndex).Power < MaxValue then
               Button :=
                 Create
                   (Widget_Image(ButtonsFrame) & ".upgradepower" &
                    ModuleIndexString,
                    "-text ""[format %c 0xf546]"" -style Header.Toolbutton -command {SetUpgrade 2 " &
                    ModuleIndexString & "}");
               Add(Button, "Start upgrading engine power");
               Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 3");
            end if;
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .Value) /
                 2.0);
            if PlayerShip.Modules(ModuleIndex).UpgradeAction = VALUE and
              PlayerShip.UpgradeModule = ModuleIndex then
               MaxValue := PlayerShip.Modules(ModuleIndex).FuelUsage + 1;
            end if;
            if PlayerShip.Modules(ModuleIndex).FuelUsage > MaxValue then
               Button :=
                 Create
                   (Widget_Image(ButtonsFrame) & ".reducefuel" &
                    ModuleIndexString,
                    "-text ""[format %c 0xf55d]"" -style Header.Toolbutton -command {SetUpgrade 3 " &
                    ModuleIndexString & "}");
               Add
                 (Button, "Start working on reduce fuel usage of this engine");
               Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 4");
            end if;
            if not PlayerShip.Modules(ModuleIndex).Disabled then
               Button :=
                 Create
                   (Widget_Image(ButtonsFrame) & ".turnoff" &
                    ModuleIndexString,
                    "-text ""[format %c 0xf28d]"" -style Header.Toolbutton -command {DisableEngine " &
                    ModuleIndexString & "}");
               Add(Button, "Turn off engine so it stop using fuel");
               Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 5");
            else
               Button :=
                 Create
                   (Widget_Image(ButtonsFrame) & ".turnoff" &
                    ModuleIndexString,
                    "-text ""[format %c 0xf144]"" -style Header.Toolbutton -command {DisableEngine " &
                    ModuleIndexString & "}");
               Add(Button, "Turn on engine so ship will be fly faster");
               Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 5");
            end if;
         when CABIN =>
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            if PlayerShip.Modules(ModuleIndex).UpgradeAction = MAX_VALUE and
              PlayerShip.UpgradeModule = ModuleIndex then
               MaxValue := 1;
            end if;
            if PlayerShip.Modules(ModuleIndex).Quality < MaxValue then
               Button :=
                 Create
                   (Widget_Image(ButtonsFrame) & ".upgradequality" &
                    ModuleIndexString,
                    "-text ""[format %c 0xf5aa]"" -style Header.Toolbutton -command {SetUpgrade 2 " &
                    ModuleIndexString & "}");
               Add(Button, "Start upgrading cabin quality");
               Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 3");
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
               Button :=
                 Create
                   (Widget_Image(ButtonsFrame) & ".assigncrew" &
                    ModuleIndexString,
                    "-text ""[format %c 0xf007]"" -style Header.Toolbutton -command {ShowAssignCrew " &
                    ModuleIndexString & "}");
               Add(Button, "Assign a crew member as owner of cabin");
               Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 4");
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
               if PlayerShip.Modules(ModuleIndex).UpgradeAction = MAX_VALUE and
                 PlayerShip.UpgradeModule = ModuleIndex then
                  MaxValue := 1;
               end if;
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
                  Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 3");
               end if;
            end;
            Button :=
              Create
                (Widget_Image(ButtonsFrame) & ".assigncrew" &
                 ModuleIndexString,
                 "-text ""[format %c 0xf007]"" -style Header.Toolbutton -command {ShowAssignCrew " &
                 ModuleIndexString & "}");
            Add(Button, "Assign a crew member as gunner");
            Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 4");
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
               AmmoMenu.Name :=
                 New_String(".shipinfoammomenu" & ModuleIndexString);
               if Winfo_Get(AmmoMenu, "exists") = "0" then
                  AmmoMenu :=
                    Create
                      (".shipinfoammomenu" & ModuleIndexString,
                       "-tearoff false");
               end if;
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
                        "} -command {AssignModule ammo " & ModuleIndexString &
                        Positive'Image(I) & "}");
                     NotEmpty := True;
                  end if;
               end loop;
               if NotEmpty then
                  MenuButton :=
                    Create
                      (Widget_Image(ButtonsFrame) & ".assignammo" &
                       ModuleIndexString,
                       "-text ""[format %c 0xf1e2]"" -style Header.Toolbutton -menu .shipinfoammomenu" &
                       ModuleIndexString);
                  Add(MenuButton, "Assign an ammo to gun");
                  Tcl.Tk.Ada.Grid.Grid(MenuButton, "-row 0 -column 5");
               end if;
            end;
         when BATTERING_RAM =>
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            if PlayerShip.Modules(ModuleIndex).UpgradeAction = MAX_VALUE and
              PlayerShip.UpgradeModule = ModuleIndex then
               MaxValue := 1;
            end if;
            if PlayerShip.Modules(ModuleIndex).Damage2 < MaxValue then
               Button :=
                 Create
                   (Widget_Image(ButtonsFrame) & ".upgradequality" &
                    ModuleIndexString,
                    "-text ""[format %c 0xf666]"" -style Header.Toolbutton -command {SetUpgrade 2 " &
                    ModuleIndexString & "}");
               Add(Button, "Start upgrading damage of battering ram");
               Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 3");
            end if;
         when HULL =>
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            if PlayerShip.Modules(ModuleIndex).UpgradeAction = MAX_VALUE and
              PlayerShip.UpgradeModule = ModuleIndex then
               MaxValue := 1;
            end if;
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
               Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 3");
            end if;
         when ALCHEMY_LAB .. GREENHOUSE =>
            if PlayerShip.Modules(ModuleIndex).CraftingIndex /=
              Null_Unbounded_String then
               Button :=
                 Create
                   (Widget_Image(ButtonsFrame) & ".assigncrew" &
                    ModuleIndexString,
                    "-text ""[format %c 0xf007]"" -style Header.Toolbutton -command {ShowAssignCrew " &
                    ModuleIndexString & "}");
               Add(Button, "Assign selected crew member as worker");
               Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 4");
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
                  Button :=
                    Create
                      (Widget_Image(ButtonsFrame) & ".assigncrew" &
                       ModuleIndexString,
                       "-text ""[format %c 0xf007]"" -style Header.Toolbutton -command {ShowAssignCrew " &
                       ModuleIndexString & "}");
                  Add(Button, "Assign selected crew member as medic");
                  Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 4");
                  exit;
               end if;
            end loop;
         when TRAINING_ROOM =>
            declare
               SkillText, ProtoIndex: Unbounded_String;
               SkillMenu: Tk_Menu;
            begin
               SkillMenu.Interp := Get_Context;
               SkillMenu.Name :=
                 New_String(".shipinfoskillmenu" & ModuleIndexString);
               if Winfo_Get(SkillMenu, "exists") = "0" then
                  SkillMenu :=
                    Create
                      (".shipinfoskillmenu" & ModuleIndexString,
                       "-tearoff false");
               end if;
               Delete(SkillMenu, "0", "end");
               for I in Skills_List.First_Index .. Skills_List.Last_Index loop
                  SkillText := Skills_List(I).Name;
                  if Skills_List(I).Tool /= Null_Unbounded_String then
                     Append(SkillText, " Tool: ");
                     ProtoIndex :=
                       FindProtoItem(ItemType => Skills_List(I).Tool);
                     if Items_List(ProtoIndex).ShowType /=
                       Null_Unbounded_String then
                        Append(SkillText, Items_List(ProtoIndex).ShowType);
                     else
                        Append(SkillText, Items_List(ProtoIndex).IType);
                     end if;
                  end if;
                  Menu.Add
                    (SkillMenu, "command",
                     "-label {" & To_String(SkillText) &
                     "} -command {AssignModule skill " & ModuleIndexString &
                     Positive'Image(I) & "}");
               end loop;
               MenuButton :=
                 Create
                   (Widget_Image(ButtonsFrame) & ".assignskill" &
                    ModuleIndexString,
                    "-text ""[format %c 0xf02d]"" -style Header.Toolbutton -menu .shipinfoskillmenu" &
                    ModuleIndexString & " -direction left");
               Add
                 (MenuButton,
                  "Assign a skill which will be trained in the training room");
               Tcl.Tk.Ada.Grid.Grid(MenuButton, "-row 0 -column 5");
            end;
         when others =>
            null;
      end case;
      Button :=
        Create
          (Widget_Image(ButtonsFrame) & ".showinfo" & ModuleIndexString,
           "-text ""[format %c 0xf05a]"" -style Header.Toolbutton -command {ShowModuleInfo " &
           ModuleIndexString & "}");
      Add(Button, "Show detailed information about the module");
      Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 6");
      Tcl.Tk.Ada.Grid.Grid
        (ButtonsFrame,
         "-row" & Positive'Image(ModuleIndex + 1) & " -column 2 -sticky w");
   end ShowModuleOptions;

   -- ****o* SUModules/Show_Module_Info_Command
   -- FUNCTION
   -- Show information about the selected module and set option for it
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowModuleInfo moduleindex
   -- ModuleIndex is the index of the module to show
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
      pragma Unreferenced(ClientData, Argc);
      Module: ModuleData;
      MaxValue: Positive;
      HaveAmmo: Boolean;
      Mamount, MaxUpgrade: Natural := 0;
      DamagePercent, UpgradePercent: Float;
      ProgressBar: Ttk_ProgressBar;
      Label: Ttk_Label;
      ModuleText: Tk_Text;
      ModuleInfo: Unbounded_String;
      ModuleIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      ModuleDialog: constant Tk_Toplevel :=
        Create
          (".moduledialog",
           "-class Dialog -background [ttk::style lookup . -background] -relief solid -borderwidth 2");
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Interp);
      CloseButton: constant Ttk_Button :=
        Create
          (Widget_Image(ModuleDialog) & ".button",
           "-text Close -command {CloseDialog " & Widget_Image(ModuleDialog) &
           "}");
      Height: Positive := 10;
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
      Module := PlayerShip.Modules(ModuleIndex);
      Tcl.Tk.Ada.Busy.Busy(MainWindow);
      Wm_Set(ModuleDialog, "title", "{Steam Sky - Module Info}");
      Wm_Set(ModuleDialog, "transient", ".");
      if Tcl_GetVar(Interp, "tcl_platform(os)") = "Linux" then
         Wm_Set(ModuleDialog, "attributes", "-type dialog");
      end if;
      if Module.Durability < Module.MaxDurability then
         Label := Create(Widget_Image(ModuleDialog) & ".damagelbl");
         DamagePercent :=
           (Float(Module.Durability) / Float(Module.MaxDurability));
         if DamagePercent < 1.0 and DamagePercent > 0.79 then
            configure(Label, "-text {Status: Slightly damaged}");
         elsif DamagePercent < 0.8 and DamagePercent > 0.49 then
            configure(Label, "-text {Status: Damaged}");
         elsif DamagePercent < 0.5 and DamagePercent > 0.19 then
            configure(Label, "-text {Status: Heavily damaged}");
         elsif DamagePercent < 0.2 and DamagePercent > 0.0 then
            configure(Label, "-text {Status: Almost destroyed}");
         elsif DamagePercent = 0.0 then
            configure(Label, "-text {Status: Destroyed}");
         end if;
         Tcl.Tk.Ada.Grid.Grid(Label);
         Height := Height + Positive'Value(Winfo_Get(Label, "reqheight"));
         MaxValue :=
           Positive(Float(Modules_List(Module.ProtoIndex).Durability) * 1.5);
         if Module.MaxDurability = MaxValue then
            configure
              (Label, "-text {" & cget(Label, "-text") & " (max upgrade)}");
         end if;
      end if;
      ModuleText :=
        Create
          (Widget_Image(ModuleDialog) & ".info",
           "-wrap char -height 15 -width 40");
      Tag_Configure(ModuleText, "red", "-foreground red");
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
            Label :=
              Create
                (Widget_Image(ModuleDialog) & ".modules",
                 "-text {Modules installed:" &
                 Integer'Image(Module.InstalledModules) & " /" &
                 Integer'Image(Module.MaxModules) & "}");
            MaxValue :=
              Positive(Float(Modules_List(Module.ProtoIndex).MaxValue) * 1.5);
            if Module.MaxModules = MaxValue then
               configure
                 (Label, "-text {" & cget(Label, "-text") & " (max upgrade)}");
            end if;
            Tcl.Tk.Ada.Grid.Grid(Label, "-sticky w");
            Height := Height + Positive'Value(Winfo_Get(Label, "reqheight"));
         when CABIN =>
            AddOwnersInfo("Owner");
            if Module.Cleanliness /= Module.Quality then
               Label := Create(Widget_Image(ModuleDialog), ".cleanlbl");
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
               ProgressBar :=
                 Create
                   (Widget_Image(ModuleDialog) & ".clean",
                    "-orient horizontal -style yellow.Horizontal.TProgressbar -maximum 1.0 -value {" &
                    Float'Image(DamagePercent) & "}");
               Tcl.Tk.Ada.Grid.Grid(Label, "-row 1 -sticky w");
               Tcl.Tk.Ada.Grid.Grid(ProgressBar, "-row 1 -column 1");
               Height :=
                 Height + Positive'Value(Winfo_Get(Label, "reqheight"));
            end if;
            ProgressBar :=
              Create
                (Widget_Image(ModuleDialog) & ".quality",
                 "-orient horizontal -style blue.Horizontal.TProgressbar -maximum 1.0 -value {" &
                 Float'Image(Float(Module.Quality) / 100.0) & "}");
            Label :=
              Create
                (Widget_Image(ModuleDialog) & ".qualitylbl",
                 "-text {" & GetCabinQuality(Module.Quality) & "}");
            MaxValue :=
              Positive(Float(Modules_List(Module.ProtoIndex).MaxValue) * 1.5);
            if Module.Quality = MaxValue then
               configure
                 (Label, "-text {" & cget(Label, "-text") & " (max upgrade)}");
            end if;
            Tcl.Tk.Ada.Grid.Grid(Label, "-row 2 -sticky w");
            Tcl.Tk.Ada.Grid.Grid(ProgressBar, "-row 2 -column 1");
            Height := Height + Positive'Value(Winfo_Get(Label, "reqheight"));
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
         UpgradePercent :=
           1.0 - (Float(Module.UpgradeProgress) / Float(MaxUpgrade));
         ProgressBar :=
           Create
             (Widget_Image(ModuleDialog) & ".clean",
              "-orient horizontal -style green.Horizontal.TProgressbar -maximum 1.0 -value {" &
              Float'Image(UpgradePercent) & "}");
         Label :=
           Create
             (Widget_Image(ModuleDialog) & ".upgradelbl",
              "-text {" & To_String(ModuleInfo) & "}");
         Tcl.Tk.Ada.Grid.Grid(Label, "-row 3 -sticky w");
         Tcl.Tk.Ada.Grid.Grid(ProgressBar, "-row 3 -column 1");
         Height := Height + Positive'Value(Winfo_Get(Label, "reqheight"));
      end if;
      configure(ModuleText, "-state disabled");
      Tcl.Tk.Ada.Grid.Grid(ModuleText, "-columnspan 2");
      Height := Height + Positive'Value(Winfo_Get(ModuleText, "reqheight"));
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-columnspan 2");
      Height := Height + Positive'Value(Winfo_Get(CloseButton, "reqheight"));
      Focus(CloseButton);
      declare
         Width: Positive;
         X, Y: Integer;
      begin
         Width := Positive'Value(Winfo_Get(ModuleText, "reqwidth")) + 4;
         X :=
           (Positive'Value(Winfo_Get(ModuleDialog, "vrootwidth")) - Width) / 2;
         if X < 0 then
            X := 0;
         end if;
         Y :=
           (Positive'Value(Winfo_Get(ModuleDialog, "vrootheight")) - Height) /
           2;
         if Y < 0 then
            Y := 0;
         end if;
         Wm_Set
           (ModuleDialog, "geometry",
            Trim(Positive'Image(Width), Left) & "x" &
            Trim(Positive'Image(Height), Left) & "+" &
            Trim(Positive'Image(X), Left) & "+" &
            Trim(Positive'Image(Y), Left));
         Bind
           (ModuleDialog, "<Destroy>",
            "{CloseDialog " & Widget_Image(ModuleDialog) & "}");
         Tcl_Eval(Interp, "update");
      end;
      return TCL_OK;
   end Show_Module_Info_Command;

   -- ****o* SUModules/Set_Upgrade_Command
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
   -- SetUpgrade upgradetype moduleindex
   -- upgradetype is type of upgrade to start: 1, 2 or 3. moduleindex is the
   -- index of the player ship module which will be upgraded
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

   -- ****o* SUModules/Assign_Module_Command
   -- FUNCTION
   -- Assign member, ammo or skill to module
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- AssignModule assigntype moduleindex assignindex
   -- assigntype is type of item to assing to module: crew, ammo, skills.
   -- moduleindex is the index of the playership module to which item will be
   -- assigned. assignindex is the index of the item which will be assigned
   -- to the module
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
         PlayerShip.Modules(ModuleIndex).TrainedSkill := AssignIndex;
         AddMessage
           ("You prepared " & To_String(PlayerShip.Modules(ModuleIndex).Name) &
            " for training " & To_String(Skills_List(AssignIndex).Name) & ".",
            OrderMessage);
      end if;
      UpdateMessages;
      return Show_Ship_Info_Command(ClientData, Interp, Argc, Argv);
   end Assign_Module_Command;

   -- ****o* SUModules/Disable_Engine_Command
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
   -- DisableEngine engineindex
   -- engineindex is the index of the engine module in the player ship
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

   -- ****o* SUModules/Stop_Upgrading_Command
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

   -- ****o* SUModules/Set_Repair_Command
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
         PlayerShip.RepairModule := Positive'Value(CArgv.Arg(Argv, 2));
         AddMessage
           ("You assigned " &
            To_String
              (PlayerShip.Modules(Positive'Value(CArgv.Arg(Argv, 2))).Name) &
            " as repair priority.",
            OrderMessage);
      else
         PlayerShip.RepairModule := 0;
         AddMessage("You removed repair priority.", OrderMessage);
      end if;
      UpdateMessages;
      return Show_Ship_Info_Command(ClientData, Interp, Argc, Argv);
   end Set_Repair_Command;

   -- ****o* SUModules/Reset_Destination_Command
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

   -- ****o* SUModules/Ship_Max_Min_Command
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

   -- ****o* SUModules/Rename_Module_Command
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

   -- ****o* SUModules/Show_Assign_Crew_Command
   -- FUNCTION
   -- Show assign the crew member UI
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowAssignCrew moduleindex
   -- Moduleindex is the index of the module to which a new crew members will
   -- be assigned.
   -- SOURCE
   function Show_Assign_Crew_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Assign_Crew_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      ModuleIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      ModuleDialog: constant Tk_Toplevel :=
        Create
          (".moduledialog",
           "-class Dialog -background [ttk::style lookup . -background] -relief solid -borderwidth 2");
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Interp);
      CloseButton: constant Ttk_Button :=
        Create
          (Widget_Image(ModuleDialog) & ".button",
           "-text Close -command {CloseDialog " & Widget_Image(ModuleDialog) &
           "}");
      Height, Width: Positive := 10;
      CrewButton: Ttk_CheckButton;
   begin
      Tcl.Tk.Ada.Busy.Busy(MainWindow);
      Wm_Set(ModuleDialog, "title", "{Steam Sky - Assign crew}");
      Wm_Set(ModuleDialog, "transient", ".");
      if Tcl_GetVar(Interp, "tcl_platform(os)") = "Linux" then
         Wm_Set(ModuleDialog, "attributes", "-type dialog");
      end if;
      for I in PlayerShip.Crew.Iterate loop
         CrewButton :=
           Create
             (Widget_Image(ModuleDialog) & ".crewbutton" &
              Trim(Positive'Image(Crew_Container.To_Index(I)), Left),
              "-text {" & To_String(PlayerShip.Crew(I).Name) & "}");
         Tcl_SetVar(Interp, Widget_Image(CrewButton), "0");
         for Owner of PlayerShip.Modules(ModuleIndex).Owner loop
            if Owner = Crew_Container.To_Index(I) then
               Tcl_SetVar(Interp, Widget_Image(CrewButton), "1");
               exit;
            end if;
         end loop;
         Tcl.Tk.Ada.Pack.Pack(CrewButton, "-anchor w");
         Height := Height + Positive'Value(Winfo_Get(CrewButton, "reqheight"));
         if Positive'Value(Winfo_Get(CrewButton, "reqwidth")) + 10 > Width then
            Width := Positive'Value(Winfo_Get(CrewButton, "reqwidth")) + 10;
         end if;
      end loop;
      Tcl.Tk.Ada.Pack.Pack(CloseButton);
      Height := Height + Positive'Value(Winfo_Get(CloseButton, "reqheight"));
      Focus(CloseButton);
      declare
         X, Y: Integer;
      begin
         X :=
           (Positive'Value(Winfo_Get(ModuleDialog, "vrootwidth")) - Width) / 2;
         if X < 0 then
            X := 0;
         end if;
         Y :=
           (Positive'Value(Winfo_Get(ModuleDialog, "vrootheight")) - Height) /
           2;
         if Y < 0 then
            Y := 0;
         end if;
         Wm_Set
           (ModuleDialog, "geometry",
            Trim(Positive'Image(Width), Left) & "x" &
            Trim(Positive'Image(Height), Left) & "+" &
            Trim(Positive'Image(X), Left) & "+" &
            Trim(Positive'Image(Y), Left));
         Bind
           (ModuleDialog, "<Destroy>",
            "{CloseDialog " & Widget_Image(ModuleDialog) & "}");
         Tcl_Eval(Interp, "update");
      end;
      return TCL_OK;
   end Show_Assign_Crew_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowModuleInfo", Show_Module_Info_Command'Access);
      AddCommand("SetUpgrade", Set_Upgrade_Command'Access);
      AddCommand("AssignModule", Assign_Module_Command'Access);
      AddCommand("DisableEngine", Disable_Engine_Command'Access);
      AddCommand("StopUpgrading", Stop_Upgrading_Command'Access);
      AddCommand("SetRepair", Set_Repair_Command'Access);
      AddCommand("ResetDestination", Reset_Destination_Command'Access);
      AddCommand("ShipMaxMin", Ship_Max_Min_Command'Access);
      AddCommand("RenameModule", Rename_Module_Command'Access);
      AddCommand("ShowAssignCrew", Show_Assign_Crew_Command'Access);
   end AddCommands;

end Ships.UI.Modules;
