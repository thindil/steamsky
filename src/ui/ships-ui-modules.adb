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
with Ada.Exceptions; use Ada.Exceptions;
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
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
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
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Widgets.TtkWidget; use Tcl.Tk.Ada.Widgets.TtkWidget;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Autoscroll; use Tcl.Tklib.Ada.Autoscroll;
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
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Ships.Upgrade; use Ships.Upgrade;
with Utils.UI; use Utils.UI;

package body Ships.UI.Modules is

   -- ****ie* SUModules/SteamSky_ShipUI_Error
   -- FUNCTION
   -- Raised when there are any problems with ship UI
   -- SOURCE
   SteamSky_ShipUI_Error: exception;
   -- ****

   procedure ShowModuleOptions(ModuleIndex: Positive) is
      MaxValue: Positive;
      IsPassenger: Boolean := False;
      ModuleIndexString: constant String :=
        Trim(Positive'Image(ModuleIndex), Left);
      ModuleMenu: Tk_Menu;
   begin
      ModuleMenu.Interp := Get_Context;
      ModuleMenu.Name := New_String(".modulemenu" & ModuleIndexString);
      if Winfo_Get(ModuleMenu, "exists") = "0" then
         ModuleMenu :=
           Create(".modulemenu" & ModuleIndexString, "-tearoff false");
      end if;
      Delete(ModuleMenu, "0", "end");
      Menu.Add
        (ModuleMenu, "command",
         "-label {Rename module} -command {RenameModule " & ModuleIndexString &
         "}");
      if PlayerShip.RepairModule /= ModuleIndex then
         Menu.Add
           (ModuleMenu, "command",
            "-label {Repair selected module as first when damaged} -command {SetRepair assign " &
            ModuleIndexString & "}");
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
         Menu.Add
           (ModuleMenu, "command",
            "-label {Start upgrading module durability} -command {SetUpgrade 1 " &
            ModuleIndexString & "}");
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
               Menu.Add
                 (ModuleMenu, "command",
                  "-label {Start upgrading engine power} -command {SetUpgrade 2 " &
                  ModuleIndexString & "}");
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
               Menu.Add
                 (ModuleMenu, "command",
                  "-label {Start working on reduce fuel usage of this engine} -command {SetUpgrade 3 " &
                  ModuleIndexString & "}");
            end if;
            if not PlayerShip.Modules(ModuleIndex).Disabled then
               Menu.Add
                 (ModuleMenu, "command",
                  "-label {Turn off engine so it stop using fuel} -command {DisableEngine " &
                  ModuleIndexString & "}");
            else
               Menu.Add
                 (ModuleMenu, "command",
                  "-label {Turn on engine so ship will be fly faster} -command {DisableEngine " &
                  ModuleIndexString & "}");
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
               Menu.Add
                 (ModuleMenu, "command",
                  "-label {Start upgrading cabin quality} -command {SetUpgrade 2 " &
                  ModuleIndexString & "}");
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
               Menu.Add
                 (ModuleMenu, "command",
                  "-label {Assign a crew member as owner of cabin...} -command {ShowAssignCrew " &
                  ModuleIndexString & "}");
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
                  if PlayerShip.Modules(ModuleIndex).MType = GUN then
                     Menu.Add
                       (ModuleMenu, "command",
                        "-label {Start upgrading damage of gun} -command {SetUpgrade 2 " &
                        ModuleIndexString & "}");
                  else
                     Menu.Add
                       (ModuleMenu, "command",
                        "-label {Start upgrading strength of gun} -command {SetUpgrade 2 " &
                        ModuleIndexString & "}");
                  end if;
               end if;
            end;
            Menu.Add
              (ModuleMenu, "command",
               "-label {Assign a crew member as gunner...} -command {ShowAssignCrew " &
               ModuleIndexString & "}");
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
                 New_String(Widget_Image(ModuleMenu) & ".ammomenu");
               if Winfo_Get(AmmoMenu, "exists") = "0" then
                  AmmoMenu :=
                    Create
                      (Widget_Image(ModuleMenu) & ".ammomenu",
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
                  Menu.Add
                    (ModuleMenu, "cascade",
                     "-label {Assign an ammo to gun} -menu " &
                     Widget_Image(AmmoMenu));
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
               Menu.Add
                 (ModuleMenu, "command",
                  "-label {Start upgrading damage of battering ram} -command {SetUpgrade 2 " &
                  ModuleIndexString & "}");
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
               Menu.Add
                 (ModuleMenu, "command",
                  "-label {Start enlarging hull so it can have more modules installed} -command {SetUpgrade 2 " &
                  ModuleIndexString & "}");
            end if;
         when ALCHEMY_LAB .. GREENHOUSE =>
            if PlayerShip.Modules(ModuleIndex).CraftingIndex /=
              Null_Unbounded_String then
               Menu.Add
                 (ModuleMenu, "command",
                  "-label {Assign selected crew member as worker...} -command {ShowAssignCrew " &
                  ModuleIndexString & "}");
               Menu.Add
                 (ModuleMenu, "command",
                  "-label {Cancel current crafting order} -command {CancelOrder " &
                  ModuleIndexString & "}");
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
                  Menu.Add
                    (ModuleMenu, "command",
                     "-label {Assign selected crew member as medic...} -command {ShowAssignCrew " &
                     ModuleIndexString & "}");
                  exit;
               end if;
            end loop;
         when TRAINING_ROOM =>
            if PlayerShip.Modules(ModuleIndex).TrainedSkill > 0 then
               Menu.Add
                 (ModuleMenu, "command",
                  "-label {Assign selected crew member as worker...} -command {ShowAssignCrew " &
                  ModuleIndexString & "}");
            end if;
            Menu.Add
              (ModuleMenu, "command",
               "-label {Assign a skill which will be trained in the training room...} -command {ShowAssignSkill " &
               ModuleIndexString & "}");
         when others =>
            null;
      end case;
      Menu.Add
        (ModuleMenu, "command",
         "-label {Show more info about the module} -command {ShowModuleInfo " &
         ModuleIndexString & "}");
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
      ModuleInfo, ProgressBarStyle: Unbounded_String;
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
               Label := Create(Widget_Image(ModuleDialog) & ".cleanlbl");
               DamagePercent :=
                 1.0 - (Float(Module.Cleanliness) / Float(Module.Quality));
               if DamagePercent > 0.0 and DamagePercent < 0.2 then
                  configure(Label, "-text {Bit dusty}");
                  ProgressBarStyle :=
                    To_Unbounded_String
                      (" -style green.Horizontal.TProgressbar");
               elsif DamagePercent > 0.19 and DamagePercent < 0.5 then
                  configure(Label, "-text {Dusty}");
                  ProgressBarStyle :=
                    To_Unbounded_String
                      (" -style yellow.Horizontal.TProgressbar");
               elsif DamagePercent > 0.49 and DamagePercent < 0.8 then
                  configure(Label, "-text {Dirty}");
                  ProgressBarStyle :=
                    To_Unbounded_String
                      (" -style yellow.Horizontal.TProgressbar");
               elsif DamagePercent > 0.79 and DamagePercent < 1.0 then
                  configure(Label, "-text {Very dirty}");
                  ProgressBarStyle := Null_Unbounded_String;
               else
                  configure(Label, "-text {Ruined}");
                  ProgressBarStyle := Null_Unbounded_String;
               end if;
               ProgressBar :=
                 Create
                   (Widget_Image(ModuleDialog) & ".clean",
                    "-orient horizontal -maximum 1.0 -value {" &
                    Float'Image(DamagePercent) & "}" &
                    To_String(ProgressBarStyle));
               Add(ProgressBar, "Cleanliness of the selected cabin");
               Tcl.Tk.Ada.Grid.Grid(Label, "-row 1 -sticky w");
               Tcl.Tk.Ada.Grid.Grid
                 (ProgressBar, "-row 1 -column 1 -sticky we");
               Height :=
                 Height + Positive'Value(Winfo_Get(Label, "reqheight"));
            end if;
            ProgressBar :=
              Create
                (Widget_Image(ModuleDialog) & ".quality",
                 "-orient horizontal -style blue.Horizontal.TProgressbar -maximum 1.0 -value {" &
                 Float'Image(Float(Module.Quality) / 100.0) & "}");
            Add(ProgressBar, "Quality of the selected cabin");
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
            Tcl.Tk.Ada.Grid.Grid(ProgressBar, "-row 2 -column 1 -sticky we");
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
                  "{" & LF & "Set for training " &
                  To_String(Skills_List(Module.TrainedSkill).Name) & ".}");
            else
               Insert
                 (ModuleText, "end", "{" & LF & "Must be set for training.}");
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
         ProgressBar :=
           Create
             (Widget_Image(ModuleDialog) & ".upgrade",
              "-orient horizontal -maximum 1.0 -value {" &
              Float'Image(UpgradePercent) & "}" & To_String(ProgressBarStyle));
         Add(ProgressBar, "The progress of the current upgrade of the module");
         Label :=
           Create
             (Widget_Image(ModuleDialog) & ".upgradelbl",
              "-text {" & To_String(ModuleInfo) & "}");
         Tcl.Tk.Ada.Grid.Grid(Label, "-row 3 -sticky w");
         Tcl.Tk.Ada.Grid.Grid(ProgressBar, "-row 3 -column 1 -sticky we");
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
      procedure UpdateOrder(Order: Crew_Orders) is
      begin
         GiveOrders(PlayerShip, AssignIndex, Order, ModuleIndex);
         if PlayerShip.Crew(AssignIndex).Order /= Order then
            Tcl_SetVar
              (Interp,
               ".moduledialog.canvas.frame.crewbutton" & CArgv.Arg(Argv, 3),
               "0");
         end if;
      end UpdateOrder;
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
               UpdateOrder(Gunner);
            when ALCHEMY_LAB .. GREENHOUSE =>
               UpdateOrder(Craft);
            when MEDICAL_ROOM =>
               UpdateOrder(Heal);
            when TRAINING_ROOM =>
               UpdateOrder(Train);
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
         if PlayerShip.Modules(ModuleIndex).TrainedSkill = AssignIndex then
            return TCL_OK;
         end if;
         PlayerShip.Modules(ModuleIndex).TrainedSkill := AssignIndex;
         AddMessage
           ("You prepared " & To_String(PlayerShip.Modules(ModuleIndex).Name) &
            " for training " & To_String(Skills_List(AssignIndex).Name) & ".",
            OrderMessage);
      end if;
      UpdateMessages;
      return Show_Ship_Info_Command(ClientData, Interp, Argc, Argv);
   exception
      when An_Exception : Crew_Order_Error =>
         ShowMessage(Exception_Message(An_Exception));
         return TCL_OK;
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

   -- ****o* SUModules/Update_Assign_Crew_Command
   -- FUNCTION
   -- Update assign the crew member UI
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- UpdateAssignCrew moduleindex ?crewindex?
   -- Moduleindex is the index of the module to which a new crew members will
   -- be assigned. Crewindex is the index of the crew member which will be
   -- assigned or removed
   -- SOURCE
   function Update_Assign_Crew_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Assign_Crew_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      ModuleIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      Assigned: Natural := 0;
      CrewButton: Ttk_CheckButton;
      ButtonName: Unbounded_String;
      CrewIndex: constant Natural :=
        (if Argc = 3 then Positive'Value(CArgv.Arg(Argv, 2)) else 0);
      InfoLabel: Ttk_Label;
   begin
      if Argc = 3 then
         if Tcl_GetVar
             (Interp,
              ".moduledialog.canvas.frame.crewbutton" & CArgv.Arg(Argv, 2)) =
           "0" then
            for Owner of PlayerShip.Modules(ModuleIndex).Owner loop
               if Owner = CrewIndex then
                  Owner := 0;
                  exit;
               end if;
            end loop;
            if Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                .MType /=
              CABIN then
               GiveOrders(PlayerShip, CrewIndex, Rest, 0, False);
            end if;
         elsif Assign_Module_Command
             (ClientData, Interp, 4,
              CArgv.Empty & "AssignModule" & "crew" & CArgv.Arg(Argv, 1) &
              CArgv.Arg(Argv, 2)) /=
           TCL_OK then
            raise SteamSky_ShipUI_Error
              with "Can't assign a crew member to module";
         end if;
      end if;
      CrewButton.Interp := Interp;
      for I in PlayerShip.Crew.Iterate loop
         CrewButton.Name :=
           New_String
             (".moduledialog.canvas.frame.crewbutton" &
              Trim(Positive'Image(Crew_Container.To_Index(I)), Left));
         State(CrewButton, "!disabled");
      end loop;
      for Owner of PlayerShip.Modules(ModuleIndex).Owner loop
         if Owner /= 0 then
            Assigned := Assigned + 1;
         end if;
      end loop;
      if Assigned = Positive(PlayerShip.Modules(ModuleIndex).Owner.Length) then
         for I in PlayerShip.Crew.Iterate loop
            ButtonName :=
              To_Unbounded_String
                (".moduledialog.canvas.frame.crewbutton" &
                 Trim(Positive'Image(Crew_Container.To_Index(I)), Left));
            if Tcl_GetVar(Interp, To_String(ButtonName)) = "0" then
               CrewButton.Name := New_String(To_String(ButtonName));
               State(CrewButton, "disabled");
            end if;
         end loop;
      end if;
      InfoLabel.Interp := Interp;
      InfoLabel.Name := New_String(".moduledialog.canvas.frame.infolabel");
      if Winfo_Get(InfoLabel, "exists") = "1" then
         configure
           (InfoLabel,
            "-text {Available:" &
            Natural'Image
              (Positive(PlayerShip.Modules(ModuleIndex).Owner.Length) -
               Assigned) &
            "}");
         UpdateHeader;
         UpdateCrewInfo;
      end if;
      return TCL_OK;
   end Update_Assign_Crew_Command;

   -- ****o* SUModules/Show_Assign_Crew_Command
   -- FUNCTION
   -- Show assign the crew member UI
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
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
      ModuleIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      ModuleDialog: constant Tk_Toplevel :=
        Create
          (".moduledialog",
           "-class Dialog -background [ttk::style lookup . -background] -relief solid -borderwidth 2");
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Interp);
      XScroll: constant Ttk_Scrollbar :=
        Create
          (ModuleDialog & ".xscroll",
           "-orient horizontal -command [list .moduledialog.canvas xview]");
      YScroll: constant Ttk_Scrollbar :=
        Create
          (ModuleDialog & ".yscroll",
           "-orient vertical -command [list .moduledialog.canvas yview]");
      CrewCanvas: constant Tk_Canvas :=
        Create
          (ModuleDialog & ".canvas",
           "-yscrollcommand [list " & YScroll &
           " set] -xscrollcommand [list " & XScroll & " set]");
      CrewFrame: constant Ttk_Frame := Create(CrewCanvas & ".frame");
      CloseButton: constant Ttk_Button :=
        Create
          (CrewFrame & ".button",
           "-text Close -command {CloseDialog " & Widget_Image(ModuleDialog) &
           "}");
      Height: Positive := 10;
      Width: Positive := 250;
      CrewButton: Ttk_CheckButton;
      InfoLabel: Ttk_Label :=
        Create
          (CrewFrame & ".titlelabel",
           "-text {Assign a crew member to " &
           To_String(PlayerShip.Modules(ModuleIndex).Name) &
           "} -wraplength 250");
      Assigned: Natural := 0;
   begin
      Tcl.Tk.Ada.Busy.Busy(MainWindow);
      Wm_Set(ModuleDialog, "title", "{Steam Sky - Assign crew}");
      Wm_Set(ModuleDialog, "transient", ".");
      if Tcl_GetVar(Interp, "tcl_platform(os)") = "Linux" then
         Wm_Set(ModuleDialog, "attributes", "-type dialog");
      end if;
      Tcl.Tk.Ada.Pack.Pack(YScroll, " -side right -fill y");
      Tcl.Tk.Ada.Pack.Pack(CrewCanvas, "-expand true -fill both");
      Tcl.Tk.Ada.Pack.Pack(XScroll, "-fill x");
      Autoscroll(YScroll);
      Autoscroll(XScroll);
      Tcl.Tk.Ada.Pack.Pack(InfoLabel);
      Height := Height + Positive'Value(Winfo_Get(InfoLabel, "reqheight"));
      for I in PlayerShip.Crew.Iterate loop
         CrewButton :=
           Create
             (CrewFrame & ".crewbutton" &
              Trim(Positive'Image(Crew_Container.To_Index(I)), Left),
              "-text {" & To_String(PlayerShip.Crew(I).Name) &
              "} -command {UpdateAssignCrew" & Positive'Image(ModuleIndex) &
              Positive'Image(Crew_Container.To_Index(I)) & "}");
         Tcl_SetVar(Interp, Widget_Image(CrewButton), "0");
         for Owner of PlayerShip.Modules(ModuleIndex).Owner loop
            if Owner = Crew_Container.To_Index(I) then
               Tcl_SetVar(Interp, Widget_Image(CrewButton), "1");
               Assigned := Assigned + 1;
               exit;
            end if;
         end loop;
         Tcl.Tk.Ada.Pack.Pack(CrewButton, "-anchor w");
         Height := Height + Positive'Value(Winfo_Get(CrewButton, "reqheight"));
         if Positive'Value(Winfo_Get(CrewButton, "reqwidth")) + 10 > Width then
            Width := Positive'Value(Winfo_Get(CrewButton, "reqwidth")) + 10;
         end if;
      end loop;
      if Update_Assign_Crew_Command(ClientData, Interp, Argc, Argv) /=
        TCL_OK then
         raise SteamSky_ShipUI_Error with "Can't set assign crew UI";
      end if;
      InfoLabel :=
        Create
          (CrewFrame & ".infolabel",
           "-text {Available:" &
           Natural'Image
             (Positive(PlayerShip.Modules(ModuleIndex).Owner.Length) -
              Assigned) &
           "}");
      Tcl.Tk.Ada.Pack.Pack(InfoLabel);
      Height := Height + Positive'Value(Winfo_Get(InfoLabel, "reqheight"));
      if Positive'Value(Winfo_Get(InfoLabel, "reqwidth")) > Width then
         Width := Positive'Value(Winfo_Get(InfoLabel, "reqwidth"));
      end if;
      Tcl.Tk.Ada.Pack.Pack(CloseButton);
      Height := Height + Positive'Value(Winfo_Get(CloseButton, "reqheight"));
      Focus(CloseButton);
      if Height > 500 then
         Height := 500;
      end if;
      configure
        (CrewFrame,
         "-height" & Positive'Image(Height) & " -width" &
         Positive'Image(Width));
      Canvas_Create
        (CrewCanvas, "window",
         "0 0 -anchor nw -window " & Widget_Image(CrewFrame));
      configure
        (CrewCanvas, "-scrollregion [list " & BBox(CrewCanvas, "all") & "]");
      Width := Width + Positive'Value(Winfo_Get(YScroll, "reqwidth")) + 5;
      Height := Height + Positive'Value(Winfo_Get(XScroll, "reqheight")) + 5;
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

   -- ****o* SUModules/Show_Assign_Skill_Command
   -- FUNCTION
   -- Show assign the skill UI
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowAssignSkill moduleindex
   -- Moduleindex is the index of the module to which a new skill will
   -- be assigned.
   -- SOURCE
   function Show_Assign_Skill_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Assign_Skill_Command
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
      XScroll: constant Ttk_Scrollbar :=
        Create
          (ModuleDialog & ".xscroll",
           "-orient horizontal -command [list .moduledialog.canvas xview]");
      YScroll: constant Ttk_Scrollbar :=
        Create
          (ModuleDialog & ".yscroll",
           "-orient vertical -command [list .moduledialog.canvas yview]");
      SkillCanvas: constant Tk_Canvas :=
        Create
          (ModuleDialog & ".canvas",
           "-yscrollcommand [list " & YScroll &
           " set] -xscrollcommand [list " & XScroll & " set]");
      SkillFrame: constant Ttk_Frame := Create(SkillCanvas & ".frame");
      CloseButton: constant Ttk_Button :=
        Create
          (SkillFrame & ".button",
           "-text Close -command {CloseDialog " & ModuleDialog & "}");
      Height, Width: Positive := 10;
      InfoLabel: constant Ttk_Label :=
        Create
          (SkillFrame & ".titlelabel",
           "-text {Assign skill to " &
           To_String(PlayerShip.Modules(ModuleIndex).Name) & "}");
      SkillsFrame: constant Ttk_Frame := Create(SkillFrame & ".frame");
      ScrollSkillY: constant Ttk_Scrollbar :=
        Create
          (SkillsFrame & ".scrolly",
           "-orient vertical -command [list " & SkillsFrame & ".view yview]");
      SkillsView: constant Ttk_Tree_View :=
        Create
          (SkillsFrame & ".view",
           "-columns [list name tool] -show headings -yscrollcommand [list " &
           SkillsFrame & ".scrolly set]");
      ToolName, ProtoIndex, Tags, SkillName: Unbounded_String;
   begin
      Tcl.Tk.Ada.Busy.Busy(MainWindow);
--     Autoscroll(ScrollSkillY);
      Wm_Set(ModuleDialog, "title", "{Steam Sky - Assign crew}");
      Wm_Set(ModuleDialog, "transient", ".");
      if Tcl_GetVar(Interp, "tcl_platform(os)") = "Linux" then
         Wm_Set(ModuleDialog, "attributes", "-type dialog");
      end if;
      Tcl.Tk.Ada.Pack.Pack(YScroll, " -side right -fill y");
      Tcl.Tk.Ada.Pack.Pack(SkillCanvas, "-expand true -fill both");
      Tcl.Tk.Ada.Pack.Pack(XScroll, "-fill x");
      Autoscroll(YScroll);
      Autoscroll(XScroll);
      Tcl.Tk.Ada.Pack.Pack(InfoLabel);
      Height := Height + Positive'Value(Winfo_Get(InfoLabel, "reqheight"));
      Heading(SkillsView, "name", "-text {Skill}");
      Heading(SkillsView, "tool", "-text {Training tool}");
      Tag_Configure(SkillsView, "gray", "-foreground gray");
      for I in Skills_List.First_Index .. Skills_List.Last_Index loop
         if Skills_List(I).Tool /= Null_Unbounded_String then
            ProtoIndex := FindProtoItem(ItemType => Skills_List(I).Tool);
            if Items_List(ProtoIndex).ShowType /= Null_Unbounded_String then
               ToolName := Items_List(ProtoIndex).ShowType;
            else
               ToolName := Items_List(ProtoIndex).IType;
            end if;
         end if;
         Tags := Null_Unbounded_String;
         SkillName := Skills_List(I).Name;
         if GetItemAmount(Items_List(ProtoIndex).IType) = 0 then
            Tags := To_Unbounded_String(" -tags [list gray]");
            Append(SkillName, " (no tool)");
         end if;
         Insert
           (SkillsView,
            "{} end -id" & Positive'Image(I) & " -values [list {" &
            To_String(SkillName) & "} {" & To_String(ToolName) & "}]" &
            To_String(Tags));
      end loop;
      if PlayerShip.Modules(ModuleIndex).TrainedSkill > 0 then
         Selection_Set
           (SkillsView,
            "[list" &
            Positive'Image(PlayerShip.Modules(ModuleIndex).TrainedSkill) &
            "]");
         TtkTreeView.Focus
           (SkillsView,
            Positive'Image(PlayerShip.Modules(ModuleIndex).TrainedSkill));
      end if;
      Bind
        (SkillsView, "<<TreeviewSelect>>",
         "{AssignModule skill" & Positive'Image(ModuleIndex) & " [" &
         SkillsView & " focus]}");
      Tcl.Tk.Ada.Pack.Pack(ScrollSkillY, "-side right -fill y");
      Tcl.Tk.Ada.Pack.Pack(SkillsView);
      Tcl.Tk.Ada.Pack.Pack(SkillsFrame);
      Height := Height + Positive'Value(Winfo_Get(SkillsView, "reqheight"));
      Width :=
        Width + Positive'Value(Winfo_Get(SkillsView, "reqwidth")) +
        Positive'Value(Winfo_Get(ScrollSkillY, "reqwidth")) + 5;
      configure(InfoLabel, "-wraplength" & Positive'Image(Width - 10));
      Tcl.Tk.Ada.Pack.Pack(CloseButton, "-side bottom");
      Height := Height + Positive'Value(Winfo_Get(CloseButton, "reqheight"));
      Focus(CloseButton);
      if Height > 500 then
         Height := 500;
      end if;
      configure
        (SkillFrame,
         "-height" & Positive'Image(Height) & " -width" &
         Positive'Image(Width));
      Canvas_Create
        (SkillCanvas, "window", "0 0 -anchor nw -window " & SkillFrame);
      configure
        (SkillCanvas, "-scrollregion [list " & BBox(SkillCanvas, "all") & "]");
      Width := Width + Positive'Value(Winfo_Get(YScroll, "reqwidth")) + 5;
      Height := Height + Positive'Value(Winfo_Get(XScroll, "reqheight")) + 5;
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
   end Show_Assign_Skill_Command;

   -- ****o* SUModules/Cancel_Order_Command
   -- FUNCTION
   -- Cancel the current crafting order
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Cancel moduleindex
   -- Moduleindex is the index of the module which the crafting order will
   -- be canceled
   -- SOURCE
   function Cancel_Order_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Cancel_Order_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      ModuleIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
   begin
      PlayerShip.Modules(ModuleIndex).CraftingIndex := Null_Unbounded_String;
      PlayerShip.Modules(ModuleIndex).CraftingAmount := 0;
      PlayerShip.Modules(ModuleIndex).CraftingTime := 0;
      for Owner of PlayerShip.Modules(ModuleIndex).Owner loop
         if Owner > 0 then
            GiveOrders(PlayerShip, Owner, Rest);
         end if;
      end loop;
      AddMessage
        ("You cancelled crafting order in " &
         To_String(PlayerShip.Modules(ModuleIndex).Name) & ".",
         CraftMessage, RED);
      UpdateMessages;
      UpdateHeader;
      UpdateCrewInfo;
      return TCL_OK;
   end Cancel_Order_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowModuleInfo", Show_Module_Info_Command'Access);
      AddCommand("SetUpgrade", Set_Upgrade_Command'Access);
      AddCommand("AssignModule", Assign_Module_Command'Access);
      AddCommand("DisableEngine", Disable_Engine_Command'Access);
      AddCommand("StopUpgrading", Stop_Upgrading_Command'Access);
      AddCommand("SetRepair", Set_Repair_Command'Access);
      AddCommand("ResetDestination", Reset_Destination_Command'Access);
      AddCommand("RenameModule", Rename_Module_Command'Access);
      AddCommand("ShowAssignCrew", Show_Assign_Crew_Command'Access);
      AddCommand("UpdateAssignCrew", Update_Assign_Crew_Command'Access);
      AddCommand("ShowAssignSkill", Show_Assign_Skill_Command'Access);
      AddCommand("CancelOrder", Cancel_Order_Command'Access);
   end AddCommands;

end Ships.UI.Modules;
