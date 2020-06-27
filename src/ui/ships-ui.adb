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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Interfaces.C;
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
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Bases; use Bases;
with Config; use Config;
with Crafts; use Crafts;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Missions; use Missions;
with ShipModules; use ShipModules;
with Utils.UI; use Utils.UI;

package body Ships.UI is

   -- ****f* SUI2/Show_Ship_Info_Command
   -- FUNCTION
   -- Show information about the player's ship
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
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
      pragma Unreferenced(ClientData, Argc, Argv);
      Label, UpgradeLabel: Ttk_Label;
      Paned: Ttk_PanedWindow;
      ShipInfoCanvas: Tk_Canvas;
      ShipInfoFrame: Ttk_Frame;
      NameEntry: Ttk_Entry;
      ShipInfo, UpgradeInfo: Unbounded_String;
      MaxUpgrade: Integer;
      UpgradePercent: Float;
      UpgradeProgress: Ttk_ProgressBar;
      ModulesView: Ttk_Tree_View;
   begin
      Paned.Interp := Interp;
      Paned.Name := New_String(".paned");
      ShipInfoFrame.Interp := Interp;
      ShipInfoFrame.Name := New_String(Widget_Image(Paned) & ".shipinfoframe");
      ShipInfoCanvas.Interp := Interp;
      ShipInfoCanvas.Name :=
        New_String(Widget_Image(ShipInfoFrame) & ".canvas");
      Label.Interp := Interp;
      Label.Name :=
        New_String(Widget_Image(ShipInfoCanvas) & ".shipinfo.left.info");
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "shipinfo.tcl");
         Bind(ShipInfoFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(Label, "ismapped") = "1" then
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      ShipInfoFrame.Name :=
        New_String(Widget_Image(ShipInfoCanvas) & ".shipinfo");
      NameEntry.Interp := Interp;
      NameEntry.Name := New_String(Widget_Image(ShipInfoFrame) & ".left.name");
      Delete(NameEntry, "0", "end");
      Insert(NameEntry, "0", To_String(PlayerShip.Name));
      ShipInfo :=
        To_Unbounded_String
          ("Home: " & To_String(SkyBases(PlayerShip.HomeBase).Name));
      UpgradeLabel.Interp := Interp;
      UpgradeLabel.Name :=
        New_String(Widget_Image(ShipInfoFrame) & ".left.upgradelabel");
      UpgradeProgress.Interp := Interp;
      UpgradeProgress.Name :=
        New_String(Widget_Image(ShipInfoFrame) & ".left.upgrade");
      if PlayerShip.UpgradeModule = 0 then
         Tcl.Tk.Ada.Grid.Grid_Remove(UpgradeLabel);
         Tcl.Tk.Ada.Grid.Grid_Remove(UpgradeProgress);
      else
         UpgradeInfo := To_Unbounded_String("Upgrading: ");
         Append
           (UpgradeInfo,
            To_String(PlayerShip.Modules(PlayerShip.UpgradeModule).Name) &
            " ");
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
         configure(UpgradeLabel, "-text {" & To_String(UpgradeInfo) & "}");
         Tcl.Tk.Ada.Grid.Grid(UpgradeLabel, "-column 0 -row 1");
         Tcl.Tk.Ada.Grid.Grid(UpgradeProgress, "-column 1 -row 1");
      end if;
      Append(ShipInfo, LF & "Repair first: ");
      if PlayerShip.RepairModule = 0 then
         Append(ShipInfo, "Any module");
      else
         Append
           (ShipInfo,
            To_String(PlayerShip.Modules(PlayerShip.RepairModule).Name));
      end if;
      Append(ShipInfo, LF & "Destination: ");
      if PlayerShip.DestinationX = 0 and PlayerShip.DestinationY = 0 then
         Append(ShipInfo, "None");
      else
         if SkyMap(PlayerShip.DestinationX, PlayerShip.DestinationY)
             .BaseIndex >
           0 then
            Append
              (ShipInfo,
               To_String
                 (SkyBases
                    (SkyMap(PlayerShip.DestinationX, PlayerShip.DestinationY)
                       .BaseIndex)
                    .Name));
         else
            Append
              (ShipInfo,
               "X:" & Positive'Image(PlayerShip.DestinationX) & " Y:" &
               Positive'Image(PlayerShip.DestinationY));
         end if;
      end if;
      Append
        (ShipInfo,
         LF & "Weight:" & Integer'Image(CountShipWeight(PlayerShip)) & "kg");
      configure(Label, "-text {" & To_String(ShipInfo) & "}");
      ModulesView.Interp := Interp;
      ModulesView.Name :=
        New_String(Widget_Image(ShipInfoFrame) & ".left.modules");
      Delete(ModulesView, "[list " & Children(ModulesView, "{}") & "]");
      for I in PlayerShip.Modules.Iterate loop
         Insert
           (ModulesView,
            "{} end -id" & Positive'Image(Modules_Container.To_Index(I)) &
            " -text {" & To_String(PlayerShip.Modules(I).Name) & "}");
      end loop;
      Selection_Set(ModulesView, "[list 1]");
      configure
        (ShipInfoCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (ShipInfoCanvas, "window",
         "[expr " & Winfo_Get(ShipInfoFrame, "reqwidth") & " / 2] [expr " &
         Winfo_Get(ShipInfoFrame, "reqheight") & " / 2] -window " &
         Widget_Image(ShipInfoFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (ShipInfoCanvas,
         "-scrollregion [list " & BBox(ShipInfoCanvas, "all") & "]");
      ShowScreen("shipinfoframe");
      return TCL_OK;
   end Show_Ship_Info_Command;

   -- ****f* SUI2/Set_Ship_Name_Command
   -- FUNCTION
   -- Change name of the player's ship
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
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
      pragma Unreferenced(ClientData, Argc, Argv);
      NameEntry: Ttk_Entry;
   begin
      NameEntry.Interp := Interp;
      NameEntry.Name :=
        New_String(".paned.shipinfoframe.canvas.shipinfo.left.name");
      PlayerShip.Name := To_Unbounded_String(Get(NameEntry));
      return TCL_OK;
   end Set_Ship_Name_Command;

   -- ****if* SUI2/ModuleIndex
   -- FUNCTION
   -- Index of the currently selected module
   -- SOURCE
   ModuleIndex: Positive;
   -- ****

   -- ****if* SUI2/ShowModuleOptions
   -- FUNCTION
   -- Show available options for the selected module
   -- SOURCE
   procedure ShowModuleOptions is
      -- ****
      ButtonsFrame, Button: Ttk_Frame;
      MaxValue: Positive;
      Tokens: Slice_Set;
      IsPassenger: Boolean := False;
      ComboBox: Ttk_ComboBox;
      ComboOptions: Unbounded_String;
--      procedure ShowAssignSkill is
--         SkillText, ProtoIndex: Unbounded_String;
--         AssignSkillCombo: constant Gtk_Combo_Box_Text :=
--           Gtk_Combo_Box_Text(Get_Object(Builder, "cmbassignskill"));
--      begin
--         if SkillsListSet then
--            return;
--         end if;
--         for I in Skills_List.First_Index .. Skills_List.Last_Index loop
--            SkillText := Skills_List(I).Name;
--            if Skills_List(I).Tool /= Null_Unbounded_String then
--               Append(SkillText, " Tool: ");
--               ProtoIndex := FindProtoItem(ItemType => Skills_List(I).Tool);
--               if Items_List(ProtoIndex).ShowType /= Null_Unbounded_String then
--                  Append(SkillText, Items_List(ProtoIndex).ShowType);
--               else
--                  Append(SkillText, Items_List(ProtoIndex).IType);
--               end if;
--            end if;
--            Append(AssignSkillCombo, Positive'Image(I), To_String(SkillText));
--         end loop;
--         Set_Active(AssignSkillCombo, 0);
--         SkillsListSet := True;
--      end ShowAssignSkill;
      procedure ShowAssignMember is
         Assigned: Boolean;
      begin
         for I in
           PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
            Assigned := False;
            for Owner of PlayerShip.Modules(ModuleIndex).Owner loop
               if Owner = I then
                  Assigned := True;
                  exit;
               end if;
            end loop;
            if not Assigned and PlayerShip.Crew(I).Skills.Length > 0 and
              PlayerShip.Crew(I).ContractLength /= 0 then
               case Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                 .MType is
                  when MEDICAL_ROOM =>
                     if PlayerShip.Crew(I).Health = 100 then
                        Append(ComboOptions, " " & PlayerShip.Crew(I).Name);
                     end if;
                  when others =>
                     Append(ComboOptions, " " & PlayerShip.Crew(I).Name);
               end case;
            end if;
         end loop;
         configure(ComboBox, "-values [list" & To_String(ComboOptions) & "]");
         Current(ComboBox, "0");
      end ShowAssignMember;
   begin
      ButtonsFrame.Interp := Get_Context;
      ButtonsFrame.Name :=
        New_String(".paned.shipinfoframe.canvas.shipinfo.right.options");
      Button.Interp := Get_Context;
      ComboBox.Interp := Get_Context;
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Slaves(ButtonsFrame), " ");
      for I in 1 .. Slice_Count(Tokens) loop
         if Slice(Tokens, I) /= "" then
            Button.Name := New_String(Slice(Tokens, I));
            Tcl.Tk.Ada.Grid.Grid_Remove(Button);
         end if;
      end loop;
      MaxValue :=
        Natural
          (Float
             (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                .Durability) *
           1.5);
      Button.Name := New_String(Widget_Image(ButtonsFrame) & ".durability");
      if PlayerShip.Modules(ModuleIndex).MaxDurability < MaxValue then
         Tcl.Tk.Ada.Grid.Grid(Button);
      end if;
      case Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex).MType is
         when ENGINE =>
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            if PlayerShip.Modules(ModuleIndex).Power < MaxValue then
               Button.Name :=
                 New_String(Widget_Image(ButtonsFrame) & ".upgrade1");
               configure(Button, "-text {Upgrade engine power}");
               Add(Button, "Start upgrading engine power");
               Tcl.Tk.Ada.Grid.Grid(Button);
            end if;
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .Value) /
                 2.0);
            if PlayerShip.Modules(ModuleIndex).FuelUsage > MaxValue then
               Button.Name :=
                 New_String(Widget_Image(ButtonsFrame) & ".upgrade1");
               configure(Button, "-text {Reduce fuel usage}");
               Add
                 (Button, "Start working on reduce fuel usage of this engine");
               Tcl.Tk.Ada.Grid.Grid(Button);
            end if;
            Button.Name := New_String(Widget_Image(ButtonsFrame) & ".disable");
            Tcl.Tk.Ada.Grid.Grid(Button);
            if not PlayerShip.Modules(ModuleIndex).Disabled then
               configure(Button, "-text {Disable engine}");
               Add(Button, "Turn off engine so it stop using fuel");
            else
               configure(Button, "-text {Enable engine}");
               Add(Button, "Turn on engine so ship will be fly faster");
            end if;
         when CABIN =>
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            if PlayerShip.Modules(ModuleIndex).Quality < MaxValue then
               Button.Name :=
                 New_String(Widget_Image(ButtonsFrame) & ".upgrade1");
               configure(Button, "-text {Upgrade quality}");
               Add(Button, "Start upgrading cabin quality");
               Tcl.Tk.Ada.Grid.Grid(Button);
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
               Button.Name :=
                 New_String(Widget_Image(ButtonsFrame) & ".assigncrew");
               configure(Button, "-text {Assing as owner}");
               Add(Button, "Assign selected crew member as owner of module");
               Tcl.Tk.Ada.Grid.Grid(Button);
               ComboBox.Name :=
                 New_String(Widget_Image(ButtonsFrame) & ".crewcombo");
               ShowAssignMember;
               Tcl.Tk.Ada.Grid.Grid(ComboBox);
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
                  Button.Name :=
                    New_String(Widget_Image(ButtonsFrame) & ".upgrade1");
                  if PlayerShip.Modules(ModuleIndex).MType = GUN then
                     configure(Button, "-text {Upgrade damage}");
                     Add(Button, "Start upgrading damage of gun");
                  else
                     configure(Button, "-text {Upgrade strength}");
                     Add(Button, "Start upgrading strength of gun");
                  end if;
                  Tcl.Tk.Ada.Grid.Grid(Button);
               end if;
            end;
            Button.Name :=
              New_String(Widget_Image(ButtonsFrame) & ".assigncrew");
            configure(Button, "-text {Assign as gunner}");
            Add(Button, "Assign selected crew member as gunner");
            Tcl.Tk.Ada.Grid.Grid(Button);
            ComboBox.Name :=
              New_String(Widget_Image(ButtonsFrame) & ".crewcombo");
            ShowAssignMember;
            Tcl.Tk.Ada.Grid.Grid(ComboBox);
         when BATTERING_RAM =>
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            if PlayerShip.Modules(ModuleIndex).Damage2 < MaxValue then
               Button.Name :=
                 New_String(Widget_Image(ButtonsFrame) & ".upgrade1");
               configure(Button, "-text {Upgrade damage}");
               Add(Button, "Start upgrading damage of battering ram");
               Tcl.Tk.Ada.Grid.Grid(Button);
            end if;
--         when HULL =>
--            MaxValue :=
--              Natural
--                (Float
--                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
--                      .MaxValue) *
--                 1.5);
--            if PlayerShip.Modules(ModuleIndex).MaxModules < MaxValue then
--               Set_Label
--                 (Gtk_Button(Get_Object(Builder, "btnupgrade1")),
--                  "Enlarge _hull");
--               Set_Tooltip_Text
--                 (Gtk_Button(Get_Object(Builder, "btnupgrade1")),
--                  "Start enlarging hull so it can have more modules installed");
--               Show_All(Gtk_Widget(Get_Object(Builder, "btnupgrade1")));
--            else
--               Hide(Gtk_Widget(Get_Object(Builder, "btnupgrade1")));
--            end if;
--         when ALCHEMY_LAB .. GREENHOUSE =>
--            if PlayerShip.Modules(ModuleIndex).CraftingIndex /=
--              Null_Unbounded_String then
--               Set_Label
--                 (Gtk_Button(Get_Object(Builder, "btnassigncrew")),
--                  "Assign as _worker");
--               Set_Tooltip_Text
--                 (Gtk_Button(Get_Object(Builder, "btnassigncrew")),
--                  "Assign selected crew member as worker");
--               Show_All(Gtk_Widget(Get_Object(Builder, "boxassigncrew")));
--            end if;
--         when MEDICAL_ROOM =>
--            for Member of PlayerShip.Crew loop
--               if Member.Health < 100 and
--                 FindItem
--                     (Inventory => PlayerShip.Cargo,
--                      ItemType =>
--                        Factions_List(PlayerShip.Crew(1).Faction)
--                          .HealingTools) >
--                   0 then
--                  Set_Label
--                    (Gtk_Button(Get_Object(Builder, "btnassigncrew")),
--                     "Assign as _medic");
--                  Set_Tooltip_Text
--                    (Gtk_Button(Get_Object(Builder, "btnassigncrew")),
--                     "Assign selected crew member as medic");
--                  Show_All(Gtk_Widget(Get_Object(Builder, "boxassigncrew")));
--                  exit;
--               end if;
--            end loop;
--         when TRAINING_ROOM =>
--            Show_All(Gtk_Widget(Get_Object(Builder, "boxassignskill")));
         when others =>
            null;
      end case;
--      if PlayerShip.Modules(ModuleIndex).UpgradeAction = NONE or
--        PlayerShip.UpgradeModule = ModuleIndex then
--         Hide(Gtk_Widget(Get_Object(Builder, "btncontinue")));
--      else
--         Show_All(Gtk_Widget(Get_Object(Builder, "btncontinue")));
--      end if;
--      if PlayerShip.UpgradeModule = 0 then
--         Hide(Gtk_Widget(Get_Object(Builder, "btnstop")));
--      else
--         Show_All(Gtk_Widget(Get_Object(Builder, "btnstop")));
--      end if;
--      if PlayerShip.RepairModule = ModuleIndex then
--         Hide(Gtk_Widget(Get_Object(Builder, "btnrepairfirst")));
--      else
--         Show_All(Gtk_Widget(Get_Object(Builder, "btnrepairfirst")));
--      end if;
--      if PlayerShip.RepairModule = 0 then
--         Hide(Gtk_Widget(Get_Object(Builder, "btnremovepriority")));
--      else
--         Show_All(Gtk_Widget(Get_Object(Builder, "btnremovepriority")));
--      end if;
--      if Is_Visible(Gtk_Widget(Get_Object(Builder, "boxassigncrew"))) then
--         ShowAssignMember;
--      end if;
--      if Is_Visible(Gtk_Widget(Get_Object(Builder, "boxassignammo"))) then
--         ShowAssignAmmo;
--      end if;
--      if Is_Visible(Gtk_Widget(Get_Object(Builder, "boxassignskill"))) then
--         ShowAssignSkill;
--      end if;
   end ShowModuleOptions;

   -- ****f* SUI2/Show_Module_Info_Command
   -- FUNCTION
   -- Show information about the selected module and set option for it
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
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
      ModulesView.Name :=
        New_String(".paned.shipinfoframe.canvas.shipinfo.left.modules");
      ModuleIndex := Positive'Value(Selection(ModulesView));
      Module := PlayerShip.Modules(ModuleIndex);
      Label.Interp := Interp;
      Label.Name :=
        New_String
          (".paned.shipinfoframe.canvas.shipinfo.right.module.damagelbl");
      ProgressBar.Interp := Interp;
      ProgressBar.Name :=
        New_String(".paned.shipinfoframe.canvas.shipinfo.right.module.damage");
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
         Tcl.Tk.Ada.Grid.Grid(ProgressBar, "-column 1 -row 0");
      end if;
      Label.Name :=
        New_String
          (".paned.shipinfoframe.canvas.shipinfo.right.module.cleanlbl");
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      ProgressBar.Name :=
        New_String(".paned.shipinfoframe.canvas.shipinfo.right.module.clean");
      Tcl.Tk.Ada.Grid.Grid_Remove(ProgressBar);
      Label.Name :=
        New_String
          (".paned.shipinfoframe.canvas.shipinfo.right.module.qualitylbl");
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      ProgressBar.Name :=
        New_String
          (".paned.shipinfoframe.canvas.shipinfo.right.module.quality");
      Tcl.Tk.Ada.Grid.Grid_Remove(ProgressBar);
      Label.Name :=
        New_String
          (".paned.shipinfoframe.canvas.shipinfo.right.module.upgradelbl");
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      ProgressBar.Name :=
        New_String
          (".paned.shipinfoframe.canvas.shipinfo.right.module.upgrade");
      Tcl.Tk.Ada.Grid.Grid_Remove(ProgressBar);
      ModuleText.Interp := Interp;
      ModuleText.Name :=
        New_String(".paned.shipinfoframe.canvas.shipinfo.right.module.info");
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
            ProgressBar.Name :=
              New_String
                (".paned.shipinfoframe.canvas.shipinfo.right.module.clean");
            Label.Name :=
              New_String
                (".paned.shipinfoframe.canvas.shipinfo.right.module.cleanlbl");
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
            Tcl.Tk.Ada.Grid.Grid(Label, "-row 1");
            Tcl.Tk.Ada.Grid.Grid(ProgressBar, "-row 1 -column 1");
         when CABIN =>
            AddOwnersInfo("Owner");
            ProgressBar.Name :=
              New_String
                (".paned.shipinfoframe.canvas.shipinfo.right.module.clean");
            Label.Name :=
              New_String
                (".paned.shipinfoframe.canvas.shipinfo.right.module.cleanlbl");
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
               Tcl.Tk.Ada.Grid.Grid(Label, "-row 1");
               Tcl.Tk.Ada.Grid.Grid(ProgressBar, "-row 1 -column 1");
            end if;
            ProgressBar.Name :=
              New_String
                (".paned.shipinfoframe.canvas.shipinfo.right.module.quality");
            Label.Name :=
              New_String
                (".paned.shipinfoframe.canvas.shipinfo.right.module.qualitylbl");
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
            Tcl.Tk.Ada.Grid.Grid(Label, "-row 2");
            Tcl.Tk.Ada.Grid.Grid(ProgressBar, "-row 2 -column 1");
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
         ProgressBar.Name :=
           New_String
             (".paned.shipinfoframe.canvas.shipinfo.right.module.upgrade");
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
         Label.Name :=
           New_String
             (".paned.shipinfoframe.canvas.shipinfo.right.module.upgradelbl");
         configure(Label, "-text {" & To_String(ModuleInfo) & "}");
         Tcl.Tk.Ada.Grid.Grid(Label, "-row 3");
         Tcl.Tk.Ada.Grid.Grid(ProgressBar, "-row 3 -column 1");
      end if;
      configure(ModuleText, "-state disabled");
      ShowModuleOptions;
      return TCL_OK;
   end Show_Module_Info_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowShipInfo", Show_Ship_Info_Command'Access);
      AddCommand("SetShipName", Set_Ship_Name_Command'Access);
      AddCommand("ShowModuleInfo", Show_Module_Info_Command'Access);
   end AddCommands;

end Ships.UI;
