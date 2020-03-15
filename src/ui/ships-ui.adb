--    Copyright 2018-2020 Bartek thindil Jasicki
--
--    This file is part of Steam Sky.
--
--    Steam Sky is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    Steam Sky is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Label; use Gtk.Label;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Button; use Gtk.Button;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.Progress_Bar; use Gtk.Progress_Bar;
with Gtk.Stack; use Gtk.Stack;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Gtkada.Builder; use Gtkada.Builder;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with ShipModules; use ShipModules;
with Ships.UI.Handlers; use Ships.UI.Handlers;
with Bases; use Bases;
with Missions; use Missions;
with Factions; use Factions;
with Utils.UI; use Utils.UI;
with Config; use Config;

package body Ships.UI is

   -- ****iv* Ships.UI/SkillsListSet
   -- FUNCTION
   -- If true, list of skills was set. Default is false
   -- SOURCE
   SkillsListSet: Boolean := False;
   -- ****

   -- ****if* Ships.UI/ShowAssignMember
   -- FUNCTION
   -- Show crew members which can be assigned to selected module
   -- SOURCE
   procedure ShowAssignMember is
      -- ****
      AssignCrewCombo: constant Gtk_Combo_Box_Text :=
        Gtk_Combo_Box_Text(Get_Object(Builder, "cmbassigncrew"));
      Assigned: Boolean;
   begin
      Remove_All(AssignCrewCombo);
      for I in PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
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
                     Append
                       (AssignCrewCombo, Positive'Image(I),
                        To_String(PlayerShip.Crew(I).Name));
                  end if;
               when others =>
                  Append
                    (AssignCrewCombo, Positive'Image(I),
                     To_String(PlayerShip.Crew(I).Name));
            end case;
         end if;
      end loop;
      Set_Active(AssignCrewCombo, 0);
   end ShowAssignMember;

   -- ****if* Ships.UI/ShowAssignAmmo
   -- FUNCTION
   -- Show ammunition which can be assigned to selected gun
   -- SOURCE
   procedure ShowAssignAmmo is
      -- ****
      HaveAmmo: Boolean := False;
      AssignAmmoCombo: constant Gtk_Combo_Box_Text :=
        Gtk_Combo_Box_Text(Get_Object(Builder, "cmbassignammo"));
      AmmoIndex: Natural;
   begin
      if PlayerShip.Modules(ModuleIndex).MType = GUN then
         AmmoIndex := PlayerShip.Modules(ModuleIndex).AmmoIndex;
      else
         AmmoIndex := PlayerShip.Modules(ModuleIndex).HarpoonIndex;
      end if;
      Remove_All(AssignAmmoCombo);
      for I in PlayerShip.Cargo.First_Index .. PlayerShip.Cargo.Last_Index loop
         if Items_List(PlayerShip.Cargo(I).ProtoIndex).IType =
           Items_Types
             (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                .Value) and
           I /= AmmoIndex then
            Append
              (AssignAmmoCombo, Positive'Image(I),
               To_String(Items_List(PlayerShip.Cargo(I).ProtoIndex).Name));
            HaveAmmo := True;
         end if;
      end loop;
      if not HaveAmmo then
         Hide(AssignAmmoCombo);
         Hide(Gtk_Widget(Get_Object(Builder, "btnassignammo")));
         return;
      end if;
      Set_Active(AssignAmmoCombo, 0);
   end ShowAssignAmmo;

   procedure ShowModuleOptions is
      MaxValue: Positive;
      IsPassenger: Boolean := False;
      procedure ShowAssignSkill is
         SkillText, ProtoIndex: Unbounded_String;
         AssignSkillCombo: constant Gtk_Combo_Box_Text :=
           Gtk_Combo_Box_Text(Get_Object(Builder, "cmbassignskill"));
      begin
         if SkillsListSet then
            return;
         end if;
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
            Append(AssignSkillCombo, Positive'Image(I), To_String(SkillText));
         end loop;
         Set_Active(AssignSkillCombo, 0);
         SkillsListSet := True;
      end ShowAssignSkill;
   begin
      Hide(Gtk_Widget(Get_Object(Builder, "btnupgrade1")));
      Hide(Gtk_Widget(Get_Object(Builder, "btnupgrade2")));
      Hide(Gtk_Widget(Get_Object(Builder, "boxassigncrew")));
      Hide(Gtk_Widget(Get_Object(Builder, "boxassignammo")));
      Hide(Gtk_Widget(Get_Object(Builder, "boxassignskill")));
      Hide(Gtk_Widget(Get_Object(Builder, "btndisableengine")));
      MaxValue :=
        Natural
          (Float
             (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                .Durability) *
           1.5);
      if PlayerShip.Modules(ModuleIndex).MaxDurability >= MaxValue then
         Hide(Gtk_Widget(Get_Object(Builder, "btnupgradedur")));
      else
         Show_All(Gtk_Widget(Get_Object(Builder, "btnupgradedur")));
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
               Set_Label
                 (Gtk_Button(Get_Object(Builder, "btnupgrade1")),
                  "Upgrade e_ngine power");
               Set_Tooltip_Text
                 (Gtk_Button(Get_Object(Builder, "btnupgrade1")),
                  "Start upgrading engine power");
               Show_All(Gtk_Widget(Get_Object(Builder, "btnupgrade1")));
            else
               Hide(Gtk_Widget(Get_Object(Builder, "btnupgrade1")));
            end if;
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .Value) /
                 2.0);
            if PlayerShip.Modules(ModuleIndex).FuelUsage > MaxValue then
               Set_Label
                 (Gtk_Button(Get_Object(Builder, "btnupgrade2")),
                  "Reduce _fuel usage");
               Set_Tooltip_Text
                 (Gtk_Button(Get_Object(Builder, "btnupgrade2")),
                  "Start working on reduce fuel usage of this engine");
               Show_All(Gtk_Widget(Get_Object(Builder, "btnupgrade2")));
            else
               Hide(Gtk_Widget(Get_Object(Builder, "btnupgrade2")));
            end if;
            Show_All(Gtk_Widget(Get_Object(Builder, "btndisableengine")));
            if not PlayerShip.Modules(ModuleIndex).Disabled then
               Set_Label
                 (Gtk_Button
                    (Gtk_Widget(Get_Object(Builder, "btndisableengine"))),
                  "Disable _engine");
               Set_Tooltip_Text
                 (Gtk_Button(Get_Object(Builder, "btndisableengine")),
                  "Turn off engine so it stop using fuel");
            else
               Set_Label
                 (Gtk_Button
                    (Gtk_Widget(Get_Object(Builder, "btndisableengine"))),
                  "Enable _engine");
               Set_Tooltip_Text
                 (Gtk_Button(Get_Object(Builder, "btndisableengine")),
                  "Turn on engine so ship will be fly faster");
            end if;
         when CABIN =>
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            if PlayerShip.Modules(ModuleIndex).Quality < MaxValue then
               Set_Label
                 (Gtk_Button(Get_Object(Builder, "btnupgrade1")),
                  "Upgrade _quality");
               Set_Tooltip_Text
                 (Gtk_Button(Get_Object(Builder, "btnupgrade1")),
                  "Start upgrading cabin quality");
               Show_All(Gtk_Widget(Get_Object(Builder, "btnupgrade1")));
            else
               Hide(Gtk_Widget(Get_Object(Builder, "btnupgrade1")));
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
               Set_Label
                 (Gtk_Button(Get_Object(Builder, "btnassigncrew")),
                  "Assign as _owner");
               Set_Tooltip_Text
                 (Gtk_Button(Get_Object(Builder, "btnassigncrew")),
                  "Assign selected crew member as owner of module");
               Show_All(Gtk_Widget(Get_Object(Builder, "boxassigncrew")));
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
                  if PlayerShip.Modules(ModuleIndex).MType = GUN then
                     Set_Label
                       (Gtk_Button(Get_Object(Builder, "btnupgrade1")),
                        "Upgrade da_mage");
                     Set_Tooltip_Text
                       (Gtk_Button(Get_Object(Builder, "btnupgrade1")),
                        "Start upgrading damage of gun");
                  else
                     Set_Label
                       (Gtk_Button(Get_Object(Builder, "btnupgrade1")),
                        "Upgrade str_ength");
                     Set_Tooltip_Text
                       (Gtk_Button(Get_Object(Builder, "btnupgrade1")),
                        "Start upgrading strength of gun");
                  end if;
                  Show_All(Gtk_Widget(Get_Object(Builder, "btnupgrade1")));
               else
                  Hide(Gtk_Widget(Get_Object(Builder, "btnupgrade1")));
               end if;
            end;
            Set_Label
              (Gtk_Button(Get_Object(Builder, "btnassigncrew")),
               "Assign as _gunner");
            Set_Tooltip_Text
              (Gtk_Button(Get_Object(Builder, "btnassigncrew")),
               "Assign selected crew member as gunner");
            Show_All(Gtk_Widget(Get_Object(Builder, "boxassigncrew")));
            Show_All(Gtk_Widget(Get_Object(Builder, "boxassignammo")));
         when BATTERING_RAM =>
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            if PlayerShip.Modules(ModuleIndex).Damage2 < MaxValue then
               Set_Label
                 (Gtk_Button(Get_Object(Builder, "btnupgrade1")),
                  "Upgrade d_amage");
               Set_Tooltip_Text
                 (Gtk_Button(Get_Object(Builder, "btnupgrade1")),
                  "Start upgrading damage of battering ram");
               Show_All(Gtk_Widget(Get_Object(Builder, "btnupgrade1")));
            else
               Hide(Gtk_Widget(Get_Object(Builder, "btnupgrade1")));
            end if;
         when HULL =>
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            if PlayerShip.Modules(ModuleIndex).MaxModules < MaxValue then
               Set_Label
                 (Gtk_Button(Get_Object(Builder, "btnupgrade1")),
                  "Enlarge _hull");
               Set_Tooltip_Text
                 (Gtk_Button(Get_Object(Builder, "btnupgrade1")),
                  "Start enlarging hull so it can have more modules installed");
               Show_All(Gtk_Widget(Get_Object(Builder, "btnupgrade1")));
            else
               Hide(Gtk_Widget(Get_Object(Builder, "btnupgrade1")));
            end if;
         when ALCHEMY_LAB .. GREENHOUSE =>
            if PlayerShip.Modules(ModuleIndex).CraftingIndex /=
              Null_Unbounded_String then
               Set_Label
                 (Gtk_Button(Get_Object(Builder, "btnassigncrew")),
                  "Assign as _worker");
               Set_Tooltip_Text
                 (Gtk_Button(Get_Object(Builder, "btnassigncrew")),
                  "Assign selected crew member as worker");
               Show_All(Gtk_Widget(Get_Object(Builder, "boxassigncrew")));
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
                  Set_Label
                    (Gtk_Button(Get_Object(Builder, "btnassigncrew")),
                     "Assign as _medic");
                  Set_Tooltip_Text
                    (Gtk_Button(Get_Object(Builder, "btnassigncrew")),
                     "Assign selected crew member as medic");
                  Show_All(Gtk_Widget(Get_Object(Builder, "boxassigncrew")));
                  exit;
               end if;
            end loop;
         when TRAINING_ROOM =>
            Show_All(Gtk_Widget(Get_Object(Builder, "boxassignskill")));
         when others =>
            null;
      end case;
      if PlayerShip.Modules(ModuleIndex).UpgradeAction = NONE or
        PlayerShip.UpgradeModule = ModuleIndex then
         Hide(Gtk_Widget(Get_Object(Builder, "btncontinue")));
      else
         Show_All(Gtk_Widget(Get_Object(Builder, "btncontinue")));
      end if;
      if PlayerShip.UpgradeModule = 0 then
         Hide(Gtk_Widget(Get_Object(Builder, "btnstop")));
      else
         Show_All(Gtk_Widget(Get_Object(Builder, "btnstop")));
      end if;
      if PlayerShip.RepairModule = ModuleIndex then
         Hide(Gtk_Widget(Get_Object(Builder, "btnrepairfirst")));
      else
         Show_All(Gtk_Widget(Get_Object(Builder, "btnrepairfirst")));
      end if;
      if PlayerShip.RepairModule = 0 then
         Hide(Gtk_Widget(Get_Object(Builder, "btnremovepriority")));
      else
         Show_All(Gtk_Widget(Get_Object(Builder, "btnremovepriority")));
      end if;
      if Is_Visible(Gtk_Widget(Get_Object(Builder, "boxassigncrew"))) then
         ShowAssignMember;
      end if;
      if Is_Visible(Gtk_Widget(Get_Object(Builder, "boxassignammo"))) then
         ShowAssignAmmo;
      end if;
      if Is_Visible(Gtk_Widget(Get_Object(Builder, "boxassignskill"))) then
         ShowAssignSkill;
      end if;
   end ShowModuleOptions;

   procedure ShowShipInfo is
      ShipInfo, UpgradeInfo: Unbounded_String;
      UpgradePercent: Gdouble;
      MaxUpgrade: Integer;
      UpgradeBar: constant GObject := Get_Object(Builder, "upgradebar");
   begin
      Set_Text
        (Gtk_Entry(Get_Object(Builder, "edtname")),
         To_String(PlayerShip.Name));
      ShipInfo :=
        To_Unbounded_String
          ("Home: " & To_String(SkyBases(PlayerShip.HomeBase).Name));
      if PlayerShip.UpgradeModule = 0 then
         Hide(Gtk_Widget(UpgradeBar));
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
           (Gdouble
              (PlayerShip.Modules(PlayerShip.UpgradeModule).UpgradeProgress) /
            Gdouble(MaxUpgrade));
         Set_Fraction(Gtk_Progress_Bar(UpgradeBar), UpgradePercent);
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
         Set_Text(Gtk_Progress_Bar(UpgradeBar), To_String(UpgradeInfo));
         Show_All(Gtk_Widget(UpgradeBar));
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
      Set_Label
        (Gtk_Label(Get_Object(Builder, "lblshipinfo")), To_String(ShipInfo));
   end ShowShipInfo;

   procedure CreateShipUI is
   begin
      Register_Handler(Builder, "Show_Module_Info", ShowModuleInfo'Access);
      Register_Handler(Builder, "Change_Ship_Name", ChangeShipName'Access);
      Register_Handler(Builder, "Set_Upgrade", SetUpgrade'Access);
      Register_Handler(Builder, "Stop_Upgrading", StopUpgrading'Access);
      Register_Handler(Builder, "Set_Repair", SetRepair'Access);
      Register_Handler(Builder, "Assign", Assign'Access);
      Register_Handler(Builder, "Disable_Engine", DisableEngine'Access);
      Register_Handler(Builder, "Toggle_Search", ToggleSearch'Access);
      On_Edited
        (Gtk_Cell_Renderer_Text(Get_Object(Builder, "rendername")),
         ChangeModuleName'Access);
      On_Key_Press_Event
        (Gtk_Widget(Get_Object(Builder, "edtname")), SelectElement'Access,
         Get_Object(Builder, "btnmenu"));
   end CreateShipUI;

   procedure ShowShipUI is
      ListIter: Gtk_Tree_Iter;
      List: Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "moduleslist"));
      Visible: array(1 .. 4) of Boolean := (others => False);
      ColumnNames: constant array(1 .. 4) of Unbounded_String :=
        (To_Unbounded_String("columnshipfatigue"),
         To_Unbounded_String("columnshipthirst"),
         To_Unbounded_String("columnshiphunger"),
         To_Unbounded_String("columnshipmorale"));
   begin
      Set_Sensitive(Gtk_Widget(Get_Object(Builder, "edtname")), True);
      Clear(List);
      for Module of PlayerShip.Modules loop
         Append(List, ListIter);
         Set(List, ListIter, 0, To_String(Module.Name));
      end loop;
      List := Gtk_List_Store(Get_Object(Builder, "shipcrewinfolist"));
      Clear(List);
      for Member of PlayerShip.Crew loop
         Append(List, ListIter);
         Set(List, ListIter, 0, To_String(Member.Name));
         Set(List, ListIter, 1, Gint(Member.Health));
         if Member.Tired - Member.Attributes(ConditionIndex)(1) > 0 then
            Set
              (List, ListIter, 2,
               Gint(Member.Tired - Member.Attributes(ConditionIndex)(1)));
         else
            Set(List, ListIter, 2, 0);
         end if;
         Set(List, ListIter, 3, Gint(Member.Thirst));
         Set(List, ListIter, 4, Gint(Member.Hunger));
         Set(List, ListIter, 5, Gint(Member.Morale(1)));
         Set(List, ListIter, 6, To_Lower(Crew_Orders'Image(Member.Order)));
         if Member.Tired - Member.Attributes(ConditionIndex)(1) > 0 then
            Visible(1) := True;
         end if;
         if Member.Thirst > 0 then
            Visible(2) := True;
         end if;
         if Member.Hunger > 0 then
            Visible(3) := True;
         end if;
         if Member.Morale(1) /= 50 then
            Visible(4) := True;
         end if;
      end loop;
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")), "ship");
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treemodules")),
         Gtk_Tree_Path_New_From_String("0"), null, False);
      UpdateMessages;
      ShowShipInfo;
      if PlayerShip.Crew(1).Health = 0 then
         Hide(Gtk_Widget(Get_Object(Builder, "expmoduleoptions")));
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "edtname")), False);
      end if;
      for I in Visible'Range loop
         Set_Visible
           (Gtk_Tree_View_Column
              (Get_Object(Builder, To_String(ColumnNames(I)))),
            Visible(I));
      end loop;
   end ShowShipUI;

end Ships.UI;
