--    Copyright 2018 Bartek thindil Jasicki
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Label; use Gtk.Label;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Window; use Gtk.Window;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Button; use Gtk.Button;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Progress_Bar; use Gtk.Progress_Bar;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Glib.Object; use Glib.Object;
with Maps; use Maps;
with Messages; use Messages;
with ShipModules; use ShipModules;
with Crafts; use Crafts;
with Help.UI; use Help.UI;
with Ships.Upgrade; use Ships.Upgrade;
with Ships.Crew; use Ships.Crew;

package body Ships.UI is

   Builder: Gtkada_Builder;
   ModuleIndex: Positive;

   procedure ShowAssignMember is
      AssignIter: Gtk_Tree_Iter;
      AssignList: Gtk_List_Store;
   begin
      AssignList := Gtk_List_Store(Get_Object(Builder, "assigncrewlist"));
      Clear(AssignList);
      for I in PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
         if PlayerShip.Modules(ModuleIndex).Owner /= I and
           PlayerShip.Crew(I).Skills.Length > 0 then
            case Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
              .MType is
               when MEDICAL_ROOM =>
                  if PlayerShip.Crew(I).Health = 100 then
                     Append(AssignList, AssignIter);
                     Set
                       (AssignList,
                        AssignIter,
                        0,
                        To_String(PlayerShip.Crew(I).Name));
                     Set(AssignList, AssignIter, 1, Gint(I));
                  end if;
               when others =>
                  Append(AssignList, AssignIter);
                  Set
                    (AssignList,
                     AssignIter,
                     0,
                     To_String(PlayerShip.Crew(I).Name));
                  Set(AssignList, AssignIter, 1, Gint(I));
            end case;
         end if;
      end loop;
      Set_Active(Gtk_Combo_Box(Get_Object(Builder, "cmbassigncrew")), 0);
   end ShowAssignMember;

   procedure ShowAssignAmmo is
      AssignIter: Gtk_Tree_Iter;
      AssignList: Gtk_List_Store;
      HaveAmmo: Boolean := False;
   begin
      AssignList := Gtk_List_Store(Get_Object(Builder, "assignammolist"));
      Clear(AssignList);
      for I in PlayerShip.Cargo.First_Index .. PlayerShip.Cargo.Last_Index loop
         if Items_List(PlayerShip.Cargo(I).ProtoIndex).IType =
           Items_Types
             (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                .Value) and
           I /= PlayerShip.Modules(ModuleIndex).Data(1) then
            Append(AssignList, AssignIter);
            Set
              (AssignList,
               AssignIter,
               0,
               To_String(Items_List(PlayerShip.Cargo(I).ProtoIndex).Name));
            Set(AssignList, AssignIter, 1, Gint(I));
            HaveAmmo := True;
         end if;
      end loop;
      if not HaveAmmo then
         Hide(Gtk_Widget(Get_Object(Builder, "boxassignammo")));
         return;
      end if;
      Set_Active(Gtk_Combo_Box(Get_Object(Builder, "cmbassignammo")), 0);
   end ShowAssignAmmo;

   procedure ShowModuleOptions is
      MaxValue: Positive;
      IsPassenger: Boolean := False;
   begin
      Hide(Gtk_Widget(Get_Object(Builder, "btnupgrade2")));
      Hide(Gtk_Widget(Get_Object(Builder, "boxassigncrew")));
      Hide(Gtk_Widget(Get_Object(Builder, "boxassignammo")));
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
            if PlayerShip.Modules(ModuleIndex).Data(2) < MaxValue then
               Set_Label
                 (Gtk_Button(Get_Object(Builder, "btnupgrade1")),
                  "Upgrade engine power");
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
            if PlayerShip.Modules(ModuleIndex).Data(1) > MaxValue then
               Set_Label
                 (Gtk_Button(Get_Object(Builder, "btnupgrade2")),
                  "Reduce fuel usage");
               Show_All(Gtk_Widget(Get_Object(Builder, "btnupgrade2")));
            else
               Hide(Gtk_Widget(Get_Object(Builder, "btnupgrade2")));
            end if;
         when CABIN =>
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            if PlayerShip.Modules(ModuleIndex).Data(2) < MaxValue then
               Set_Label
                 (Gtk_Button(Get_Object(Builder, "btnupgrade1")),
                  "Upgrade quality");
               Show_All(Gtk_Widget(Get_Object(Builder, "btnupgrade1")));
            else
               Hide(Gtk_Widget(Get_Object(Builder, "btnupgrade1")));
            end if;
            for Mission of PlayerShip.Missions loop
               if Mission.MType = Passenger and
                 Mission.Target = PlayerShip.Modules(ModuleIndex).Owner then
                  IsPassenger := True;
                  exit;
               end if;
            end loop;
            if not IsPassenger then
               Set_Label
                 (Gtk_Button(Get_Object(Builder, "btnassigncrew")),
                  "Assign as owner");
               Show_All(Gtk_Widget(Get_Object(Builder, "boxassigncrew")));
            end if;
         when GUN | HARPOON_GUN =>
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            if PlayerShip.Modules(ModuleIndex).Data(2) < MaxValue then
               if Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                   .MType =
                 GUN then
                  Set_Label
                    (Gtk_Button(Get_Object(Builder, "btnupgrade1")),
                     "Upgrade damage");
               else
                  Set_Label
                    (Gtk_Button(Get_Object(Builder, "btnupgrade1")),
                     "Upgrade strength");
               end if;
               Show_All(Gtk_Widget(Get_Object(Builder, "btnupgrade1")));
            else
               Hide(Gtk_Widget(Get_Object(Builder, "btnupgrade1")));
            end if;
            Set_Label
              (Gtk_Button(Get_Object(Builder, "btnassigncrew")),
               "Assign as gunner");
            Show_All(Gtk_Widget(Get_Object(Builder, "boxassigncrew")));
            Show_All(Gtk_Widget(Get_Object(Builder, "boxassignammo")));
         when BATTERING_RAM =>
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            if PlayerShip.Modules(ModuleIndex).Data(2) < MaxValue then
               Set_Label
                 (Gtk_Button(Get_Object(Builder, "btnupgrade1")),
                  "Upgrade damage");
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
            if PlayerShip.Modules(ModuleIndex).Data(2) < MaxValue then
               Set_Label
                 (Gtk_Button(Get_Object(Builder, "btnupgrade1")),
                  "Enlarge hull");
               Show_All(Gtk_Widget(Get_Object(Builder, "btnupgrade1")));
            else
               Hide(Gtk_Widget(Get_Object(Builder, "btnupgrade1")));
            end if;
         when ALCHEMY_LAB .. GREENHOUSE =>
            if PlayerShip.Modules(ModuleIndex).Data(1) /= 0 then
               Set_Label
                 (Gtk_Button(Get_Object(Builder, "btnassigncrew")),
                  "Assign as worker");
               Show_All(Gtk_Widget(Get_Object(Builder, "boxassigncrew")));
            end if;
         when MEDICAL_ROOM =>
            for Member of PlayerShip.Crew loop
               if Member.Health < 100 and
                 FindItem
                     (Inventory => PlayerShip.Cargo,
                      ItemType => HealingTools) >
                   0 then
                  Set_Label
                    (Gtk_Button(Get_Object(Builder, "btnassigncrew")),
                     "Assign as medic");
                  Show_All(Gtk_Widget(Get_Object(Builder, "boxassigncrew")));
                  exit;
               end if;
            end loop;
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
   end ShowModuleOptions;

   procedure ShowModuleInfo(Object: access Gtkada_Builder_Record'Class) is
      ModulesIter: Gtk_Tree_Iter;
      ModulesModel: Gtk_Tree_Model;
      ModuleInfo: Unbounded_String;
      UpgradePercent: Natural;
      Module: ModuleData;
      MaxValue, MaxUpgrade: Positive;
      HaveAmmo: Boolean;
      Mamount: Natural := 0;
      DamagePercent: Gdouble;
      DamageBar: constant Gtk_Progress_Bar :=
        Gtk_Progress_Bar(Get_Object(Object, "damagebar"));
      CleanBar: constant GObject := Get_Object(Object, "cleanbar");
      QualityBar: constant GObject := Get_Object(Object, "qualitybar");
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treemodules"))),
         ModulesModel,
         ModulesIter);
      if ModulesIter = Null_Iter then
         return;
      end if;
      ModuleIndex :=
        Natural'Value(To_String(Get_Path(ModulesModel, ModulesIter))) + 1;
      Module := PlayerShip.Modules(ModuleIndex);
      if Module.Durability < Module.MaxDurability then
         DamagePercent :=
           1.0 - (Gdouble(Module.Durability) / Gdouble(Module.MaxDurability));
         if DamagePercent < 1.0 and DamagePercent > 0.79 then
            Set_Text(DamageBar, "Slightly damaged");
         elsif DamagePercent < 0.8 and DamagePercent > 0.49 then
            Set_Text(DamageBar, "Damaged");
         elsif DamagePercent < 0.5 and DamagePercent > 0.19 then
            Set_Text(DamageBar, "Heavily damaged");
         elsif DamagePercent < 0.2 and DamagePercent > 0.0 then
            Set_Text(DamageBar, "Almost destroyed");
         elsif DamagePercent = 0.0 then
            Set_Text(DamageBar, "Destroyed");
         end if;
      else
         Set_Text(DamageBar, "Not damaged");
         DamagePercent := 1.0;
      end if;
      Set_Fraction(DamageBar, DamagePercent);
      MaxValue :=
        Positive(Float(Modules_List(Module.ProtoIndex).Durability) * 1.5);
      if Module.MaxDurability = MaxValue then
         Set_Text(DamageBar, Get_Text(DamageBar) & " (max upgrade)");
      end if;
      ModuleInfo :=
        To_Unbounded_String("Weight:" & Integer'Image(Module.Weight) & " kg");
      Append(ModuleInfo, ASCII.LF & "Repair/Upgrade material: ");
      for Item of Items_List loop
         if Item.IType = Modules_List(Module.ProtoIndex).RepairMaterial then
            if Mamount > 0 then
               Append(ModuleInfo, " or ");
            end if;
            if FindItem
                (Inventory => PlayerShip.Cargo,
                 ItemType => Item.IType) =
              0 then
               Append
                 (ModuleInfo,
                  "<span foreground=""red"">" &
                  To_String(Item.Name) &
                  "</span>");
            else
               Append(ModuleInfo, To_String(Item.Name));
            end if;
            Mamount := Mamount + 1;
         end if;
      end loop;
      Append
        (ModuleInfo,
         ASCII.LF &
         "Repair/Upgrade skill: " &
         To_String
           (Skills_List(Modules_List(Module.ProtoIndex).RepairSkill).Name) &
         "/" &
         To_String
           (Attributes_Names
              (Skills_List(Modules_List(Module.ProtoIndex).RepairSkill)
                 .Attribute)));

      Set_Markup
        (Gtk_Label(Get_Object(Object, "lblmoduleinfo")),
         To_String(ModuleInfo));
      ModuleInfo := Null_Unbounded_String;
      Hide(Gtk_Widget(CleanBar));
      Hide(Gtk_Widget(QualityBar));
      case Modules_List(Module.ProtoIndex).MType is
         when ENGINE =>
            Append(ModuleInfo, "Max power:" & Integer'Image(Module.Data(2)));
            MaxValue :=
              Positive(Float(Modules_List(Module.ProtoIndex).MaxValue) * 1.5);
            if Module.Data(2) = MaxValue then
               Append(ModuleInfo, " (max upgrade)");
            end if;
            Append
              (ModuleInfo,
               ASCII.LF & "Fuel usage:" & Integer'Image(Module.Data(1)));
            MaxValue :=
              Positive(Float(Modules_List(Module.ProtoIndex).Value) / 2.0);
            if Module.Data(1) = MaxValue then
               Append(ModuleInfo, " (max upgrade)");
            end if;
         when ShipModules.CARGO =>
            Append
              (ModuleInfo,
               "Max cargo:" & Integer'Image(Module.Data(2)) & " kg");
         when HULL =>
            Append
              (ModuleInfo,
               "Modules space:" &
               Integer'Image(Module.Data(1)) &
               " /" &
               Integer'Image(Module.Data(2)));
            MaxValue :=
              Positive(Float(Modules_List(Module.ProtoIndex).MaxValue) * 1.5);
            if Module.Data(2) = MaxValue then
               Append(ModuleInfo, " (max upgrade)");
            end if;
         when CABIN =>
            if Module.Owner > 0 then
               Append
                 (ModuleInfo,
                  "Owner: " & To_String(PlayerShip.Crew(Module.Owner).Name));
            else
               Append(ModuleInfo, "Owner: none");
            end if;
            Show_All(Gtk_Widget(QualityBar));
            Set_Fraction
              (Gtk_Progress_Bar(QualityBar),
               Gdouble(Module.Data(2)) / 100.0);
            if Module.Data(2) < 30 then
               Set_Text(Gtk_Progress_Bar(QualityBar), "Minimal quality");
            elsif Module.Data(2) > 29 and Module.Data(2) < 60 then
               Set_Text(Gtk_Progress_Bar(QualityBar), "Basic quality");
            elsif Module.Data(2) > 59 and Module.Data(2) < 80 then
               Set_Text(Gtk_Progress_Bar(QualityBar), "Extended quality");
            else
               Set_Text(Gtk_Progress_Bar(QualityBar), "Luxury");
            end if;
            MaxValue :=
              Positive(Float(Modules_List(Module.ProtoIndex).MaxValue) * 1.5);
            if Module.Data(2) = MaxValue then
               Set_Text
                 (Gtk_Progress_Bar(QualityBar),
                  Get_Text(Gtk_Progress_Bar(QualityBar)) & " (max upgrade)");
            end if;
            Show_All(Gtk_Widget(CleanBar));
            if Module.Data(1) = Module.Data(2) then
               DamagePercent := 0.0;
               Set_Text(Gtk_Progress_Bar(CleanBar), "Clean");
            else
               DamagePercent :=
                 1.0 - (Gdouble(Module.Data(1)) / Gdouble(Module.Data(2)));
               if DamagePercent > 0.0 and DamagePercent < 0.2 then
                  Set_Text(Gtk_Progress_Bar(CleanBar), "Bit dusty");
               elsif DamagePercent > 0.19 and DamagePercent < 0.5 then
                  Set_Text(Gtk_Progress_Bar(CleanBar), "Dusty");
               elsif DamagePercent > 0.49 and DamagePercent < 0.8 then
                  Set_Text(Gtk_Progress_Bar(CleanBar), "Dirty");
               elsif DamagePercent > 0.79 and DamagePercent < 1.0 then
                  Set_Text(Gtk_Progress_Bar(CleanBar), "Very dirty");
               else
                  Set_Text(Gtk_Progress_Bar(CleanBar), "Ruined");
               end if;
            end if;
            Set_Fraction(Gtk_Progress_Bar(CleanBar), DamagePercent);
         when GUN | HARPOON_GUN =>
            Append(ModuleInfo, "Ammunition: ");
            if Module.Data(1) >= PlayerShip.Cargo.First_Index and
              Module.Data(1) <= PlayerShip.Cargo.Last_Index then
               if Items_List(PlayerShip.Cargo(Module.Data(1)).ProtoIndex)
                   .IType =
                 Items_Types(Modules_List(Module.ProtoIndex).Value) then
                  Append
                    (ModuleInfo,
                     To_String
                       (Items_List(PlayerShip.Cargo(Module.Data(1)).ProtoIndex)
                          .Name) &
                     " (assigned)");
                  HaveAmmo := True;
               end if;
            end if;
            if not HaveAmmo then
               Mamount := 0;
               for I in Items_List.Iterate loop
                  if Items_List(I).IType =
                    Items_Types(Modules_List(Module.ProtoIndex).Value) then
                     if Mamount > 0 then
                        Append(ModuleInfo, " or ");
                     end if;
                     if FindItem
                         (PlayerShip.Cargo,
                          Objects_Container.To_Index(I)) >
                       0 then
                        Append(ModuleInfo, To_String(Items_List(I).Name));
                     else
                        Append
                          (ModuleInfo,
                           "<span foreground=""red"">" &
                           To_String(Items_List(I).Name) &
                           "</span>");
                     end if;
                     Mamount := Mamount + 1;
                  end if;
               end loop;
            end if;
            Append(ModuleInfo, ASCII.LF);
            if Module.Owner > 0 then
               Append
                 (ModuleInfo,
                  "Gunner: " & To_String(PlayerShip.Crew(Module.Owner).Name));
            else
               Append(ModuleInfo, "Gunner: none");
            end if;
         when TURRET =>
            if Module.Data(1) > 0 then
               Append
                 (ModuleInfo,
                  "Weapon: " &
                  To_String(PlayerShip.Modules(Module.Data(1)).Name));
            else
               Append(ModuleInfo, "Weapon: none");
            end if;
         when ALCHEMY_LAB .. GREENHOUSE =>
            if Module.Owner > 0 then
               Append
                 (ModuleInfo,
                  "Worker: " & To_String(PlayerShip.Crew(Module.Owner).Name));
            else
               Append(ModuleInfo, "Worker: none");
            end if;
            Append(ModuleInfo, ASCII.LF);
            if Module.Data(1) /= 0 then
               if Module.Data(1) > 0 then
                  Append
                    (ModuleInfo,
                     "Manufacturing:" &
                     Positive'Image(Module.Data(3)) &
                     "x " &
                     To_String
                       (Items_List(Recipes_List(Module.Data(1)).ResultIndex)
                          .Name));
               else
                  Append
                    (ModuleInfo,
                     "Deconstructing " &
                     To_String(Items_List(abs (Module.Data(1))).Name));
               end if;
               Append
                 (ModuleInfo,
                  ASCII.LF &
                  "Time to complete current:" &
                  Positive'Image(Module.Data(2)) &
                  " mins");
            else
               Append(ModuleInfo, "Manufacturing: nothing");
            end if;
         when MEDICAL_ROOM =>
            if Module.Owner > 0 then
               Append
                 (ModuleInfo,
                  "Medic: " & To_String(PlayerShip.Crew(Module.Owner).Name));
            else
               Append(ModuleInfo, "Medic: none");
            end if;
         when others =>
            null;
      end case;
      if Modules_List(Module.ProtoIndex).Size > 0 then
         if ModuleInfo /= Null_Unbounded_String then
            Append(ModuleInfo, ASCII.LF);
         end if;
         Append
           (ModuleInfo,
            "Size:" & Natural'Image(Modules_List(Module.ProtoIndex).Size));
      end if;
      if Module.UpgradeAction /= NONE then
         Append(ModuleInfo, ASCII.LF & "Upgrading: ");
         case Module.UpgradeAction is
            when DURABILITY =>
               Append(ModuleInfo, "durability");
               MaxUpgrade := 10;
            when MAX_VALUE =>
               case Modules_List(Module.ProtoIndex).MType is
                  when ENGINE =>
                     Append(ModuleInfo, "power");
                     MaxUpgrade := 10;
                  when CABIN =>
                     Append(ModuleInfo, "quality");
                     MaxUpgrade := 100;
                  when GUN | BATTERING_RAM =>
                     Append(ModuleInfo, "damage");
                     MaxUpgrade := 100;
                  when HULL =>
                     Append(ModuleInfo, "enlarge");
                     MaxUpgrade := 500;
                  when others =>
                     null;
               end case;
            when VALUE =>
               case Modules_List(Module.ProtoIndex).MType is
                  when ENGINE =>
                     Append(ModuleInfo, "fuel usage");
                     MaxUpgrade := 100;
                  when others =>
                     null;
               end case;
            when others =>
               null;
         end case;
         Append(ModuleInfo, ASCII.LF & "Upgrade progress: ");
         UpgradePercent :=
           100 -
           Natural
             ((Float(Module.UpgradeProgress) / Float(MaxUpgrade)) * 100.0);
         if UpgradePercent < 11 then
            Append(ModuleInfo, "started");
         elsif UpgradePercent < 31 then
            Append(ModuleInfo, "designing");
         elsif UpgradePercent < 51 then
            Append(ModuleInfo, "base upgrades");
         elsif UpgradePercent < 80 then
            Append(ModuleInfo, "advanced upgrades");
         else
            Append(ModuleInfo, "final upgrades");
         end if;
      end if;
      if Modules_List(Module.ProtoIndex).Description /=
        Null_Unbounded_String then
         Append
           (ModuleInfo,
            ASCII.LF &
            ASCII.LF &
            To_String(Modules_List(Module.ProtoIndex).Description));
      end if;
      Set_Markup
        (Gtk_Label(Get_Object(Object, "lblmoduleinfo2")),
         To_String(ModuleInfo));
      ShowModuleOptions;
   end ShowModuleInfo;

   procedure ShowShipInfo is
      ShipInfo: Unbounded_String;
      UpgradePercent: Natural;
      MaxUpgrade: Positive;
   begin
      Set_Text
        (Gtk_Entry(Get_Object(Builder, "edtname")),
         To_String(PlayerShip.Name));
      ShipInfo :=
        To_Unbounded_String
          ("Home: " & To_String(SkyBases(PlayerShip.HomeBase).Name));
      Append(ShipInfo, ASCII.LF & "Upgrading: ");
      if PlayerShip.UpgradeModule = 0 then
         Append(ShipInfo, "Nothing");
      else
         Append
           (ShipInfo,
            To_String(PlayerShip.Modules(PlayerShip.UpgradeModule).Name) &
            " ");
         case PlayerShip.Modules(PlayerShip.UpgradeModule).UpgradeAction is
            when DURABILITY =>
               Append(ShipInfo, "(durability)");
               MaxUpgrade := 10;
            when MAX_VALUE =>
               case Modules_List
                 (PlayerShip.Modules(PlayerShip.UpgradeModule).ProtoIndex)
                 .MType is
                  when ENGINE =>
                     Append(ShipInfo, "(power)");
                     MaxUpgrade := 10;
                  when CABIN =>
                     Append(ShipInfo, "(quality)");
                     MaxUpgrade := 100;
                  when GUN | BATTERING_RAM =>
                     Append(ShipInfo, "(damage)");
                     MaxUpgrade := 100;
                  when HULL =>
                     Append(ShipInfo, "(enlarge)");
                     MaxUpgrade := 500;
                  when HARPOON_GUN =>
                     Append(ShipInfo, "(strength)");
                     MaxUpgrade := 100;
                  when others =>
                     null;
               end case;
            when VALUE =>
               case Modules_List
                 (PlayerShip.Modules(PlayerShip.UpgradeModule).ProtoIndex)
                 .MType is
                  when ENGINE =>
                     Append(ShipInfo, "(fuel usage)");
                     MaxUpgrade := 100;
                  when others =>
                     null;
               end case;
            when others =>
               null;
         end case;
         Append(ShipInfo, ASCII.LF & "Upgrade progress: ");
         UpgradePercent :=
           100 -
           Natural
             ((Float
                 (PlayerShip.Modules(PlayerShip.UpgradeModule)
                    .UpgradeProgress) /
               Float(MaxUpgrade)) *
              100.0);
         if UpgradePercent < 11 then
            Append(ShipInfo, "started");
         elsif UpgradePercent < 31 then
            Append(ShipInfo, "designing");
         elsif UpgradePercent < 51 then
            Append(ShipInfo, "base upgrades");
         elsif UpgradePercent < 80 then
            Append(ShipInfo, "advanced upgrades");
         else
            Append(ShipInfo, "final upgrades");
         end if;
      end if;
      Append(ShipInfo, ASCII.LF & "Repair first: ");
      if PlayerShip.RepairModule = 0 then
         Append(ShipInfo, "Any module");
      else
         Append
           (ShipInfo,
            To_String(PlayerShip.Modules(PlayerShip.RepairModule).Name));
      end if;
      Append(ShipInfo, ASCII.LF & "Destination: ");
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
               "X:" &
               Positive'Image(PlayerShip.DestinationX) &
               " Y:" &
               Positive'Image(PlayerShip.DestinationY));
         end if;
      end if;
      Append
        (ShipInfo,
         ASCII.LF &
         "Weight:" &
         Integer'Image(CountShipWeight(PlayerShip)) &
         "kg");
      Set_Label
        (Gtk_Label(Get_Object(Builder, "lblshipinfo")),
         To_String(ShipInfo));
   end ShowShipInfo;

   procedure ShowHelp(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
   begin
      ShowHelpUI(6);
   end ShowHelp;

   procedure ChangeShipName(Object: access Gtkada_Builder_Record'Class) is
      NewName: Unbounded_String :=
        To_Unbounded_String
          (Get_Text(Gtk_Entry(Get_Object(Object, "edtname"))));
      SemicolonIndex: Natural;
   begin
      if Length(NewName) = 0 then
         ShowDialog
           ("You must enter new ship name",
            Gtk_Window(Get_Object(Builder, "shipwindow")));
         return;
      end if;
      SemicolonIndex := Index(NewName, ";");
      while SemicolonIndex > 0 loop
         Delete(NewName, SemicolonIndex, SemicolonIndex);
         SemicolonIndex := Index(NewName, ";");
      end loop;
      PlayerShip.Name := NewName;
   end ChangeShipName;

   procedure ChangeModuleName
     (Self: access Gtk_Cell_Renderer_Text_Record'Class;
      Path: UTF8_String;
      New_Text: UTF8_String) is
      pragma Unreferenced(Self);
      ModulesList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "moduleslist"));
      NewName: Unbounded_String := To_Unbounded_String(New_Text);
      SemicolonIndex: Natural;
   begin
      if Length(NewName) = 0 then
         ShowDialog
           ("You must enter new module name",
            Gtk_Window(Get_Object(Builder, "shipwindow")));
         return;
      end if;
      SemicolonIndex := Index(NewName, ";");
      while SemicolonIndex > 0 loop
         Delete(NewName, SemicolonIndex, SemicolonIndex);
         SemicolonIndex := Index(NewName, ";");
      end loop;
      PlayerShip.Modules(ModuleIndex).Name := NewName;
      Set
        (ModulesList,
         Get_Iter_From_String(ModulesList, Path),
         0,
         To_String(NewName));
   end ChangeModuleName;

   procedure SetUpgrade(User_Data: access GObject_Record'Class) is
      UpgradeType: Positive;
   begin
      if User_Data = Get_Object(Builder, "btnupgradedur") then
         UpgradeType := 1;
      elsif User_Data = Get_Object(Builder, "btnupgrade1") then
         UpgradeType := 2;
      elsif User_Data = Get_Object(Builder, "btnupgrade2") then
         UpgradeType := 3;
      else
         UpgradeType := 4;
      end if;
      StartUpgrading(ModuleIndex, UpgradeType);
      UpdateOrders(PlayerShip);
      ShowLastMessage(Builder);
      ShowShipInfo;
      ShowModuleInfo(Builder);
   exception
      when An_Exception : Ship_Upgrade_Error =>
         ShowDialog
           (Exception_Message(An_Exception),
            Gtk_Window(Get_Object(Builder, "shipwindow")));
         return;
   end SetUpgrade;

   procedure StopUpgrading(Object: access Gtkada_Builder_Record'Class) is
   begin
      PlayerShip.UpgradeModule := 0;
      for I in PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
         if PlayerShip.Crew(I).Order = Upgrading then
            GiveOrders(PlayerShip, I, Rest);
            exit;
         end if;
      end loop;
      AddMessage("You stopped current upgrade.", OrderMessage);
      ShowLastMessage(Object);
      ShowShipInfo;
      ShowModuleInfo(Object);
   end StopUpgrading;

   procedure SetRepair(User_Data: access GObject_Record'Class) is
   begin
      if User_Data = Get_Object(Builder, "btnrepairfirst") then
         PlayerShip.RepairModule := ModuleIndex;
         AddMessage
           ("You assigned " &
            To_String(PlayerShip.Modules(ModuleIndex).Name) &
            " as repair priority.",
            OrderMessage);
      else
         PlayerShip.RepairModule := 0;
         AddMessage("You removed repair priority.", OrderMessage);
      end if;
      ShowLastMessage(Builder);
      ShowShipInfo;
      ShowModuleInfo(Builder);
   end SetRepair;

   procedure Assign(User_Data: access GObject_Record'Class) is
      ActiveIndex: Natural;
      AssignList: Gtk_List_Store;
      AssignIndex: Positive;
   begin
      if User_Data = Get_Object(Builder, "btnassigncrew") then
         ActiveIndex :=
           Natural
             (Get_Active(Gtk_Combo_Box(Get_Object(Builder, "cmbassigncrew"))));
         AssignList := Gtk_List_Store(Get_Object(Builder, "assigncrewlist"));
      else
         ActiveIndex :=
           Natural
             (Get_Active(Gtk_Combo_Box(Get_Object(Builder, "cmbassignammo"))));
         AssignList := Gtk_List_Store(Get_Object(Builder, "assignammolist"));
      end if;
      AssignIndex :=
        Positive
          (Get_Int
             (AssignList,
              Get_Iter_From_String(AssignList, Natural'Image(ActiveIndex)),
              1));
      if User_Data = Get_Object(Builder, "btnassigncrew") then
         case Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex).MType is
            when CABIN =>
               for I in PlayerShip.Modules.Iterate loop
                  if PlayerShip.Modules(I).Owner = AssignIndex and
                    Modules_List(PlayerShip.Modules(I).ProtoIndex).MType =
                      CABIN then
                     PlayerShip.Modules(I).Owner := 0;
                  end if;
               end loop;
               PlayerShip.Modules(ModuleIndex).Owner := AssignIndex;
               AddMessage
                 ("You assigned " &
                  To_String(PlayerShip.Modules(ModuleIndex).Name) &
                  " to " &
                  To_String(PlayerShip.Crew(AssignIndex).Name) &
                  ".",
                  OrderMessage);
            when GUN =>
               GiveOrders(PlayerShip, AssignIndex, Gunner, ModuleIndex);
            when ALCHEMY_LAB .. GREENHOUSE =>
               GiveOrders(PlayerShip, AssignIndex, Craft, ModuleIndex);
            when MEDICAL_ROOM =>
               GiveOrders(PlayerShip, AssignIndex, Heal, ModuleIndex);
            when others =>
               null;
         end case;
      else
         PlayerShip.Modules(ModuleIndex).Data(1) := AssignIndex;
         AddMessage
           ("You assigned " &
            To_String
              (Items_List(PlayerShip.Cargo(AssignIndex).ProtoIndex).Name) &
            " to " &
            To_String(PlayerShip.Modules(ModuleIndex).Name) &
            ".",
            OrderMessage);
      end if;
      ShowLastMessage(Builder);
      ShowShipInfo;
      ShowModuleInfo(Builder);
   end Assign;

   procedure CreateShipUI is
      Error: aliased GError;
   begin
      if Builder /= null then
         return;
      end if;
      Gtk_New(Builder);
      if Add_From_File
          (Builder,
           To_String(DataDirectory) & "ui" & Dir_Separator & "ships.glade",
           Error'Access) =
        Guint(0) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      Register_Handler(Builder, "Hide_Ship_Info", HideShipInfo'Access);
      Register_Handler(Builder, "Hide_Last_Message", HideLastMessage'Access);
      Register_Handler(Builder, "Show_Module_Info", ShowModuleInfo'Access);
      Register_Handler(Builder, "Show_Help", ShowHelp'Access);
      Register_Handler(Builder, "Change_Ship_Name", ChangeShipName'Access);
      Register_Handler(Builder, "Set_Upgrade", SetUpgrade'Access);
      Register_Handler(Builder, "Stop_Upgrading", StopUpgrading'Access);
      Register_Handler(Builder, "Set_Repair", SetRepair'Access);
      Register_Handler(Builder, "Assign", Assign'Access);
      Do_Connect(Builder);
      On_Edited
        (Gtk_Cell_Renderer_Text(Get_Object(Builder, "rendername")),
         ChangeModuleName'Access);
      On_Key_Release_Event
        (Gtk_Widget(Get_Object(Builder, "shipwindow")),
         CloseWindow'Access);
   end CreateShipUI;

   procedure ShowShipUI(OldState: GameStates) is
      ModulesIter: Gtk_Tree_Iter;
      ModulesList: Gtk_List_Store;
   begin
      PreviousGameState := OldState;
      ModulesList := Gtk_List_Store(Get_Object(Builder, "moduleslist"));
      Clear(ModulesList);
      for Module of PlayerShip.Modules loop
         Append(ModulesList, ModulesIter);
         Set(ModulesList, ModulesIter, 0, To_String(Module.Name));
      end loop;
      Show_All(Gtk_Widget(Get_Object(Builder, "shipwindow")));
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treemodules")),
         Gtk_Tree_Path_New_From_String("0"),
         Gtk_Tree_View_Column(Get_Object(Builder, "columnmodule")),
         False);
      ShowLastMessage(Builder);
      ShowShipInfo;
   end ShowShipUI;

end Ships.UI;
