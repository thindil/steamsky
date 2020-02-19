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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Exceptions; use Ada.Exceptions;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Label; use Gtk.Label;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Progress_Bar; use Gtk.Progress_Bar;
with Messages; use Messages;
with ShipModules; use ShipModules;
with Crafts; use Crafts;
with Ships.Upgrade; use Ships.Upgrade;
with Ships.Crew; use Ships.Crew;
with Game.SaveLoad; use Game.SaveLoad;
with Utils.UI; use Utils.UI;
with Config; use Config;
with Maps.UI; use Maps.UI;

package body Ships.UI.Handlers is

   procedure ShowModuleInfo(Object: access Gtkada_Builder_Record'Class) is
      ModuleInfo: Unbounded_String;
      Module: ModuleData;
      MaxValue: Positive;
      HaveAmmo: Boolean;
      Mamount, MaxUpgrade: Natural := 0;
      DamagePercent, UpgradePercent: Gdouble;
      CleanBar: constant GObject := Get_Object(Object, "cleanbar");
      QualityBar: constant GObject := Get_Object(Object, "qualitybar");
      UpgradeBar: constant GObject := Get_Object(Object, "upgradebar2");
      procedure AddOwnersInfo(OwnersName: String) is
         HaveOwner: Boolean := False;
      begin
         Append(ModuleInfo, OwnersName);
         if Module.Owner.Length > 1 then
            Append(ModuleInfo, "s");
         end if;
         Append
           (ModuleInfo,
            " (max" & Count_Type'Image(Module.Owner.Length) & "): ");
         for I in Module.Owner.First_Index .. Module.Owner.Last_Index loop
            if Module.Owner(I) > 0 then
               if HaveOwner then
                  Append(ModuleInfo, ", ");
               end if;
               HaveOwner := True;
               Append
                 (ModuleInfo,
                  To_String(PlayerShip.Crew(Module.Owner(I)).Name));
            end if;
         end loop;
         if not HaveOwner then
            Append(ModuleInfo, "none");
         end if;
      end AddOwnersInfo;
   begin
      declare
         ModulesIter: Gtk_Tree_Iter;
         ModulesModel: Gtk_Tree_Model;
      begin
         Get_Selected
           (Gtk.Tree_View.Get_Selection
              (Gtk_Tree_View(Get_Object(Object, "treemodules"))),
            ModulesModel, ModulesIter);
         if ModulesIter = Null_Iter then
            return;
         end if;
         ModuleIndex :=
           Natural'Value(To_String(Get_Path(ModulesModel, ModulesIter))) + 1;
      end;
      Module := PlayerShip.Modules(ModuleIndex);
      declare
         DamageBar: constant Gtk_Progress_Bar :=
           Gtk_Progress_Bar(Get_Object(Object, "moduledamagebar"));
      begin
         Hide(Gtk_Widget(DamageBar));
         if Module.Durability < Module.MaxDurability then
            Show_All(Gtk_Widget(DamageBar));
            DamagePercent :=
              (Gdouble(Module.Durability) / Gdouble(Module.MaxDurability));
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
            Set_Fraction(DamageBar, DamagePercent);
         end if;
         MaxValue :=
           Positive(Float(Modules_List(Module.ProtoIndex).Durability) * 1.5);
         if Module.MaxDurability = MaxValue then
            Set_Text(DamageBar, Get_Text(DamageBar) & " (max upgrade)");
         end if;
      end;
      ModuleInfo :=
        To_Unbounded_String("Weight:" & Integer'Image(Module.Weight) & " kg");
      Append(ModuleInfo, LF & "Repair/Upgrade material: ");
      for Item of Items_List loop
         if Item.IType = Modules_List(Module.ProtoIndex).RepairMaterial then
            if Mamount > 0 then
               Append(ModuleInfo, " or ");
            end if;
            if FindItem
                (Inventory => PlayerShip.Cargo, ItemType => Item.IType) =
              0 then
               Append
                 (ModuleInfo,
                  "<span foreground=""red"">" & To_String(Item.Name) &
                  "</span>");
            else
               Append(ModuleInfo, To_String(Item.Name));
            end if;
            Mamount := Mamount + 1;
         end if;
      end loop;
      Append
        (ModuleInfo,
         LF & "Repair/Upgrade skill: " &
         To_String
           (Skills_List(Modules_List(Module.ProtoIndex).RepairSkill).Name) &
         "/" &
         To_String
           (Attributes_List
              (Skills_List(Modules_List(Module.ProtoIndex).RepairSkill)
                 .Attribute)
              .Name));
      Set_Markup
        (Gtk_Label(Get_Object(Object, "lblmoduleinfo")),
         To_String(ModuleInfo));
      ModuleInfo := Null_Unbounded_String;
      Hide(Gtk_Widget(CleanBar));
      Hide(Gtk_Widget(QualityBar));
      case Module.MType is
         when ENGINE =>
            Append(ModuleInfo, "Max power:" & Integer'Image(Module.Power));
            MaxValue :=
              Positive(Float(Modules_List(Module.ProtoIndex).MaxValue) * 1.5);
            if Module.Power = MaxValue then
               Append(ModuleInfo, " (max upgrade)");
            end if;
            if Module.Disabled then
               Append(ModuleInfo, " (disabled)");
            end if;
            Append
              (ModuleInfo,
               LF & "Fuel usage:" & Integer'Image(Module.FuelUsage));
            MaxValue :=
              Positive(Float(Modules_List(Module.ProtoIndex).Value) / 2.0);
            if Module.FuelUsage = MaxValue then
               Append(ModuleInfo, " (max upgrade)");
            end if;
         when CARGO_ROOM =>
            Append
              (ModuleInfo,
               "Max cargo:" &
               Integer'Image(Modules_List(Module.ProtoIndex).MaxValue) &
               " kg");
         when HULL =>
            Show_All(Gtk_Widget(CleanBar));
            DamagePercent :=
              Gdouble(Module.InstalledModules) / Gdouble(Module.MaxModules);
            Set_Fraction(Gtk_Progress_Bar(CleanBar), DamagePercent);
            Set_Text
              (Gtk_Progress_Bar(CleanBar),
               "Modules installed:" & Integer'Image(Module.InstalledModules) &
               " /" & Integer'Image(Module.MaxModules));
            MaxValue :=
              Positive(Float(Modules_List(Module.ProtoIndex).MaxValue) * 1.5);
            if Module.MaxModules = MaxValue then
               Set_Text
                 (Gtk_Progress_Bar(CleanBar),
                  Get_Text(Gtk_Progress_Bar(CleanBar)) & " (max upgrade)");
            end if;
         when CABIN =>
            AddOwnersInfo("Owner");
            Show_All(Gtk_Widget(QualityBar));
            Set_Fraction
              (Gtk_Progress_Bar(QualityBar), Gdouble(Module.Quality) / 100.0);
            Set_Text
              (Gtk_Progress_Bar(QualityBar), GetCabinQuality(Module.Quality));
            MaxValue :=
              Positive(Float(Modules_List(Module.ProtoIndex).MaxValue) * 1.5);
            if Module.Quality = MaxValue then
               Set_Text
                 (Gtk_Progress_Bar(QualityBar),
                  Get_Text(Gtk_Progress_Bar(QualityBar)) & " (max upgrade)");
            end if;
            if Module.Cleanliness /= Module.Quality then
               Show_All(Gtk_Widget(CleanBar));
               DamagePercent :=
                 1.0 - (Gdouble(Module.Cleanliness) / Gdouble(Module.Quality));
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
               Set_Fraction(Gtk_Progress_Bar(CleanBar), DamagePercent);
            end if;
         when GUN | HARPOON_GUN =>
            Append(ModuleInfo, "Strength:");
            if Modules_List(Module.ProtoIndex).MType = GUN then
               Append(ModuleInfo, Positive'Image(Module.Damage));
            else
               Append(ModuleInfo, Positive'Image(Module.Duration));
            end if;
            Append(ModuleInfo, LF & "Ammunition: ");
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
                  Append
                    (ModuleInfo,
                     To_String
                       (Items_List(PlayerShip.Cargo(AmmoIndex).ProtoIndex)
                          .Name) &
                     " (assigned)");
                  HaveAmmo := True;
               end if;
            end;
            if not HaveAmmo then
               Mamount := 0;
               for I in Items_List.Iterate loop
                  if Items_List(I).IType =
                    Items_Types(Modules_List(Module.ProtoIndex).Value) then
                     if Mamount > 0 then
                        Append(ModuleInfo, " or ");
                     end if;
                     if FindItem(PlayerShip.Cargo, Objects_Container.Key(I)) >
                       0 then
                        Append(ModuleInfo, To_String(Items_List(I).Name));
                     else
                        Append
                          (ModuleInfo,
                           "<span foreground=""red"">" &
                           To_String(Items_List(I).Name) & "</span>");
                     end if;
                     Mamount := Mamount + 1;
                  end if;
               end loop;
            end if;
            Append(ModuleInfo, LF);
            if Module.Owner(1) > 0 then
               Append
                 (ModuleInfo,
                  "Gunner: " &
                  To_String(PlayerShip.Crew(Module.Owner(1)).Name));
            else
               Append(ModuleInfo, "Gunner: none");
            end if;
            if Module.MType = GUN then
               Append(ModuleInfo, LF);
               if Modules_List(Module.ProtoIndex).Speed > 0 then
                  Append
                    (ModuleInfo,
                     "Max fire rate:" &
                     Positive'Image(Modules_List(Module.ProtoIndex).Speed) &
                     "/round");
               else
                  Append
                    (ModuleInfo,
                     "Max fire rate: 1/" &
                     Trim
                       (Integer'Image
                          (abs (Modules_List(Module.ProtoIndex).Speed)),
                        Both) &
                     " rounds");
               end if;
            end if;
         when TURRET =>
            if Module.GunIndex > 0 then
               Append
                 (ModuleInfo,
                  "Weapon: " &
                  To_String(PlayerShip.Modules(Module.GunIndex).Name));
            else
               Append(ModuleInfo, "Weapon: none");
            end if;
         when WORKSHOP =>
            AddOwnersInfo("Worker");
            Append(ModuleInfo, LF);
            if Module.CraftingIndex /= Null_Unbounded_String then
               if Length(Module.CraftingIndex) > 6
                 and then Slice(Module.CraftingIndex, 1, 5) = "Study" then
                  Append
                    (ModuleInfo,
                     "Studying " &
                     To_String
                       (Items_List
                          (Unbounded_Slice
                             (Module.CraftingIndex, 7,
                              Length(Module.CraftingIndex)))
                          .Name));
               elsif Length(Module.CraftingIndex) > 12
                 and then Slice(Module.CraftingIndex, 1, 11) =
                   "Deconstruct" then
                  Append
                    (ModuleInfo,
                     "Deconstructing " &
                     To_String
                       (Items_List
                          (Unbounded_Slice
                             (Module.CraftingIndex, 13,
                              Length(Module.CraftingIndex)))
                          .Name));
               else
                  Append
                    (ModuleInfo,
                     "Manufacturing:" & Positive'Image(Module.CraftingAmount) &
                     "x " &
                     To_String
                       (Items_List
                          (Recipes_List(Module.CraftingIndex).ResultIndex)
                          .Name));
               end if;
               Append
                 (ModuleInfo,
                  LF & "Time to complete current:" &
                  Positive'Image(Module.CraftingTime) & " mins");
            else
               Append(ModuleInfo, "Manufacturing: nothing");
            end if;
         when MEDICAL_ROOM =>
            AddOwnersInfo("Medic");
         when TRAINING_ROOM =>
            if Module.TrainedSkill > 0 then
               Append
                 (ModuleInfo,
                  "Set for training " &
                  To_String(Skills_List(Module.TrainedSkill).Name) & ".");
            else
               Append(ModuleInfo, "Must be set for training.");
            end if;
            Append(ModuleInfo, LF);
            AddOwnersInfo("Trainee");
         when BATTERING_RAM =>
            Append(ModuleInfo, "Strength:");
            Append(ModuleInfo, Positive'Image(Module.Damage2));
         when others =>
            null;
      end case;
      if Modules_List(Module.ProtoIndex).Size > 0 then
         if ModuleInfo /= Null_Unbounded_String then
            Append(ModuleInfo, LF);
         end if;
         Append
           (ModuleInfo,
            "Size:" & Natural'Image(Modules_List(Module.ProtoIndex).Size));
      end if;
      if Modules_List(Module.ProtoIndex).Description /=
        Null_Unbounded_String then
         if ModuleInfo /= Null_Unbounded_String then
            Append(ModuleInfo, LF);
         end if;
         Append
           (ModuleInfo,
            LF & To_String(Modules_List(Module.ProtoIndex).Description));
      end if;
      Set_Markup
        (Gtk_Label(Get_Object(Object, "lblmoduleinfo2")),
         To_String(ModuleInfo));
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
           1.0 - (Gdouble(Module.UpgradeProgress) / Gdouble(MaxUpgrade));
         Set_Fraction(Gtk_Progress_Bar(UpgradeBar), UpgradePercent);
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
         Set_Text(Gtk_Progress_Bar(UpgradeBar), To_String(ModuleInfo));
         Show_All(Gtk_Widget(UpgradeBar));
      else
         Hide(Gtk_Widget(UpgradeBar));
      end if;
      ShowModuleOptions;
   end ShowModuleInfo;

   procedure ChangeShipName(Object: access Gtkada_Builder_Record'Class) is
      NewName: constant String :=
        Get_Text(Gtk_Entry(Get_Object(Object, "edtname")));
   begin
      if NewName'Length = 0 then
         ShowDialog("You must enter new ship name");
         return;
      end if;
      if To_Unbounded_String(NewName) /= PlayerShip.Name then
         PlayerShip.Name := To_Unbounded_String(NewName);
         GenerateSaveName(True);
      end if;
   end ChangeShipName;

   procedure ChangeModuleName
     (Self: access Gtk_Cell_Renderer_Text_Record'Class; Path: UTF8_String;
      New_Text: UTF8_String) is
      pragma Unreferenced(Self);
      ModulesList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "moduleslist"));
   begin
      if New_Text'Length = 0 then
         ShowDialog("You must enter new module name");
         return;
      end if;
      PlayerShip.Modules(ModuleIndex).Name := To_Unbounded_String(New_Text);
      Set(ModulesList, Get_Iter_From_String(ModulesList, Path), 0, New_Text);
   end ChangeModuleName;

   procedure SetUpgrade(User_Data: access GObject_Record'Class) is
   begin
      if User_Data = Get_Object(Builder, "btnupgradedur") then
         StartUpgrading(ModuleIndex, 1);
      elsif User_Data = Get_Object(Builder, "btnupgrade1") then
         StartUpgrading(ModuleIndex, 2);
      elsif User_Data = Get_Object(Builder, "btnupgrade2") then
         StartUpgrading(ModuleIndex, 3);
      else
         StartUpgrading(ModuleIndex, 4);
      end if;
      UpdateOrders(PlayerShip);
      UpdateMessages;
      ShowShipInfo;
      ShowModuleInfo(Builder);
   exception
      when An_Exception : Ship_Upgrade_Error =>
         ShowDialog(Exception_Message(An_Exception));
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
      UpdateMessages;
      ShowShipInfo;
      ShowModuleInfo(Object);
   end StopUpgrading;

   procedure SetRepair(User_Data: access GObject_Record'Class) is
   begin
      if User_Data = Get_Object(Builder, "btnrepairfirst") then
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
      ShowShipInfo;
      ShowModuleInfo(Builder);
   end SetRepair;

   procedure Assign(User_Data: access GObject_Record'Class) is
      AssignIndex: Positive;
   begin
      if User_Data = Get_Object(Builder, "btnassigncrew") then
         AssignIndex :=
           Positive'Value
             (Get_Active_Id
                (Gtk_Combo_Box(Get_Object(Builder, "cmbassigncrew"))));
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
               for Owner of PlayerShip.Modules(ModuleIndex).Owner loop
                  if Owner = 0 then
                     Owner := AssignIndex;
                     exit;
                  end if;
               end loop;
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
      elsif User_Data = Get_Object(Builder, "btnassignammo") then
         AssignIndex :=
           Positive'Value
             (Get_Active_Id
                (Gtk_Combo_Box(Get_Object(Builder, "cmbassignammo"))));
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
      elsif User_Data = Get_Object(Builder, "btnassignskill") then
         AssignIndex :=
           Positive'Value
             (Get_Active_Id
                (Gtk_Combo_Box(Get_Object(Builder, "cmbassignskill"))));
         PlayerShip.Modules(ModuleIndex).TrainedSkill := AssignIndex;
         AddMessage
           ("You prepared " & To_String(PlayerShip.Modules(ModuleIndex).Name) &
            " for training " & To_String(Skills_List(AssignIndex).Name) & ".",
            OrderMessage);
      end if;
      UpdateMessages;
      ShowShipInfo;
      ShowModuleInfo(Builder);
   end Assign;

   procedure DisableEngine(Object: access Gtkada_Builder_Record'Class) is
      CanDisable: Boolean := False;
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
            ShowDialog
              ("You can't disable this engine because it is your last working engine.");
            return;
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
      ShowShipInfo;
      ShowModuleInfo(Object);
   end DisableEngine;

   procedure ToggleSearch(Object: access Gtkada_Builder_Record'Class) is
      ModulesTree: constant Gtk_Tree_View :=
        Gtk_Tree_View(Get_Object(Object, "treemodules"));
   begin
      Set_Enable_Search(ModulesTree, not Get_Enable_Search(ModulesTree));
   end ToggleSearch;

end Ships.UI.Handlers;
