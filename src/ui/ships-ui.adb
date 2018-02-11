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
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Glib.Object; use Glib.Object;
with Gdk.RGBA; use Gdk.RGBA;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Combat.UI; use Combat.UI;
with Messages; use Messages;
with ShipModules; use ShipModules;
with Crafts; use Crafts;
with Help.UI; use Help.UI;
with Ships.Upgrade; use Ships.Upgrade;
with Ships.Crew; use Ships.Crew;

package body Ships.UI is

   Builder: Gtkada_Builder;
   GameState: GameStates;
   ModuleIndex: Positive;

   function HideShipInfo
     (Object: access Gtkada_Builder_Record'Class) return Boolean is
   begin
      Hide(Gtk_Widget(Get_Object(Object, "shipwindow")));
      case GameState is
         when SkyMap_View =>
            CreateSkyMap;
         when Combat_View =>
            ShowCombatUI;
      end case;
      return True;
   end HideShipInfo;

   procedure HideLastMessage(Object: access Gtkada_Builder_Record'Class) is
   begin
      Hide(Gtk_Widget(Get_Object(Object, "infolastmessage")));
      LastMessage := Null_Unbounded_String;
   end HideLastMessage;

   procedure ShowLastMessage is
   begin
      if LastMessage = Null_Unbounded_String then
         HideLastMessage(Builder);
      else
         Set_Text
           (Gtk_Label(Get_Object(Builder, "lbllastmessage")),
            To_String(LastMessage));
         Show_All(Gtk_Widget(Get_Object(Builder, "infolastmessage")));
         LastMessage := Null_Unbounded_String;
      end if;
   end ShowLastMessage;

   procedure ShowModuleInfo(Object: access Gtkada_Builder_Record'Class) is
      ModulesIter: Gtk_Tree_Iter;
      ModulesModel: Gtk_Tree_Model;
      ModuleInfo: Unbounded_String;
      DamagePercent, UpgradePercent: Natural;
      Module: ModuleData;
      MaxValue, MaxUpgrade: Positive;
      HaveAmmo: Boolean;
      Mamount: Natural := 0;
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
      ModuleInfo := To_Unbounded_String("Status: ");
      DamagePercent :=
        100 -
        Natural
          ((Float(Module.Durability) / Float(Module.MaxDurability)) * 100.0);
      if DamagePercent = 0 then
         Append(ModuleInfo, "Ok");
      elsif DamagePercent > 0 and DamagePercent < 20 then
         Append(ModuleInfo, "Slightly damaged");
      elsif DamagePercent > 19 and DamagePercent < 50 then
         Append(ModuleInfo, "<span foreground=""green"">Damaged</span>");
      elsif DamagePercent > 49 and DamagePercent < 80 then
         Append
           (ModuleInfo,
            "<span foreground=""yellow"">Heavily damaged</span>");
      elsif DamagePercent > 79 and DamagePercent < 100 then
         Append
           (ModuleInfo,
            "<span foreground=""red"">Almost destroyed</span>");
      else
         Append(ModuleInfo, "<span foreground=""blue"">Destroyed</span>");
      end if;
      MaxValue :=
        Positive(Float(Modules_List(Module.ProtoIndex).Durability) * 1.5);
      if Module.MaxDurability = MaxValue then
         Append(ModuleInfo, " (max upgrade)");
      end if;
      Append
        (ModuleInfo,
         ASCII.LF & "Weight:" & Integer'Image(Module.Weight) & " kg");
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
      Append(ModuleInfo, ASCII.LF);
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
            Append(ModuleInfo, ASCII.LF & "Quality: ");
            if Module.Data(2) < 30 then
               Append(ModuleInfo, "minimal");
            elsif Module.Data(2) > 29 and Module.Data(2) < 60 then
               Append(ModuleInfo, "basic");
            elsif Module.Data(2) > 59 and Module.Data(2) < 80 then
               Append(ModuleInfo, "extended");
            else
               Append(ModuleInfo, "luxury");
            end if;
            MaxValue :=
              Positive(Float(Modules_List(Module.ProtoIndex).MaxValue) * 1.5);
            if Module.Data(2) = MaxValue then
               Append(ModuleInfo, " (max upgrade)");
            end if;
            DamagePercent :=
              100 -
              Natural((Float(Module.Data(1)) / Float(Module.Data(2))) * 100.0);
            Append(ModuleInfo, ASCII.LF & "State: ");
            if DamagePercent = 0 then
               Append(ModuleInfo, "clean");
            elsif DamagePercent > 0 and DamagePercent < 20 then
               Append(ModuleInfo, "bit dusty");
            elsif DamagePercent > 19 and DamagePercent < 50 then
               Append(ModuleInfo, "dusty");
            elsif DamagePercent > 49 and DamagePercent < 80 then
               Append(ModuleInfo, "dirty");
            elsif DamagePercent > 79 and DamagePercent < 100 then
               Append(ModuleInfo, "very dirty");
            else
               Append(ModuleInfo, "ruined");
            end if;
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
         Append
           (ModuleInfo,
            ASCII.LF &
            "Size:" &
            Natural'Image(Modules_List(Module.ProtoIndex).Size));
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
        (Gtk_Label(Get_Object(Object, "lblmoduleinfo")),
         To_String(ModuleInfo));
   end ShowModuleInfo;

   procedure ShowShipInfo is
      ShipInfo: Unbounded_String;
      UpgradePercent: Natural;
      MaxUpgrade: Positive;
   begin
      Set_Label
        (Gtk_Label(Get_Object(Builder, "lblshipname")),
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

   procedure ShowChangeName(Object: access Gtkada_Builder_Record'Class) is
   begin
      Set_Text
        (Gtk_Entry(Get_Object(Object, "edtnewname")),
         To_String(PlayerShip.Name));
      Show_All(Gtk_Widget(Get_Object(Object, "changenamewindow")));
   end ShowChangeName;

   procedure ChangeShipName(Object: access Gtkada_Builder_Record'Class) is
      NewName: Unbounded_String :=
        To_Unbounded_String
          (Get_Text(Gtk_Entry(Get_Object(Object, "edtnewname"))));
      SemicolonIndex: Natural;
   begin
      if Length(NewName) = 0 then
         ShowDialog
           ("You must enter new ship name",
            Gtk_Window(Get_Object(Builder, "changenamewindow")));
         return;
      end if;
      SemicolonIndex := Index(NewName, ";");
      while SemicolonIndex > 0 loop
         Delete(NewName, SemicolonIndex, SemicolonIndex);
         SemicolonIndex := Index(NewName, ";");
      end loop;
      PlayerShip.Name := NewName;
      Set_Label
        (Gtk_Label(Get_Object(Builder, "lblshipname")),
         To_String(PlayerShip.Name));
      Hide(Gtk_Widget(Get_Object(Builder, "changenamewindow")));
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

   procedure ShowModuleOptions(Object: access Gtkada_Builder_Record'Class) is
      MaxValue: Positive;
      IsPassenger: Boolean := False;
   begin
      Show_All(Gtk_Widget(Get_Object(Object, "optionswindow")));
      Hide(Gtk_Widget(Get_Object(Object, "btnupgrade2")));
      Hide(Gtk_Widget(Get_Object(Object, "btnassigncrew")));
      Hide(Gtk_Widget(Get_Object(Object, "btnassignammo")));
      MaxValue :=
        Natural
          (Float
             (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                .Durability) *
           1.5);
      if PlayerShip.Modules(ModuleIndex).MaxDurability >= MaxValue then
         Hide(Gtk_Widget(Get_Object(Object, "btnupgradedur")));
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
                 (Gtk_Button(Get_Object(Object, "btnupgrade1")),
                  "Upgrade engine power");
            else
               Hide(Gtk_Widget(Get_Object(Object, "btnupgrade1")));
            end if;
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .Value) /
                 2.0);
            if PlayerShip.Modules(ModuleIndex).Data(1) > MaxValue then
               Set_Label
                 (Gtk_Button(Get_Object(Object, "btnupgrade2")),
                  "Reduce fuel usage");
            else
               Hide(Gtk_Widget(Get_Object(Object, "btnupgrade2")));
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
                 (Gtk_Button(Get_Object(Object, "btnupgrade1")),
                  "Upgrade quality");
            else
               Hide(Gtk_Widget(Get_Object(Object, "btnupgrade1")));
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
                 (Gtk_Button(Get_Object(Object, "btnassigncrew")),
                  "Assign owner");
               Show_All(Gtk_Widget(Get_Object(Object, "btnassigncrew")));
            else
               Hide(Gtk_Widget(Get_Object(Object, "btnassigncrew")));
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
                    (Gtk_Button(Get_Object(Object, "btnupgrade1")),
                     "Upgrade damage");
               else
                  Set_Label
                    (Gtk_Button(Get_Object(Object, "btnupgrade1")),
                     "Upgrade strength");
               end if;
            else
               Hide(Gtk_Widget(Get_Object(Object, "btnupgrade1")));
            end if;
            Set_Label
              (Gtk_Button(Get_Object(Object, "btnassigncrew")),
               "Assign gunner");
            Show_All(Gtk_Widget(Get_Object(Object, "btnassigncrew")));
            Show_All(Gtk_Widget(Get_Object(Object, "btnassignammo")));
         when BATTERING_RAM =>
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            if PlayerShip.Modules(ModuleIndex).Data(2) < MaxValue then
               Set_Label
                 (Gtk_Button(Get_Object(Object, "btnupgrade1")),
                  "Upgrade damage");
            else
               Hide(Gtk_Widget(Get_Object(Object, "btnupgrade1")));
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
                 (Gtk_Button(Get_Object(Object, "btnupgrade1")),
                  "Enlarge hull");
            else
               Hide(Gtk_Widget(Get_Object(Object, "btnupgrade1")));
            end if;
         when ALCHEMY_LAB .. GREENHOUSE =>
            if PlayerShip.Modules(ModuleIndex).Data(1) /= 0 then
               Set_Label
                 (Gtk_Button(Get_Object(Object, "btnassigncrew")),
                  "Assign worker");
               Show_All(Gtk_Widget(Get_Object(Object, "btnassigncrew")));
            end if;
         when MEDICAL_ROOM =>
            for Member of PlayerShip.Crew loop
               if Member.Health < 100 and
                 FindItem
                     (Inventory => PlayerShip.Cargo,
                      ItemType => HealingTools) >
                   0 then
                  Set_Label
                    (Gtk_Button(Get_Object(Object, "btnassigncrew")),
                     "Assign medic");
                  Show_All(Gtk_Widget(Get_Object(Object, "btnassigncrew")));
                  exit;
               end if;
            end loop;
         when others =>
            null;
      end case;
      if PlayerShip.Modules(ModuleIndex).UpgradeAction = NONE or
        PlayerShip.UpgradeModule = ModuleIndex then
         Hide(Gtk_Widget(Get_Object(Object, "btncontinue")));
      end if;
      if PlayerShip.UpgradeModule = 0 then
         Hide(Gtk_Widget(Get_Object(Object, "btnstop")));
      end if;
      if PlayerShip.RepairModule = ModuleIndex then
         Hide(Gtk_Widget(Get_Object(Object, "btnrepairfirst")));
      end if;
      if PlayerShip.RepairModule = 0 then
         Hide(Gtk_Widget(Get_Object(Object, "btnremovepriority")));
      end if;
   end ShowModuleOptions;

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
      Hide(Gtk_Widget(Get_Object(Builder, "optionswindow")));
      ShowLastMessage;
      ShowShipInfo;
      ShowModuleInfo(Builder);
   end SetUpgrade;

   procedure ShowAssignMember(Object: access Gtkada_Builder_Record'Class) is
   begin
      null;
   end ShowAssignMember;

   procedure ShowAssignAmmo(Object: access Gtkada_Builder_Record'Class) is
   begin
      null;
   end ShowAssignAmmo;

   procedure StopUpgrading(Object: access Gtkada_Builder_Record'Class) is
   begin
      PlayerShip.UpgradeModule := 0;
      for I in PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
         if PlayerShip.Crew(I).Order = Upgrading then
            GiveOrders(PlayerShip, I, Rest);
            exit;
         end if;
      end loop;
      Hide(Gtk_Widget(Get_Object(Object, "optionswindow")));
      AddMessage("You stopped current upgrade.", OrderMessage);
      ShowLastMessage;
      ShowShipInfo;
      ShowModuleInfo(Object);
   end StopUpgrading;

   procedure SetRepair(User_Data: access GObject_Record'Class) is
   begin
      null;
   end SetRepair;

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
      Override_Background_Color
        (Gtk_Widget(Get_Object(Builder, "lblmoduleinfo")),
         0,
         Black_RGBA);
      Override_Color
        (Gtk_Widget(Get_Object(Builder, "lblmoduleinfo")),
         0,
         White_RGBA);
      Register_Handler(Builder, "Hide_Ship_Info", HideShipInfo'Access);
      Register_Handler(Builder, "Hide_Last_Message", HideLastMessage'Access);
      Register_Handler(Builder, "Show_Module_Info", ShowModuleInfo'Access);
      Register_Handler(Builder, "Show_Help", ShowHelp'Access);
      Register_Handler(Builder, "Hide_Window", HideWindow'Access);
      Register_Handler(Builder, "Show_Change_Name", ShowChangeName'Access);
      Register_Handler(Builder, "Change_Ship_Name", ChangeShipName'Access);
      Register_Handler
        (Builder,
         "Show_Module_Options",
         ShowModuleOptions'Access);
      Register_Handler(Builder, "Set_Upgrade", SetUpgrade'Access);
      Register_Handler(Builder, "Show_Assign_Member", ShowAssignMember'Access);
      Register_Handler(Builder, "Show_Assign_Ammo", ShowAssignAmmo'Access);
      Register_Handler(Builder, "Stop_Upgrading", StopUpgrading'Access);
      Register_Handler(Builder, "Set_Repair", SetRepair'Access);
      Do_Connect(Builder);
      On_Edited
        (Gtk_Cell_Renderer_Text(Get_Object(Builder, "rendername")),
         ChangeModuleName'Access);
   end CreateShipUI;

   procedure ShowShipUI(OldState: GameStates) is
      ModulesIter: Gtk_Tree_Iter;
      ModulesList: Gtk_List_Store;
   begin
      GameState := OldState;
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
      ShowLastMessage;
      ShowShipInfo;
   end ShowShipUI;

end Ships.UI;
