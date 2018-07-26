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

with Gtk.Widget; use Gtk.Widget;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Label; use Gtk.Label;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Button; use Gtk.Button;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Progress_Bar; use Gtk.Progress_Bar;
with Gtk.Window; use Gtk.Window;
with Gtk.Stack; use Gtk.Stack;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Maps; use Maps;
with ShipModules; use ShipModules;
with Ships.UI.Handlers; use Ships.UI.Handlers;
with Bases; use Bases;
with Missions; use Missions;

package body Ships.UI is

   procedure ShowAssignMember is
      AssignIter: Gtk_Tree_Iter;
      AssignList: Gtk_List_Store;
   begin
      AssignList := Gtk_List_Store(Get_Object(Builder, "assigncrewlist"));
      Clear(AssignList);
      for I in PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
         if PlayerShip.Modules(ModuleIndex).Owner /= I and
           PlayerShip.Crew(I).Skills.Length > 0 and
           PlayerShip.Crew(I).ContractLength /= 0 then
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
      procedure ShowAssignSkill is
         AssignIter: Gtk_Tree_Iter;
         AssignList: Gtk_List_Store;
      begin
         AssignList := Gtk_List_Store(Get_Object(Builder, "assignskilllist"));
         if N_Children(AssignList) > 0 then
            return;
         end if;
         for I in Skills_List.First_Index .. Skills_List.Last_Index loop
            Append(AssignList, AssignIter);
            Set(AssignList, AssignIter, 0, To_String(Skills_List(I).Name));
            Set(AssignList, AssignIter, 1, Gint(I));
         end loop;
         Set_Active(Gtk_Combo_Box(Get_Object(Builder, "cmbassignskill")), 0);
      end ShowAssignSkill;
   begin
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
            Show_All(Gtk_Widget(Get_Object(Builder, "btndisableengine")));
            if PlayerShip.Modules(ModuleIndex).Data(3) = 0 then
               Set_Label
                 (Gtk_Button
                    (Gtk_Widget(Get_Object(Builder, "btndisableengine"))),
                  "Disable engine");
            else
               Set_Label
                 (Gtk_Button
                    (Gtk_Widget(Get_Object(Builder, "btndisableengine"))),
                  "Enable engine");
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
            for Mission of AcceptedMissions loop
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
      MaxUpgrade: Positive;
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
               MaxUpgrade := 10;
            when MAX_VALUE =>
               case Modules_List
                 (PlayerShip.Modules(PlayerShip.UpgradeModule).ProtoIndex)
                 .MType is
                  when ENGINE =>
                     Append(UpgradeInfo, "(power)");
                     MaxUpgrade := 10;
                  when CABIN =>
                     Append(UpgradeInfo, "(quality)");
                     MaxUpgrade := 100;
                  when GUN | BATTERING_RAM =>
                     Append(UpgradeInfo, "(damage)");
                     MaxUpgrade := 100;
                  when HULL =>
                     Append(UpgradeInfo, "(enlarge)");
                     MaxUpgrade := 500;
                  when HARPOON_GUN =>
                     Append(UpgradeInfo, "(strength)");
                     MaxUpgrade := 100;
                  when others =>
                     null;
               end case;
            when VALUE =>
               case Modules_List
                 (PlayerShip.Modules(PlayerShip.UpgradeModule).ProtoIndex)
                 .MType is
                  when ENGINE =>
                     Append(UpgradeInfo, "(fuel usage)");
                     MaxUpgrade := 100;
                  when others =>
                     null;
               end case;
            when others =>
               null;
         end case;
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

   procedure CreateShipUI(NewBuilder: Gtkada_Builder) is
   begin
      Builder := NewBuilder;
      Register_Handler(Builder, "Show_Module_Info", ShowModuleInfo'Access);
      Register_Handler(Builder, "Change_Ship_Name", ChangeShipName'Access);
      Register_Handler(Builder, "Set_Upgrade", SetUpgrade'Access);
      Register_Handler(Builder, "Stop_Upgrading", StopUpgrading'Access);
      Register_Handler(Builder, "Set_Repair", SetRepair'Access);
      Register_Handler(Builder, "Assign", Assign'Access);
      Register_Handler(Builder, "Disable_Engine", DisableEngine'Access);
      On_Edited
        (Gtk_Cell_Renderer_Text(Get_Object(Builder, "rendername")),
         ChangeModuleName'Access);
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
      Show_All(Gtk_Widget(Get_Object(Builder, "btnshowhelp")));
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")),
         "ship");
      Set_Deletable(Gtk_Window(Get_Object(Builder, "skymapwindow")), False);
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treemodules")),
         Gtk_Tree_Path_New_From_String("0"),
         Gtk_Tree_View_Column(Get_Object(Builder, "columnmodule")),
         False);
      ShowLastMessage(Builder);
      ShowShipInfo;
   end ShowShipUI;

end Ships.UI;
