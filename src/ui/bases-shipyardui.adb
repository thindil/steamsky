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

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Label; use Gtk.Label;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Progress_Bar; use Gtk.Progress_Bar;
with Gtk.Stack; use Gtk.Stack;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Gtkada.Builder; use Gtkada.Builder;
with Game; use Game;
with ShipModules; use ShipModules;
with Ships; use Ships;
with Ships.Crew; use Ships.Crew;
with Items; use Items;
with Bases.Ship; use Bases.Ship;
with Utils.UI; use Utils.UI;
with Trades; use Trades;
with Maps; use Maps;
with Maps.UI; use Maps.UI;

package body Bases.ShipyardUI is

   -- ****iv* Bases.ShipyardUI/ModuleIndex
   -- SOURCE
   ModuleIndex: Unbounded_String;
   -- ****

   -- ****if* Bases.ShipyardUI/GetModuleInfo
   -- FUNCTION
   -- Show information about selected module
   -- PARAMETERS
   -- ModuleInfo - String which contains whole info about the module
   -- Installing - If true, player looking at installing modules list
   -- RESULT
   -- Parameter ModuleInfo
   -- SOURCE
   procedure GetModuleInfo
     (ModuleInfo: in out Unbounded_String; Installing: Boolean) is
      -- ****
      MType: ModuleType;
      MAmount, Weight, MaxValue, Value, MaxOwners: Natural;
      ShipModuleIndex, Size: Positive;
      Speed: Integer;
   begin
      if Installing then
         MType := Modules_List(ModuleIndex).MType;
         MaxValue := Modules_List(ModuleIndex).MaxValue;
         Value := Modules_List(ModuleIndex).Value;
         Size := Modules_List(ModuleIndex).Size;
         Weight := Modules_List(ModuleIndex).Weight;
         MaxOwners := Modules_List(ModuleIndex).MaxOwners;
         Speed := Modules_List(ModuleIndex).Speed;
      else
         ShipModuleIndex := Integer'Value(To_String(ModuleIndex));
         MType :=
           Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex).MType;
         case MType is
            when HARPOON_GUN =>
               MaxValue := PlayerShip.Modules(ShipModuleIndex).Duration;
               Value :=
                 Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
                   .Value;
            when ENGINE =>
               MaxValue := PlayerShip.Modules(ShipModuleIndex).Power;
               Value := PlayerShip.Modules(ShipModuleIndex).FuelUsage;
            when CABIN =>
               MaxValue := PlayerShip.Modules(ShipModuleIndex).Quality;
               Value := PlayerShip.Modules(ShipModuleIndex).Cleanliness;
            when GUN =>
               MaxValue := PlayerShip.Modules(ShipModuleIndex).Damage;
               Value :=
                 Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
                   .Value;
            when ShipModules.CARGO =>
               MaxValue :=
                 Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
                   .MaxValue;
               Value :=
                 Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
                   .Value;
            when HULL =>
               MaxValue := PlayerShip.Modules(ShipModuleIndex).MaxModules;
               Value :=
                 Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
                   .Value;
            when BATTERING_RAM =>
               MaxValue := PlayerShip.Modules(ShipModuleIndex).Damage2;
               Value := 0;
            when others =>
               MaxValue := 0;
               Value := 0;
         end case;
         Size :=
           Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex).Size;
         Weight :=
           Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex).Weight;
         MaxOwners :=
           Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
             .MaxOwners;
         Speed :=
           Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex).Speed;
      end if;
      case MType is
         when HULL =>
            if Installing then
               Append(ModuleInfo, LF & "Ship hull can be only replaced.");
               Append
                 (ModuleInfo,
                  LF & "Modules space:" & Positive'Image(MaxValue));
            end if;
            Append(ModuleInfo, LF & "Max module size:" & Integer'Image(Value));
         when ENGINE =>
            Append(ModuleInfo, LF & "Max power:" & Positive'Image(MaxValue));
            if Installing then
               Append(ModuleInfo, LF & "Fuel usage:" & Positive'Image(Value));
            end if;
         when ShipModules.CARGO =>
            Append
              (ModuleInfo,
               LF & "Max cargo:" & Positive'Image(MaxValue) & " kg");
         when CABIN =>
            Append(ModuleInfo, LF & "Quality: ");
            if MaxValue < 30 then
               Append(ModuleInfo, "minimal");
            elsif MaxValue < 60 then
               Append(ModuleInfo, "basic");
            elsif MaxValue < 80 then
               Append(ModuleInfo, "extended");
            else
               Append(ModuleInfo, "luxury");
            end if;
            Append(ModuleInfo, LF & "Max owners:" & Natural'Image(MaxOwners));
         when ALCHEMY_LAB .. GREENHOUSE =>
            Append(ModuleInfo, LF & "Max workers:" & Natural'Image(MaxOwners));
         when GUN | HARPOON_GUN =>
            Append(ModuleInfo, LF & "Strength:" & Natural'Image(MaxValue));
            Append(ModuleInfo, LF & "Ammunition: ");
            MAmount := 0;
            for Item of Items_List loop
               if Item.IType = Items_Types(Value) then
                  if MAmount > 0 then
                     Append(ModuleInfo, " or ");
                  end if;
                  Append(ModuleInfo, Item.Name);
                  MAmount := MAmount + 1;
               end if;
            end loop;
            if MType = GUN then
               Append(ModuleInfo, LF);
               if Speed > 0 then
                  Append
                    (ModuleInfo,
                     "Max fire rate:" & Positive'Image(Speed) & "/round");
               else
                  Append
                    (ModuleInfo,
                     "Max fire rate: 1/" &
                     Trim(Integer'Image(abs (Speed)), Both) & " rounds");
               end if;
            end if;
         when BATTERING_RAM =>
            Append(ModuleInfo, LF & "Strength:" & Natural'Image(MaxValue));
         when others =>
            null;
      end case;
      if MType not in HULL | ARMOR then
         Append(ModuleInfo, LF & "Size:" & Natural'Image(Size));
         if Installing then
            for Module of PlayerShip.Modules loop
               if Module.MType = HULL
                 and then Size > Modules_List(Module.ProtoIndex).Value then
                  Append
                    (ModuleInfo, " <span foreground=""red"">(too big)</span>");
                  exit;
               end if;
            end loop;
         end if;
      end if;
      if Weight > 0 then
         Append(ModuleInfo, LF & "Weight:" & Natural'Image(Weight) & " kg");
      end if;
      if Installing then
         Append(ModuleInfo, LF & "Repair/Upgrade material: ");
         MAmount := 0;
         for Item of Items_List loop
            if Item.IType = Modules_List(ModuleIndex).RepairMaterial then
               if MAmount > 0 then
                  Append(ModuleInfo, " or ");
               end if;
               Append(ModuleInfo, Item.Name);
               MAmount := MAmount + 1;
            end if;
         end loop;
         Append
           (ModuleInfo,
            LF & "Repair/Upgrade skill: " &
            To_String
              (Skills_List(Modules_List(ModuleIndex).RepairSkill).Name) &
            "/" &
            To_String
              (Attributes_List
                 (Skills_List(Modules_List(ModuleIndex).RepairSkill).Attribute)
                 .Name));
         if Modules_List(ModuleIndex).Description /= Null_Unbounded_String then
            Append
              (ModuleInfo, LF & LF & Modules_List(ModuleIndex).Description);
         end if;
      end if;
   end GetModuleInfo;

   -- ****if* Bases.ShipyardUI/ShowInstallInfo
   -- FUNCTION
   -- Show information about selected module to install
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure ShowInstallInfo(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      ModuleInfo, InstallInfo: Unbounded_String;
      Cost: Positive;
      MoneyIndex2, UsedSpace, AllSpace, MaxSize: Natural;
   begin
      declare
         ModulesIter: Gtk_Tree_Iter;
         ModulesModel: Gtk_Tree_Model;
      begin
         Get_Selected
           (Gtk.Tree_View.Get_Selection
              (Gtk_Tree_View(Get_Object(Object, "treeinstall"))),
            ModulesModel, ModulesIter);
         if ModulesIter = Null_Iter then
            return;
         end if;
         ModuleIndex :=
           To_Unbounded_String(Get_String(ModulesModel, ModulesIter, 1));
      end;
      Cost := Modules_List(ModuleIndex).Price;
      CountPrice(Cost, FindMember(Talk));
      MoneyIndex2 := FindItem(PlayerShip.Cargo, MoneyIndex);
      ModuleInfo := To_Unbounded_String("Install cost:");
      if MoneyIndex2 = 0
        or else PlayerShip.Cargo(MoneyIndex2).Amount < Cost then
         Append
           (ModuleInfo,
            "<span foreground=""red"">" & Positive'Image(Cost) & " " &
            To_String(MoneyName) & "</span> ");
      else
         Append(ModuleInfo, Positive'Image(Cost) & " " & To_String(MoneyName));
      end if;
      Append
        (ModuleInfo,
         LF & "Installation time:" &
         Positive'Image(Modules_List(ModuleIndex).InstallTime) & " minutes");
      GetModuleInfo(ModuleInfo, True);
      Set_Label
        (Gtk_Label(Get_Object(Object, "lblinstallinfo")),
         To_String(ModuleInfo));
      if MoneyIndex2 > 0 then
         InstallInfo :=
           To_Unbounded_String
             (LF & "You have" &
              Natural'Image(PlayerShip.Cargo(MoneyIndex2).Amount) & " " &
              To_String(MoneyName) & ".");
      else
         InstallInfo :=
           To_Unbounded_String
             (LF & "You don't have any " & To_String(MoneyName) &
              " to install anything.");
      end if;
      for Module of PlayerShip.Modules loop
         if Module.MType = HULL then
            UsedSpace := Module.InstalledModules;
            AllSpace := Module.MaxModules;
            MaxSize := Modules_List(Module.ProtoIndex).Value;
            Append
              (InstallInfo,
               LF & "You have used" & Natural'Image(UsedSpace) &
               " modules space from max" & Natural'Image(AllSpace) &
               " allowed.");
            exit;
         end if;
      end loop;
      Set_Label
        (Gtk_Label(Get_Object(Object, "lblmoneyinstall")),
         To_String(InstallInfo));
      if MoneyIndex2 = 0 then
         Set_Sensitive(Gtk_Widget(Get_Object(Object, "btninstall")), False);
      else
         if PlayerShip.Cargo(MoneyIndex2).Amount < Cost or
           ((Modules_List(ModuleIndex).MType not in GUN | HARPOON_GUN |
                 HULL) and
            ((AllSpace - UsedSpace) < Modules_List(ModuleIndex).Size or
             Modules_List(ModuleIndex).Size > MaxSize)) or
           (Modules_List(ModuleIndex).MType = HULL and
            Modules_List(ModuleIndex).MaxValue < UsedSpace) then
            Set_Sensitive(Gtk_Widget(Get_Object(Object, "btninstall")), False);
         else
            Set_Sensitive(Gtk_Widget(Get_Object(Object, "btninstall")), True);
         end if;
      end if;
   end ShowInstallInfo;

   -- ****if* Bases.ShipyardUI/ShowRemoveInfo
   -- FUNCTION
   -- Show information about selected module to remove
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure ShowRemoveInfo(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      ModuleInfo, RemoveInfo: Unbounded_String;
      Cost: Natural;
      Damage: Gdouble;
      DamageBar: constant GObject := Get_Object(Object, "removedamagebar");
      ShipModuleIndex: Natural;
   begin
      declare
         ModulesIter: Gtk_Tree_Iter;
         ModulesModel: Gtk_Tree_Model;
      begin
         Get_Selected
           (Gtk.Tree_View.Get_Selection
              (Gtk_Tree_View(Get_Object(Object, "treeremove"))),
            ModulesModel, ModulesIter);
         if ModulesIter = Null_Iter then
            return;
         end if;
         ShipModuleIndex := Natural(Get_Int(ModulesModel, ModulesIter, 1));
         ModuleIndex := To_Unbounded_String(Integer'Image(ShipModuleIndex));
      end;
      if ShipModuleIndex > Positive(PlayerShip.Modules.Length) then
         return;
      end if;
      Damage :=
        1.0 -
        Gdouble
          (Gdouble(PlayerShip.Modules(ShipModuleIndex).Durability) /
           Gdouble(PlayerShip.Modules(ShipModuleIndex).MaxDurability));
      Cost :=
        Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex).Price -
        Integer
          (Float
             (Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
                .Price) *
           Float(Damage));
      if Cost = 0 then
         Cost := 1;
      end if;
      CountPrice(Cost, FindMember(Talk), False);
      ModuleInfo := To_Unbounded_String("Remove gain:" & Positive'Image(Cost));
      Append
        (ModuleInfo,
         LF & "Removing time:" &
         Positive'Image
           (Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
              .InstallTime) &
         " minutes");
      GetModuleInfo(ModuleInfo, False);
      Set_Label
        (Gtk_Label(Get_Object(Object, "lblremoveinfo")),
         To_String(ModuleInfo));
      if Damage = 0.0 then
         Hide(Gtk_Widget(DamageBar));
      else
         Show_All(Gtk_Widget(DamageBar));
         Set_Fraction(Gtk_Progress_Bar(DamageBar), Damage);
         if Damage < 0.2 then
            Set_Text(Gtk_Progress_Bar(DamageBar), "Slightly damaged");
         elsif Damage < 0.5 then
            Set_Text(Gtk_Progress_Bar(DamageBar), "Damaged");
         elsif Damage < 0.8 then
            Set_Text(Gtk_Progress_Bar(DamageBar), "Heavily damaged");
         elsif Damage < 1.0 then
            Set_Text(Gtk_Progress_Bar(DamageBar), "Almost destroyed");
         else
            Set_Text(Gtk_Progress_Bar(DamageBar), "Destroyed");
         end if;
      end if;
      if Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
          .Description /=
        Null_Unbounded_String then
         Set_Label
           (Gtk_Label(Get_Object(Object, "lblremovedescription")),
            LF &
            To_String
              (Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
                 .Description));
      end if;
      declare
         MoneyIndex2: constant Natural :=
           FindItem(PlayerShip.Cargo, MoneyIndex);
      begin
         if MoneyIndex2 > 0 then
            RemoveInfo :=
              To_Unbounded_String
                (LF & "You have" &
                 Natural'Image(PlayerShip.Cargo(MoneyIndex2).Amount) & " " &
                 To_String(MoneyName) & ".");
         else
            RemoveInfo :=
              To_Unbounded_String
                (LF & "You don't have any " & To_String(MoneyName) &
                 " to install anything.");
         end if;
      end;
      for Module of PlayerShip.Modules loop
         if Module.MType = HULL then
            Append
              (RemoveInfo,
               LF & "You have used" & Natural'Image(Module.InstalledModules) &
               " modules space from max" & Natural'Image(Module.MaxModules) &
               " allowed.");
            exit;
         end if;
      end loop;
      Set_Label
        (Gtk_Label(Get_Object(Object, "lblmoneyremove")),
         To_String(RemoveInfo));
   end ShowRemoveInfo;

   -- ****if* Bases.ShipyardUI/GetModuleType
   -- FUNCTION
   -- Get type of selected module
   -- PARAMETERS
   -- ModuleIndex - Index of module in prototypes list
   -- RETURNS
   -- Formatted type of module
   -- SOURCE
   function GetModuleType(ModuleIndex: Unbounded_String) return String is
      -- ****
      ModuleTypeName: Unbounded_String :=
        To_Unbounded_String
          (To_Lower(ModuleType'Image(Modules_List(ModuleIndex).MType)));
   begin
      Replace_Element(ModuleTypeName, 1, To_Upper(Element(ModuleTypeName, 1)));
      while Index(ModuleTypeName, "_", 1) > 0 loop
         Replace_Element(ModuleTypeName, Index(ModuleTypeName, "_", 1), ' ');
      end loop;
      return To_String(ModuleTypeName);
   end GetModuleType;

   -- ****if* Bases.ShipyardUI/SetRemoveModulesList
   -- FUNCTION
   -- Fill remove modules list with player ship modules
   -- SOURCE
   procedure SetRemoveModulesList is
      -- ****
      ModulesList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "removemodulelist"));
      ModulesIter: Gtk_Tree_Iter;
   begin
      Clear(ModulesList);
      for I in PlayerShip.Modules.Iterate loop
         if Modules_List(PlayerShip.Modules(I).ProtoIndex).MType /= HULL then
            Append(ModulesList, ModulesIter);
            Set
              (ModulesList, ModulesIter, 0,
               To_String(PlayerShip.Modules(I).Name));
            Set
              (ModulesList, ModulesIter, 1,
               Gint(Modules_Container.To_Index(I)));
            Set
              (ModulesList, ModulesIter, 2,
               GetModuleType(PlayerShip.Modules(I).ProtoIndex));
            Set
              (ModulesList, ModulesIter, 3,
               Gint(Modules_List(PlayerShip.Modules(I).ProtoIndex).Size));
            Set
              (ModulesList, ModulesIter, 4,
               To_String
                 (Modules_List(PlayerShip.Modules(I).ProtoIndex)
                    .RepairMaterial));
         end if;
      end loop;
   end SetRemoveModulesList;

   -- ****if* Bases.ShipyardUI/ManipulateModule
   -- FUNCTION
   -- Install or remove selected module
   -- PARAMETERS
   -- User_Data - Button clicked
   -- SOURCE
   procedure ManipulateModule(User_Data: access GObject_Record'Class) is
      -- ****
      Install: Boolean;
      ModulesIter: Gtk_Tree_Iter;
      ModulesModel: Gtk_Tree_Model;
   begin
      if User_Data = Get_Object(Builder, "btninstall") then
         Install := True;
      else
         Get_Selected
           (Gtk.Tree_View.Get_Selection
              (Gtk_Tree_View(Get_Object(Builder, "treeremove"))),
            ModulesModel, ModulesIter);
         Install := False;
      end if;
      Bases.Ship.UpgradeShip(Install, ModuleIndex);
      SetRemoveModulesList;
      ShowInstallInfo(Builder);
      UpdateMessages;
   exception
      when Trade_No_Money =>
         ShowDialog
           ("You don't have " & To_String(MoneyName) & " to pay for modules.");
      when An_Exception : Trade_Not_Enough_Money =>
         ShowDialog
           ("You don't have enough " & To_String(MoneyName) & " to pay for " &
            Exception_Message(An_Exception) & ".");
      when An_Exception : BasesShip_Unique_Module =>
         ShowDialog
           ("You can't install another " & Exception_Message(An_Exception) &
            " because you have installed one module that type. Remove old first.");
      when An_Exception : BasesShip_Installation_Error |
        BasesShip_Removing_Error =>
         ShowDialog(Exception_Message(An_Exception));
      when Trade_No_Free_Cargo =>
         ShowDialog
           ("You don't have enough free space for " & To_String(MoneyName) &
            " in ship cargo.");
      when Trade_No_Money_In_Base =>
         ShowDialog
           ("Base don't have enough " & To_String(MoneyName) &
            " for buy this module.");
   end ManipulateModule;

   -- ****if* Bases.ShipyardUI/SearchShipyard
   -- FUNCTION
   -- Search module to install by its name
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure SearchShipyard(Object: access Gtkada_Builder_Record'Class) is
   -- ****
   begin
      Refilter
        (Gtk_Tree_Model_Filter(Get_Object(Object, "installmodulesfilter")));
      if N_Children
          (Gtk_List_Store(Get_Object(Builder, "installmoduleslist")),
           Null_Iter) >
        0 then
         Set_Cursor
           (Gtk_Tree_View(Get_Object(Builder, "treeinstall")),
            Gtk_Tree_Path_New_From_String("0"), null, False);
      end if;
   end SearchShipyard;

   -- ****if* Bases.ShipyardUI/VisibleShipyard
   -- FUNCTION
   -- Check if selected module is visible on the list
   -- PARAMETERS
   -- Model - Gtk_Tree_Model with modules which will be checked
   -- Iter  - Gtk_Tree_Iter of module which will be checked
   -- RESULT
   -- True if module should be visible, otherwise false
   -- SOURCE
   function VisibleShipyard
     (Model: Gtk_Tree_Model; Iter: Gtk_Tree_Iter) return Boolean is
      -- ****
      SearchEntry: constant Gtk_GEntry :=
        Gtk_GEntry(Get_Object(Builder, "shipyardsearch"));
      MType: constant ModuleType :=
        ModuleType'Val
          (Natural
             (Get_Active(Gtk_Combo_Box(Get_Object(Builder, "cmbtypes")))));
      ShowModule: Boolean := False;
   begin
      if MType = ANY then
         ShowModule := True;
      else
         if Modules_List(To_Unbounded_String(Get_String(Model, Iter, 1)))
             .Price >
           0 and
           Modules_List(To_Unbounded_String(Get_String(Model, Iter, 1)))
               .MType =
             MType then
            ShowModule := True;
         end if;
      end if;
      if Get_Text(SearchEntry) = "" then
         return ShowModule;
      end if;
      if Index
          (To_Lower(Get_String(Model, Iter, 0)),
           To_Lower(Get_Text(SearchEntry)), 1) >
        0 and
        ShowModule then
         return True;
      end if;
      return False;
   end VisibleShipyard;

   procedure CreateBasesShipyardUI is
   begin
      Register_Handler(Builder, "Show_Install_Info", ShowInstallInfo'Access);
      Register_Handler(Builder, "Manipulate_Module", ManipulateModule'Access);
      Register_Handler(Builder, "Show_Remove_Info", ShowRemoveInfo'Access);
      Register_Handler(Builder, "Search_Shipyard", SearchShipyard'Access);
      Set_Visible_Func
        (Gtk_Tree_Model_Filter(Get_Object(Builder, "installmodulesfilter")),
         VisibleShipyard'Access);
      On_Key_Press_Event
        (Gtk_Widget(Get_Object(Builder, "shipyardsearch")),
         SelectElement'Access, Get_Object(Builder, "btnmenu"));
   end CreateBasesShipyardUI;

   procedure ShowShipyardUI is
      ModulesList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "installmoduleslist"));
      ModulesIter: Gtk_Tree_Iter;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
   begin
      SetRemoveModulesList;
      ModulesList.Clear;
      for I in Modules_List.Iterate loop
         if Modules_List(I).Price > 0 and
           SkyBases(BaseIndex).Reputation(1) >= Modules_List(I).Reputation then
            Append(ModulesList, ModulesIter);
            Set(ModulesList, ModulesIter, 0, To_String(Modules_List(I).Name));
            Set
              (ModulesList, ModulesIter, 1,
               To_String(BaseModules_Container.Key(I)));
            Set
              (ModulesList, ModulesIter, 2,
               GetModuleType(BaseModules_Container.Key(I)));
            case Modules_List(I).MType is
               when HULL =>
                  Set
                    (ModulesList, ModulesIter, 3,
                     Gint(Modules_List(I).MaxValue));
               when others =>
                  Set(ModulesList, ModulesIter, 3, Gint(Modules_List(I).Size));
            end case;
            Set
              (ModulesList, ModulesIter, 4,
               To_String(Modules_List(I).RepairMaterial));
         end if;
      end loop;
      Set_Active(Gtk_Combo_Box(Get_Object(Builder, "cmbtypes")), 0);
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")), "shipyard");
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treeinstall")),
         Gtk_Tree_Path_New_From_String("0"), null, False);
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treeremove")),
         Gtk_Tree_Path_New_From_String("0"), null, False);
      UpdateMessages;
   end ShowShipyardUI;

end Bases.ShipyardUI;
