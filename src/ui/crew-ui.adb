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

with Gtk.Widget; use Gtk.Widget;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Label; use Gtk.Label;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Combo; use Gtk.Cell_Renderer_Combo;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Cell_Renderer_Text;
with Gtk.Stack; use Gtk.Stack;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Glib.Types; use Glib.Types;
with Glib.Properties; use Glib.Properties;
with Gtkada.Builder; use Gtkada.Builder;
with Game; use Game;
with ShipModules; use ShipModules;
with Crew.Inventory; use Crew.Inventory;
with Crew.UI.Handlers; use Crew.UI.Handlers;
with Utils.UI; use Utils.UI;
with Maps.UI; use Maps.UI;

package body Crew.UI is

   procedure SetOrdersList is
      OrdersList: Gtk_List_Store;
      OrdersIter: Gtk_Tree_Iter;
      NeedClean, NeedRepair: Boolean := True;
      procedure AddOrder
        (OrderText: String; OrderIndex, ModuleIndex: Natural) is
      begin
         Append(OrdersList, OrdersIter);
         Set(OrdersList, OrdersIter, 0, OrderText);
         Set(OrdersList, OrdersIter, 1, Gint(OrderIndex));
         Set(OrdersList, OrdersIter, 2, Gint(ModuleIndex));
      end AddOrder;
      function IsWorking(Owners: Natural_Container.Vector) return Boolean is
      begin
         for Owner of Owners loop
            if Owner = MemberIndex then
               return True;
            end if;
         end loop;
         return False;
      end IsWorking;
   begin
      declare
         OrdersModel: constant Glib.Types.GType_Interface :=
           Get_Property
             (Get_Object(Builder, "renderorders1"),
              Gtk.Cell_Renderer_Combo.Model_Property);
      begin
         OrdersList := -(Gtk_Tree_Model(OrdersModel));
      end;
      OrdersList.Clear;
      if
        ((PlayerShip.Crew(MemberIndex).Tired = 100 or
          PlayerShip.Crew(MemberIndex).Hunger = 100 or
          PlayerShip.Crew(MemberIndex).Thirst = 100) and
         PlayerShip.Crew(MemberIndex).Order /= Rest) or
        (PlayerShip.Crew(MemberIndex).Skills.Length = 0 or
         PlayerShip.Crew(MemberIndex).ContractLength = 0) then
         AddOrder("Go on break", 9, 0);
      else
         if PlayerShip.Crew(MemberIndex).Order /= Pilot then
            AddOrder("Piloting", 0, 0);
         end if;
         if PlayerShip.Crew(MemberIndex).Order /= Engineer then
            AddOrder("Engineering", 1, 0);
         end if;
         for I in PlayerShip.Modules.Iterate loop
            if PlayerShip.Modules(I).Durability > 0 then
               case Modules_List(PlayerShip.Modules(I).ProtoIndex).MType is
                  when GUN | HARPOON_GUN =>
                     if PlayerShip.Modules(I).Owner(1) /= MemberIndex then
                        AddOrder
                          ("Operate " & To_String(PlayerShip.Modules(I).Name),
                           2, Modules_Container.To_Index(I));
                     end if;
                  when ALCHEMY_LAB .. GREENHOUSE =>
                     if not IsWorking(PlayerShip.Modules(I).Owner) then
                        AddOrder
                          ("Work in " & To_String(PlayerShip.Modules(I).Name),
                           4, Modules_Container.To_Index(I));
                     end if;
                  when CABIN =>
                     if PlayerShip.Modules(I).Cleanliness <
                       PlayerShip.Modules(I).Quality and
                       PlayerShip.Crew(MemberIndex).Order /= Clean and
                       NeedClean then
                        AddOrder("Clean ship", 8, 0);
                        NeedClean := False;
                     end if;
                  when TRAINING_ROOM =>
                     if not IsWorking(PlayerShip.Modules(I).Owner) then
                        AddOrder
                          ("Go on training in " &
                           To_String(PlayerShip.Modules(I).Name),
                           12, Modules_Container.To_Index(I));
                     end if;
                  when others =>
                     null;
               end case;
               if PlayerShip.Modules(I).Durability <
                 PlayerShip.Modules(I).MaxDurability and
                 NeedRepair then
                  AddOrder("Repair ship", 3, 0);
                  NeedRepair := False;
               end if;
            end if;
         end loop;
         for I in PlayerShip.Crew.Iterate loop
            if PlayerShip.Crew(I).Health < 100 and
              Crew_Container.To_Index(I) /= MemberIndex and
              PlayerShip.Crew(MemberIndex).Order /= Heal then
               AddOrder("Heal wounded crew members", 7, 0);
               exit;
            end if;
         end loop;
         if PlayerShip.UpgradeModule > 0 and
           PlayerShip.Crew(MemberIndex).Order /= Upgrading then
            AddOrder("Upgrade module", 5, 0);
         end if;
         if PlayerShip.Crew(MemberIndex).Order /= Talk then
            AddOrder("Talking in bases", 6, 0);
         end if;
         if PlayerShip.Crew(MemberIndex).Order /= Rest then
            AddOrder("Go on break", 9, 0);
         end if;
      end if;
   end SetOrdersList;

   procedure ShowOrdersForAll is
      NeedCleaning, NeedRepair: Boolean := False;
   begin
      for Module of PlayerShip.Modules loop
         if Module.Durability < Module.MaxDurability then
            NeedRepair := True;
         end if;
         if (Module.Durability > 0 and Module.MType = CABIN)
           and then Module.Cleanliness < Module.Quality then
            NeedCleaning := True;
         end if;
         exit when NeedCleaning and NeedRepair;
      end loop;
      if NeedRepair then
         Show_All(Gtk_Widget(Get_Object(Builder, "btnrepairall")));
      else
         Hide(Gtk_Widget(Get_Object(Builder, "btnrepairall")));
      end if;
      if NeedCleaning then
         Show_All(Gtk_Widget(Get_Object(Builder, "btnclearall")));
      else
         Hide(Gtk_Widget(Get_Object(Builder, "btnclearall")));
      end if;
   end ShowOrdersForAll;

   procedure RefreshInventory is
      InventoryIter: Gtk_Tree_Iter;
      InventoryList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "inventorylist"));
      ItemType, ProtoIndex: Unbounded_String;
      Visible: Boolean := False;
      ItemWeight: Positive;
   begin
      Clear(InventoryList);
      for I in
        PlayerShip.Crew(MemberIndex).Inventory.First_Index ..
          PlayerShip.Crew(MemberIndex).Inventory.Last_Index loop
         Append(InventoryList, InventoryIter);
         Set
           (InventoryList, InventoryIter, 0,
            GetItemName(PlayerShip.Crew(MemberIndex).Inventory(I), False));
         Set(InventoryList, InventoryIter, 1, Gint(I));
         if ItemIsUsed(MemberIndex, I) then
            Set(InventoryList, InventoryIter, 2, True);
         else
            Set(InventoryList, InventoryIter, 2, False);
         end if;
         ProtoIndex := PlayerShip.Crew(MemberIndex).Inventory(I).ProtoIndex;
         ItemType := Items_List(ProtoIndex).IType;
         if ItemType = WeaponType or ItemType = ShieldType or
           ItemType = HeadArmor or ItemType = ChestArmor or
           ItemType = ArmsArmor or ItemType = LegsArmor or
           Tools_List.Find_Index(Item => ItemType) /=
             UnboundedString_Container.No_Index then
            Set(InventoryList, InventoryIter, 3, True);
         else
            Set(InventoryList, InventoryIter, 3, False);
         end if;
         if PlayerShip.Crew(MemberIndex).Inventory(I).Durability < 100 then
            Set(InventoryList, InventoryIter, 6, True);
            Set
              (InventoryList, InventoryIter, 4,
               " " &
               GetItemDamage
                 (PlayerShip.Crew(MemberIndex).Inventory(I).Durability) &
               " ");
            Visible := True;
         else
            Set(InventoryList, InventoryIter, 6, False);
         end if;
         Set
           (InventoryList, InventoryIter, 5,
            Gint(PlayerShip.Crew(MemberIndex).Inventory(I).Durability));
         if Items_List(ProtoIndex).ShowType = Null_Unbounded_String then
            Set
              (InventoryList, InventoryIter, 7,
               To_String(Items_List(ProtoIndex).IType));
         else
            Set
              (InventoryList, InventoryIter, 7,
               To_String(Items_List(ProtoIndex).ShowType));
         end if;
         Set
           (InventoryList, InventoryIter, 8,
            Gint(PlayerShip.Crew(MemberIndex).Inventory(I).Amount));
         ItemWeight :=
           PlayerShip.Crew(MemberIndex).Inventory(I).Amount *
           Items_List(ProtoIndex).Weight;
         Set(InventoryList, InventoryIter, 9, Gint(ItemWeight));
      end loop;
      Set_Visible
        (Gtk_Tree_View_Column
           (Get_Object(Builder, "columninventorydurability")),
         Visible);
      Set_Label
        (Gtk_Label(Get_Object(Builder, "lblinventoryfreespace")),
         "Free inventory space:" &
         Integer'Image(FreeInventory(MemberIndex, 0)) & " kg");
   end RefreshInventory;

   procedure SetActiveItem is
   begin
      if PlayerShip.Crew(MemberIndex).Inventory.Length > 0 then
         Set_Cursor
           (Gtk_Tree_View(Get_Object(Builder, "treeinventory")),
            Gtk_Tree_Path_New_From_String("0"), null, False);
      else
         Set_Text(Gtk_Label(Get_Object(Builder, "lbliteminfo")), "");
         Hide(Gtk_Widget(Get_Object(Builder, "boxitemtocargo")));
      end if;
   end SetActiveItem;

   procedure RefreshCrewInfo is
      CrewIter: Gtk_Tree_Iter;
      CrewList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "crewlist2"));
      OrdersNames: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("Piloting"), To_Unbounded_String("Engineering"),
         To_Unbounded_String("Gunner"), To_Unbounded_String("Repair ship"),
         To_Unbounded_String("Manufacturing"),
         To_Unbounded_String("Upgrading module"),
         To_Unbounded_String("Talking in bases"),
         To_Unbounded_String("Healing wounded"),
         To_Unbounded_String("Cleans ship"), To_Unbounded_String("On break"),
         To_Unbounded_String("Defends ship"), To_Unbounded_String("Boarding"),
         To_Unbounded_String("Trains"));
   begin
      Clear(CrewList);
      for I in PlayerShip.Crew.Iterate loop
         Append(CrewList, CrewIter);
         Set(CrewList, CrewIter, 0, To_String(PlayerShip.Crew(I).Name));
         Set
           (CrewList, CrewIter, 1,
            To_String
              (OrdersNames(Crew_Orders'Pos(PlayerShip.Crew(I).Order) + 1)));
         Set(CrewList, CrewIter, 2, Gint(Crew_Container.To_Index(I)));
      end loop;
   end RefreshCrewInfo;

   procedure SetActiveMember(NewMemberIndex: Natural := 0) is
   begin
      MemberIndex := NewMemberIndex + 1;
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treecrew2")),
         Gtk_Tree_Path_New_From_String(Natural'Image(NewMemberIndex)), null,
         False);
   end SetActiveMember;

   procedure CreateCrewUI is
   begin
      Register_Handler(Builder, "Show_Member_Info", ShowMemberInfo'Access);
      Register_Handler(Builder, "Give_Orders_All", GiveOrdersAll'Access);
      Register_Handler(Builder, "Show_Inventory", ShowInventory'Access);
      Register_Handler(Builder, "Show_Item_Info2", ShowItemInfo2'Access);
      Register_Handler(Builder, "Move_Item", MoveItem'Access);
      Register_Handler(Builder, "Dismiss_Member", DismissMember'Access);
      On_Changed
        (Gtk_Cell_Renderer_Combo(Get_Object(Builder, "renderorders1")),
         GiveCrewOrders'Access);
      On_Changed
        (Gtk_Cell_Renderer_Combo(Get_Object(Builder, "renderpriorities")),
         SetPriority'Access);
      On_Toggled
        (Gtk_Cell_Renderer_Toggle(Get_Object(Builder, "renderused")),
         UseItem'Access);
      On_Key_Press_Event
        (Gtk_Widget(Get_Object(Builder, "spininventorymove")),
         SelectElement'Access, Get_Object(Builder, "btnmove"));
   end CreateCrewUI;

   procedure ShowCrewUI is
   begin
      RefreshCrewInfo;
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")), "crew");
      SetActiveMember;
      ShowOrdersForAll;
      UpdateMessages;
      if PlayerShip.Crew(1).Health = 0 then
         Set_Property
           (Get_Object(Builder, "renderorders1"),
            Gtk.Cell_Renderer_Text.Editable_Property, False);
         Set_Property
           (Get_Object(Builder, "renderpriorities"),
            Gtk.Cell_Renderer_Text.Editable_Property, False);
         Set_Property
           (Get_Object(Builder, "renderused"),
            Gtk.Cell_Renderer_Toggle.Activatable_Property, False);
         Hide(Gtk_Widget(Get_Object(Builder, "btndismiss")));
         Hide(Gtk_Widget(Get_Object(Builder, "btnclearall")));
         Hide(Gtk_Widget(Get_Object(Builder, "btnrepairall")));
         Hide(Gtk_Widget(Get_Object(Builder, "boxitemtocargo")));
      elsif not Is_Visible(Gtk_Widget(Get_Object(Builder, "btndismiss"))) then
         Set_Property
           (Get_Object(Builder, "renderorders1"),
            Gtk.Cell_Renderer_Text.Editable_Property, True);
         Set_Property
           (Get_Object(Builder, "renderpriorities"),
            Gtk.Cell_Renderer_Text.Editable_Property, True);
         Set_Property
           (Get_Object(Builder, "renderused"),
            Gtk.Cell_Renderer_Toggle.Activatable_Property, True);
      end if;
   end ShowCrewUI;

end Crew.UI;
