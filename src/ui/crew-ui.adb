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
with Gtk.Widget; use Gtk.Widget;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Label; use Gtk.Label;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Combo; use Gtk.Cell_Renderer_Combo;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Glib.Object; use Glib.Object;
with Glib.Types; use Glib.Types;
with Glib.Properties; use Glib.Properties;
with Game; use Game;
with Ships; use Ships;
with ShipModules; use ShipModules;
with Crew.Inventory; use Crew.Inventory;
with Crew.UI.Handlers; use Crew.UI.Handlers;

package body Crew.UI is

   procedure SetOrdersList is
      OrdersModel: Glib.Types.GType_Interface;
      OrdersList: Gtk_List_Store;
      OrdersIter: Gtk_Tree_Iter;
      NeedClean, NeedRepair: Boolean := True;
      procedure AddOrder
        (OrderText: String;
         OrderIndex, ModuleIndex: Natural) is
      begin
         Append(OrdersList, OrdersIter);
         Set(OrdersList, OrdersIter, 0, OrderText);
         Set(OrdersList, OrdersIter, 1, Gint(OrderIndex));
         Set(OrdersList, OrdersIter, 2, Gint(ModuleIndex));
      end AddOrder;
   begin
      OrdersModel :=
        Get_Property
          (Get_Object(Builder, "renderorders"),
           Gtk.Cell_Renderer_Combo.Model_Property);
      OrdersList := -(Gtk_Tree_Model(OrdersModel));
      OrdersList.Clear;
      if
        (PlayerShip.Crew(MemberIndex).Tired = 100 or
         PlayerShip.Crew(MemberIndex).Hunger = 100 or
         PlayerShip.Crew(MemberIndex).Thirst = 100) and
        PlayerShip.Crew(MemberIndex).Order /= Rest then
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
                  when GUN =>
                     if PlayerShip.Modules(I).Owner /= MemberIndex then
                        AddOrder
                          ("Operate " & To_String(PlayerShip.Modules(I).Name),
                           2,
                           Modules_Container.To_Index(I));
                     end if;
                  when ALCHEMY_LAB .. GREENHOUSE =>
                     if PlayerShip.Modules(I).Owner /= MemberIndex and
                       PlayerShip.Modules(I).Data(1) /= 0 then
                        AddOrder
                          ("Work in " & To_String(PlayerShip.Modules(I).Name),
                           4,
                           Modules_Container.To_Index(I));
                     end if;
                  when CABIN =>
                     if PlayerShip.Modules(I).Data(1) <
                       PlayerShip.Modules(I).Data(2) and
                       PlayerShip.Crew(MemberIndex).Order /= Clean and
                       NeedClean then
                        AddOrder("Clean ship", 8, 0);
                        NeedClean := False;
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
         if Module.Durability > 0 and
           Modules_List(Module.ProtoIndex).MType = CABIN and
           Module.Data(1) < Module.Data(2) then
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
      InventoryList: Gtk_List_Store;
      ItemName: Unbounded_String;
   begin
      InventoryList := Gtk_List_Store(Get_Object(Builder, "inventorylist"));
      Clear(InventoryList);
      for I in
        PlayerShip.Crew(MemberIndex).Inventory.First_Index ..
            PlayerShip.Crew(MemberIndex).Inventory.Last_Index loop
         ItemName :=
           To_Unbounded_String
             (GetItemName(PlayerShip.Crew(MemberIndex).Inventory(I)));
         if ItemIsUsed(MemberIndex, I) then
            ItemName := ItemName & "(used)";
         end if;
         Append(InventoryList, InventoryIter);
         Set(InventoryList, InventoryIter, 0, To_String(ItemName));
      end loop;
      Set_Label
        (Gtk_Label(Get_Object(Builder, "lblfreespace")),
         "Free inventory space:" &
         Integer'Image(FreeInventory(MemberIndex, 0)) &
         " kg");
   end RefreshInventory;

   procedure SetActiveItem is
   begin
      if PlayerShip.Crew(MemberIndex).Inventory.Length > 0 then
         Set_Cursor
           (Gtk_Tree_View(Get_Object(Builder, "treeinventory")),
            Gtk_Tree_Path_New_From_String("0"),
            Gtk_Tree_View_Column(Get_Object(Builder, "columninventory")),
            False);
      end if;
   end SetActiveItem;

   procedure RefreshCrewInfo is
      CrewIter: Gtk_Tree_Iter;
      CrewList: Gtk_List_Store;
      OrdersNames: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("Piloting"),
         To_Unbounded_String("Engineering"),
         To_Unbounded_String("Gunner"),
         To_Unbounded_String("Repair ship"),
         To_Unbounded_String("Manufacturing"),
         To_Unbounded_String("Upgrading module"),
         To_Unbounded_String("Talking in bases"),
         To_Unbounded_String("Healing wounded"),
         To_Unbounded_String("Cleans ship"),
         To_Unbounded_String("On break"),
         To_Unbounded_String("Boarding"),
         To_Unbounded_String("Defends ship"));
   begin
      CrewList := Gtk_List_Store(Get_Object(Builder, "crewlist"));
      Clear(CrewList);
      for Member of PlayerShip.Crew loop
         Append(CrewList, CrewIter);
         Set(CrewList, CrewIter, 0, To_String(Member.Name));
         Set
           (CrewList,
            CrewIter,
            1,
            To_String(OrdersNames(Crew_Orders'Pos(Member.Order) + 1)));
      end loop;
   end RefreshCrewInfo;

   procedure SetActiveMember is
   begin
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treecrew")),
         Gtk_Tree_Path_New_From_String("0"),
         Gtk_Tree_View_Column(Get_Object(Builder, "columncrew")),
         False);
   end SetActiveMember;

   procedure CreateCrewUI is
      Error: aliased GError;
   begin
      if Builder /= null then
         return;
      end if;
      Gtk_New(Builder);
      if Add_From_File
          (Builder,
           To_String(DataDirectory) & "ui" & Dir_Separator & "crew.glade",
           Error'Access) =
        Guint(0) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      Register_Handler(Builder, "Hide_Ship_Info", HideShipInfo'Access);
      Register_Handler(Builder, "Show_Member_Info", ShowMemberInfo'Access);
      Register_Handler(Builder, "Show_Help", ShowHelp'Access);
      Register_Handler(Builder, "Hide_Window", HideWindow'Access);
      Register_Handler(Builder, "Give_Orders_All", GiveOrdersAll'Access);
      Register_Handler(Builder, "Hide_Last_Message", HideLastMessage'Access);
      Register_Handler(Builder, "Show_Inventory", ShowInventory'Access);
      Register_Handler(Builder, "Show_Item_Info", ShowItemInfo'Access);
      Register_Handler(Builder, "Use_Item", UseItem'Access);
      Register_Handler(Builder, "Show_Move_Item", ShowMoveItem'Access);
      Register_Handler(Builder, "Move_Item", MoveItem'Access);
      Register_Handler(Builder, "Show_Priorities", ShowPriorities'Access);
      Register_Handler(Builder, "Set_Priorities", SetPriorities'Access);
      Register_Handler(Builder, "Dismiss_Member", DismissMember'Access);
      Do_Connect(Builder);
      On_Changed
        (Gtk_Cell_Renderer_Combo(Get_Object(Builder, "renderorders")),
         GiveCrewOrders'Access);
      On_Changed
        (Gtk_Cell_Renderer_Combo(Get_Object(Builder, "renderpriorities")),
         SetPriority'Access);
   end CreateCrewUI;

   procedure ShowCrewUI(OldState: GameStates) is
   begin
      RefreshCrewInfo;
      PreviousGameState := OldState;
      Show_All(Gtk_Widget(Get_Object(Builder, "crewwindow")));
      ShowLastMessage(Builder);
      SetActiveMember;
      ShowOrdersForAll;
   end ShowCrewUI;

end Crew.UI;
