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
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Label; use Gtk.Label;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Window; use Gtk.Window;
with Gtk.Progress_Bar; use Gtk.Progress_Bar;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Glib.Object; use Glib.Object;
with Messages; use Messages;
with Crew.Inventory; use Crew.Inventory;

package body Ships.Cargo.UI is

   Builder: Gtkada_Builder;
   ItemIndex: Positive;

   procedure RefreshCargoInfo is
      CargoIter: Gtk_Tree_Iter;
      CargoList: Gtk_List_Store;
   begin
      CargoList := Gtk_List_Store(Get_Object(Builder, "cargolist"));
      Clear(CargoList);
      for I in PlayerShip.Cargo.Iterate loop
         Append(CargoList, CargoIter);
         Set(CargoList, CargoIter, 0, GetItemName(PlayerShip.Cargo(I)));
         Set(CargoList, CargoIter, 1, Gint(Inventory_Container.To_Index(I)));
      end loop;
      Set_Label
        (Gtk_Label(Get_Object(Builder, "lblfreespace")),
         "Free cargo space:" & Integer'Image(FreeCargo(0)) & " kg");
   end RefreshCargoInfo;

   procedure SetActiveItem is
   begin
      if PlayerShip.Cargo.Length > 0 then
         Set_Cursor
           (Gtk_Tree_View(Get_Object(Builder, "treecargo")),
            Gtk_Tree_Path_New_From_String("0"),
            Gtk_Tree_View_Column(Get_Object(Builder, "columncargo")),
            False);
      end if;
   end SetActiveItem;

   procedure ShowItemInfo(Object: access Gtkada_Builder_Record'Class) is
      CargoIter: Gtk_Tree_Iter;
      CargoModel: Gtk_Tree_Model;
      ItemInfo: Unbounded_String;
      ProtoIndex, ItemWeight: Positive;
      DamagePercent: Gdouble;
      DamageBar: constant GObject := Get_Object(Object, "damagebar");
      AmountAdj: constant Gtk_Adjustment :=
        Gtk_Adjustment(Get_Object(Object, "amountadj"));
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treecargo"))),
         CargoModel,
         CargoIter);
      if CargoIter = Null_Iter then
         return;
      end if;
      ItemIndex := Positive(Get_Int(CargoModel, CargoIter, 1));
      if ItemIndex > Positive(PlayerShip.Cargo.Length) then
         return;
      end if;
      ProtoIndex := PlayerShip.Cargo(ItemIndex).ProtoIndex;
      ItemWeight :=
        PlayerShip.Cargo(ItemIndex).Amount * Items_List(ProtoIndex).Weight;
      ItemInfo := To_Unbounded_String("Type: ");
      if Items_List(ProtoIndex).ShowType = Null_Unbounded_String then
         Append(ItemInfo, Items_List(ProtoIndex).IType);
      else
         Append(ItemInfo, Items_List(ProtoIndex).ShowType);
      end if;
      Append
        (ItemInfo,
         ASCII.LF &
         "Amount:" &
         Positive'Image(PlayerShip.Cargo(ItemIndex).Amount));
      Append
        (ItemInfo,
         ASCII.LF &
         "Weight:" &
         Positive'Image(Items_List(ProtoIndex).Weight) &
         " kg");
      Append
        (ItemInfo,
         ASCII.LF & "Total weight:" & Positive'Image(ItemWeight) & " kg");
      if Items_List(ProtoIndex).IType = WeaponType then
         Append
           (ItemInfo,
            ASCII.LF &
            "Skill: " &
            Skills_List(Items_List(ProtoIndex).Value(3)).Name &
            "/" &
            Attributes_Names
              (Skills_List(Items_List(ProtoIndex).Value(3)).Attribute));
      end if;
      Set_Markup
        (Gtk_Label(Get_Object(Object, "lblinfo")),
         To_String(ItemInfo));
      if PlayerShip.Cargo(ItemIndex).Durability < 100 then
         Set_Visible(Gtk_Widget(DamageBar), True);
         DamagePercent :=
           1.0 - (Gdouble(PlayerShip.Cargo(ItemIndex).Durability) / 100.0);
         Set_Fraction(Gtk_Progress_Bar(DamageBar), DamagePercent);
         if DamagePercent < 0.2 then
            Set_Text(Gtk_Progress_Bar(DamageBar), "Slightly used");
         elsif DamagePercent < 0.5 then
            Set_Text(Gtk_Progress_Bar(DamageBar), "Damaged");
         elsif DamagePercent < 0.8 then
            Set_Text(Gtk_Progress_Bar(DamageBar), "Heavily damaged");
         else
            Set_Text(Gtk_Progress_Bar(DamageBar), "Almost destroyed");
         end if;
      else
         Set_Visible(Gtk_Widget(DamageBar), False);
      end if;
      if Items_List(ProtoIndex).Description /= Null_Unbounded_String then
         Set_Label
           (Gtk_Label(Get_Object(Object, "lbldescription")),
            ASCII.LF & To_String(Items_List(ProtoIndex).Description));
      end if;
      if Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).IType =
        MissionItemsType then
         Hide(Gtk_Widget(Get_Object(Builder, "btngiveto")));
      else
         Show_All(Gtk_Widget(Get_Object(Builder, "btngiveto")));
      end if;
      Set_Upper(AmountAdj, Gdouble(PlayerShip.Cargo(ItemIndex).Amount));
      Set_Value(AmountAdj, 1.0);
   end ShowItemInfo;

   procedure DropItem(Object: access Gtkada_Builder_Record'Class) is
      DropAmount: Natural :=
        Natural(Get_Value(Gtk_Adjustment(Get_Object(Object, "amountadj"))));
      DropAmount2: constant Natural := DropAmount;
   begin
      if Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).IType =
        MissionItemsType then
         for J in 1 .. DropAmount2 loop
            for I in
              PlayerShip.Missions.First_Index ..
                  PlayerShip.Missions.Last_Index loop
               if PlayerShip.Missions(I).MType = Deliver and
                 PlayerShip.Missions(I).Target =
                   PlayerShip.Cargo(ItemIndex).ProtoIndex then
                  DeleteMission(I);
                  DropAmount := DropAmount - 1;
                  exit;
               end if;
            end loop;
         end loop;
      end if;
      if DropAmount > 0 then
         AddMessage
           ("You dropped" &
            Positive'Image(DropAmount) &
            " " &
            GetItemName(PlayerShip.Cargo(ItemIndex)) &
            ".",
            OtherMessage);
         UpdateCargo
           (PlayerShip,
            PlayerShip.Cargo.Element(ItemIndex).ProtoIndex,
            (0 - DropAmount),
            PlayerShip.Cargo.Element(ItemIndex).Durability);
      end if;
      RefreshCargoInfo;
      ShowLastMessage(Object);
      SetActiveItem;
   end DropItem;

   procedure ShowGiveItem(Object: access Gtkada_Builder_Record'Class) is
      CrewIter: Gtk_Tree_Iter;
      CrewList: Gtk_List_Store;
      AmountAdj: constant Gtk_Adjustment :=
        Gtk_Adjustment(Get_Object(Object, "amountadj"));
   begin
      Set_Upper(AmountAdj, Gdouble(PlayerShip.Cargo(ItemIndex).Amount));
      Set_Value(AmountAdj, 1.0);
      CrewList := Gtk_List_Store(Get_Object(Object, "crewlist"));
      Clear(CrewList);
      for Member of PlayerShip.Crew loop
         Append(CrewList, CrewIter);
         Set(CrewList, CrewIter, 0, To_String(Member.Name));
      end loop;
      Show_All(Gtk_Widget(Get_Object(Object, "giveitemwindow")));
      Set_Active(Gtk_Combo_Box(Get_Object(Object, "cmbmember")), 0);
   end ShowGiveItem;

   procedure GiveItem(Object: access Gtkada_Builder_Record'Class) is
      MemberIndex: constant Positive :=
        Positive
          (Get_Active(Gtk_Combo_Box(Get_Object(Object, "cmbmember"))) + 1);
      Amount: constant Positive :=
        Positive(Get_Value(Gtk_Adjustment(Get_Object(Object, "amountadj"))));
      Item: constant InventoryData := PlayerShip.Cargo(ItemIndex);
   begin
      if FreeInventory
          (MemberIndex,
           0 - (Items_List(Item.ProtoIndex).Weight * Amount)) <
        0 then
         ShowDialog
           ("No free space in " &
            To_String(PlayerShip.Crew(MemberIndex).Name) &
            "'s inventory for that amount of " &
            GetItemName(Item),
            Gtk_Window(Get_Object(Object, "giveitemwindow")));
         return;
      end if;
      UpdateInventory(MemberIndex, Amount, Item.ProtoIndex, Item.Durability);
      UpdateCargo
        (Ship => PlayerShip,
         Amount => (0 - Amount),
         CargoIndex => ItemIndex);
      Hide(Gtk_Widget(Get_Object(Object, "giveitemwindow")));
      RefreshCargoInfo;
      ShowLastMessage(Object);
      SetActiveItem;
   end GiveItem;

   procedure CreateCargoUI is
      Error: aliased GError;
   begin
      if Builder /= null then
         return;
      end if;
      Gtk_New(Builder);
      if Add_From_File
          (Builder,
           To_String(DataDirectory) &
           "ui" &
           Dir_Separator &
           "ships-cargo.glade",
           Error'Access) =
        Guint(0) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      Register_Handler(Builder, "Hide_Ship_Info", HideShipInfo'Access);
      Register_Handler(Builder, "Hide_Last_Message", HideLastMessage'Access);
      Register_Handler(Builder, "Show_Item_Info", ShowItemInfo'Access);
      Register_Handler(Builder, "Hide_Window", HideWindow'Access);
      Register_Handler(Builder, "Drop_Item", DropItem'Access);
      Register_Handler(Builder, "Show_Give_Item", ShowGiveItem'Access);
      Register_Handler(Builder, "Give_Item", GiveItem'Access);
      Do_Connect(Builder);
      On_Key_Release_Event
        (Gtk_Widget(Get_Object(Builder, "cargowindow")),
         CloseWindow'Access);
      On_Key_Release_Event
        (Gtk_Widget(Get_Object(Builder, "giveitemwindow")),
         CloseWindow'Access);
   end CreateCargoUI;

   procedure ShowCargoUI(OldState: GameStates) is
   begin
      RefreshCargoInfo;
      PreviousGameState := OldState;
      Show_All(Gtk_Widget(Get_Object(Builder, "cargowindow")));
      ShowLastMessage(Builder);
      SetActiveItem;
   end ShowCargoUI;

end Ships.Cargo.UI;
