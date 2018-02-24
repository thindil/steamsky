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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Label; use Gtk.Label;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Window; use Gtk.Window;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Glib.Object; use Glib.Object;
with Gdk.RGBA; use Gdk.RGBA;
with Game; use Game;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Events; use Events;
with Items; use Items;
with Bases.Cargo; use Bases.Cargo;
with Help.UI; use Help.UI;
with Utils.UI; use Utils.UI;

package body Trades.UI is

   Builder: Gtkada_Builder;

   function HideTrade
     (Object: access Gtkada_Builder_Record'Class) return Boolean is
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      EventIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex;
   begin
      if BaseIndex = 0 and EventIndex > 0 then
         DeleteEvent(EventIndex);
      end if;
      Hide(Gtk_Widget(Get_Object(Object, "tradewindow")));
      CreateSkyMap;
      return True;
   end HideTrade;

   procedure ShowItemInfo(Object: access Gtkada_Builder_Record'Class) is
      ItemsIter: Gtk_Tree_Iter;
      ItemsModel: Gtk_Tree_Model;
      ItemInfo: Unbounded_String;
      Amount, ProtoIndex, Price: Positive;
      CargoIndex, BaseCargoIndex, BaseCargoIndex2: Natural := 0;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseType: Positive;
      EventIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex;
      DamagePercent, MoneyIndex2: Natural;
      FreeSpace: Integer;
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treeitems"))),
         ItemsModel,
         ItemsIter);
      if ItemsIter = Null_Iter then
         return;
      end if;
      if BaseIndex > 0 then
         BaseType := Bases_Types'Pos(SkyBases(BaseIndex).BaseType) + 1;
      else
         BaseType := 1;
      end if;
      CargoIndex := Natural(Get_Int(ItemsModel, ItemsIter, 1));
      if CargoIndex > Natural(PlayerShip.Cargo.Length) then
         return;
      end if;
      BaseCargoIndex := Natural(Get_Int(ItemsModel, ItemsIter, 2));
      if BaseIndex = 0 and BaseCargoIndex > Natural(TraderCargo.Length) then
         return;
      elsif BaseCargoIndex > Natural(SkyBases(BaseIndex).Cargo.Length) then
         return;
      end if;
      if CargoIndex > 0 then
         ProtoIndex := PlayerShip.Cargo(CargoIndex).ProtoIndex;
         if BaseCargoIndex = 0 then
            BaseCargoIndex2 := FindBaseCargo(ProtoIndex);
         end if;
      else
         if BaseIndex = 0 then
            ProtoIndex := TraderCargo(BaseCargoIndex).ProtoIndex;
         else
            ProtoIndex := SkyBases(BaseIndex).Cargo(BaseCargoIndex).ProtoIndex;
         end if;
      end if;
      if BaseCargoIndex > 0 then
         if BaseIndex = 0 then
            Amount := TraderCargo(BaseCargoIndex).Amount;
         else
            Amount := SkyBases(BaseIndex).Cargo(BaseCargoIndex).Amount;
         end if;
      end if;
      ItemInfo := To_Unbounded_String("Type: ");
      if Items_List(ProtoIndex).ShowType = Null_Unbounded_String then
         Append(ItemInfo, Items_List(ProtoIndex).IType);
      else
         Append(ItemInfo, Items_List(ProtoIndex).ShowType);
      end if;
      if Items_List(ProtoIndex).Buyable(BaseType) then
         Append(ItemInfo, ASCII.LF & "Base buy/sell price:");
      else
         Append(ItemInfo, ASCII.LF & "Base sell price:");
      end if;
      if BaseCargoIndex = 0 then
         if BaseCargoIndex2 > 0 then
            if BaseIndex > 0 then
               Price := SkyBases(BaseIndex).Cargo(BaseCargoIndex2).Price;
            else
               Price := TraderCargo(BaseCargoIndex2).Price;
            end if;
         else
            Price := Items_List(ProtoIndex).Prices(BaseType);
         end if;
      else
         if BaseIndex > 0 then
            Price := SkyBases(BaseIndex).Cargo(BaseCargoIndex).Price;
         else
            Price := TraderCargo(BaseCargoIndex).Price;
         end if;
      end if;
      if EventIndex > 0 then
         if Events_List(EventIndex).EType = DoublePrice and
           Events_List(EventIndex).Data = ProtoIndex then
            Price := Price * 2;
         end if;
      end if;
      Append(ItemInfo, Natural'Image(Price) & " " & To_String(MoneyName));
      Append
        (ItemInfo,
         ASCII.LF &
         "Weight:" &
         Integer'Image(Items_List(ProtoIndex).Weight) &
         " kg");
      if Items_List(ProtoIndex).IType = WeaponType then
         Append
           (ItemInfo,
            ASCII.LF &
            "Skill: " &
            To_String(Skills_List(Items_List(ProtoIndex).Value(3)).Name) &
            "/" &
            To_String
              (Attributes_Names
                 (Skills_List(Items_List(ProtoIndex).Value(3)).Attribute)));
      end if;
      if CargoIndex > 0 then
         Append
           (ItemInfo,
            ASCII.LF &
            "Owned:" &
            Positive'Image(PlayerShip.Cargo(CargoIndex).Amount));
         if PlayerShip.Cargo(CargoIndex).Durability < 100 then
            DamagePercent :=
              100 -
              Natural
                ((Float(PlayerShip.Cargo(CargoIndex).Durability) / 100.0) *
                 100.0);
            Append(ItemInfo, ASCII.LF & "Status: ");
            case DamagePercent is
               when 1 .. 19 =>
                  Append
                    (ItemInfo,
                     "<span foreground=""green"">Slightly used</span>");
               when 20 .. 49 =>
                  Append
                    (ItemInfo,
                     "<span foreground=""yellow"">Damaged</span>");
               when 50 .. 79 =>
                  Append
                    (ItemInfo,
                     "<span foreground=""red"">Heavily damaged</span>");
               when others =>
                  Append
                    (ItemInfo,
                     "<span foreground=""blue"">Almost destroyed</span>");
            end case;
         end if;
      end if;
      if BaseCargoIndex > 0 then
         if BaseIndex > 0 then
            Append(ItemInfo, ASCII.LF & "In base:");
         else
            Append(ItemInfo, ASCII.LF & "In ship:");
         end if;
         Append(ItemInfo, Positive'Image(Amount));
      end if;
      if Items_List(ProtoIndex).Description /= Null_Unbounded_String then
         Append
           (ItemInfo,
            ASCII.LF & ASCII.LF & Items_List(ProtoIndex).Description);
      end if;
      Set_Label(Gtk_Label(Get_Object(Object, "lblinfo")), To_String(ItemInfo));
      if CargoIndex = 0 then
         Set_Visible(Gtk_Widget(Get_Object(Object, "btnsell")), False);
      else
         Set_Visible(Gtk_Widget(Get_Object(Object, "btnsell")), True);
      end if;
      MoneyIndex2 := FindItem(PlayerShip.Cargo, FindProtoItem(MoneyIndex));
      if BaseCargoIndex = 0 or MoneyIndex2 = 0 then
         Set_Visible(Gtk_Widget(Get_Object(Object, "btnbuy")), False);
      else
         Set_Visible(Gtk_Widget(Get_Object(Object, "btnbuy")), True);
      end if;
      if MoneyIndex2 > 0 then
         Set_Label
           (Gtk_Label(Get_Object(Object, "lblshipmoney")),
            "You have" &
            Natural'Image(PlayerShip.Cargo(MoneyIndex2).Amount) &
            " " &
            To_String(MoneyName) &
            ".");
      else
         Set_Label
           (Gtk_Label(Get_Object(Object, "lblshipmoney")),
            "You don't have any " &
            To_String(MoneyName) &
            " to buy anything.");
      end if;
      FreeSpace := FreeCargo(0);
      if FreeSpace < 0 then
         FreeSpace := 0;
      end if;
      Set_Label
        (Gtk_Label(Get_Object(Object, "lblshipspace")),
         "Free cargo space:" & Integer'Image(FreeSpace) & " kg");
      if BaseIndex > 0 then
         if SkyBases(BaseIndex).Cargo(1).Amount = 0 then
            Set_Label
              (Gtk_Label(Get_Object(Object, "lblbasemoney")),
               "Base don't have any " &
               To_String(MoneyName) &
               "to buy anything.");
         else
            Set_Label
              (Gtk_Label(Get_Object(Object, "lblbasemoney")),
               "Base have" &
               Positive'Image(SkyBases(BaseIndex).Cargo(1).Amount) &
               " " &
               To_String(MoneyName) &
               ".");
         end if;
      else
         if TraderCargo(1).Amount = 0 then
            Set_Label
              (Gtk_Label(Get_Object(Object, "lblbasemoney")),
               "Ship don't have any " &
               To_String(MoneyName) &
               "to buy anything.");
         else
            Set_Label
              (Gtk_Label(Get_Object(Object, "lblbasemoney")),
               "Ship have" &
               Positive'Image(TraderCargo(1).Amount) &
               " " &
               To_String(MoneyName) &
               ".");
         end if;
      end if;
   end ShowItemInfo;

   procedure ShowHelp(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
   begin
      ShowHelpUI(3);
   end ShowHelp;

   procedure ShowTradeItem(User_Data: access GObject_Record'Class) is
      ItemsIter: Gtk_Tree_Iter;
      ItemsModel: Gtk_Tree_Model;
      MaxAmount, MoneyIndex2: Natural;
      CargoIndex, BaseCargoIndex: Natural := 0;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Price, BaseType, ProtoIndex: Positive;
      EventIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex;
      AmountAdj: constant Gtk_Adjustment :=
        Gtk_Adjustment(Get_Object(Builder, "amountadj"));
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Builder, "treeitems"))),
         ItemsModel,
         ItemsIter);
      if ItemsIter = Null_Iter then
         return;
      end if;
      CargoIndex := Natural(Get_Int(ItemsModel, ItemsIter, 1));
      BaseCargoIndex := Natural(Get_Int(ItemsModel, ItemsIter, 2));
      if CargoIndex > 0 then
         ProtoIndex := PlayerShip.Cargo(CargoIndex).ProtoIndex;
      else
         if BaseIndex = 0 then
            ProtoIndex := TraderCargo(BaseCargoIndex).ProtoIndex;
         else
            ProtoIndex := SkyBases(BaseIndex).Cargo(BaseCargoIndex).ProtoIndex;
         end if;
      end if;
      if BaseIndex > 0 then
         BaseType := Bases_Types'Pos(SkyBases(BaseIndex).BaseType) + 1;
      else
         BaseType := 1;
      end if;
      if BaseCargoIndex = 0 then
         Price := Items_List(ProtoIndex).Prices(BaseType);
      else
         if BaseIndex > 0 then
            Price := SkyBases(BaseIndex).Cargo(BaseCargoIndex).Price;
         else
            Price := TraderCargo(BaseCargoIndex).Price;
         end if;
      end if;
      if EventIndex > 0 then
         if Events_List(EventIndex).EType = DoublePrice and
           Events_List(EventIndex).Data = ProtoIndex then
            Price := Price * 2;
         end if;
      end if;
      Set_Value(AmountAdj, 1.0);
      if User_Data = Get_Object(Builder, "btnbuy") then
         MoneyIndex2 := FindItem(PlayerShip.Cargo, FindProtoItem(MoneyIndex));
         MaxAmount := PlayerShip.Cargo(MoneyIndex2).Amount / Price;
         if BaseIndex > 0 then
            if MaxAmount >
              SkyBases(BaseIndex).Cargo(BaseCargoIndex).Amount then
               MaxAmount := SkyBases(BaseIndex).Cargo(BaseCargoIndex).Amount;
            end if;
         else
            if MaxAmount > TraderCargo(BaseCargoIndex).Amount then
               MaxAmount := TraderCargo(BaseCargoIndex).Amount;
            end if;
         end if;
         Set_Upper(AmountAdj, Gdouble(MaxAmount));
         Set_Label
           (Gtk_Label(Get_Object(Builder, "lblbuyamount")),
            "Amount to buy (max" & Natural'Image(MaxAmount) & "):");
         Show_All(Gtk_Widget(Get_Object(Builder, "buywindow")));
      else
         MaxAmount := PlayerShip.Cargo(CargoIndex).Amount;
         if BaseIndex > 0 then
            if MaxAmount > (SkyBases(BaseIndex).Cargo(1).Amount / Price) then
               MaxAmount := SkyBases(BaseIndex).Cargo(1).Amount / Price;
            end if;
         else
            if MaxAmount > (TraderCargo(1).Amount / Price) then
               MaxAmount := TraderCargo(1).Amount / Price;
            end if;
         end if;
         Set_Upper(AmountAdj, Gdouble(MaxAmount));
         Set_Label
           (Gtk_Label(Get_Object(Builder, "lblsellamount")),
            "Amount to sell (max" & Natural'Image(MaxAmount) & "):");
         Show_All(Gtk_Widget(Get_Object(Builder, "sellwindow")));
      end if;
   end ShowTradeItem;

   procedure TradeItem(User_Data: access GObject_Record'Class) is
      ItemsIter: Gtk_Tree_Iter;
      ItemsModel: Gtk_Tree_Model;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseCargoIndex, CargoIndex: Natural := 0;
      Trader: String(1 .. 4);
      Amount: constant Natural :=
        Natural(Get_Value(Gtk_Adjustment(Get_Object(Builder, "amountadj"))));
      ParentWindow: Gtk_Window;
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Builder, "treeitems"))),
         ItemsModel,
         ItemsIter);
      if ItemsIter = Null_Iter then
         return;
      end if;
      CargoIndex := Natural(Get_Int(ItemsModel, ItemsIter, 1));
      BaseCargoIndex := Natural(Get_Int(ItemsModel, ItemsIter, 2));
      if BaseIndex > 0 then
         Trader := "base";
      else
         Trader := "ship";
      end if;
      if User_Data = Get_Object(Builder, "btnbuyitem") then
         ParentWindow := Gtk_Window(Get_Object(Builder, "buywindow"));
         BuyItems(BaseCargoIndex, Natural'Image(Amount));
      else
         ParentWindow := Gtk_Window(Get_Object(Builder, "sellwindow"));
         if User_Data = Get_Object(Builder, "btnsellitem") then
            SellItems(CargoIndex, Natural'Image(Amount));
         else
            SellItems
              (CargoIndex,
               Positive'Image(PlayerShip.Cargo.Element(CargoIndex).Amount));
         end if;
      end if;
      Hide(Gtk_Widget(ParentWindow));
      ShowTradeUI;
   exception
      when An_Exception : Trade_Cant_Buy =>
         ShowDialog
           ("You can't buy " &
            Exception_Message(An_Exception) &
            " in this " &
            Trader &
            ".",
            ParentWindow);
      when An_Exception : Trade_Not_For_Sale_Now =>
         ShowDialog
           ("You can't buy " &
            Exception_Message(An_Exception) &
            " in this base at this moment.",
            ParentWindow);
      when An_Exception : Trade_Buying_Too_Much =>
         ShowDialog
           (Trader &
            " don't have that much " &
            Exception_Message(An_Exception) &
            " for sale.",
            ParentWindow);
      when Trade_No_Free_Cargo =>
         ShowDialog
           ("You don't have that much free space in your ship cargo.",
            ParentWindow);
      when An_Exception : Trade_No_Money =>
         ShowDialog
           ("You don't have any " &
            To_String(MoneyName) &
            " to buy " &
            Exception_Message(An_Exception) &
            ".",
            ParentWindow);
      when An_Exception : Trade_Not_Enough_Money =>
         ShowDialog
           ("You don't have enough " &
            To_String(MoneyName) &
            " to buy so much " &
            Exception_Message(An_Exception) &
            ".",
            ParentWindow);
      when Trade_Invalid_Amount =>
         if User_Data = Get_Object(Builder, "btnbuyitem") then
            ShowDialog("You entered invalid amount to buy.", ParentWindow);
         else
            ShowDialog("You entered invalid amount to sell.", ParentWindow);
         end if;
      when An_Exception : Trade_Too_Much_For_Sale =>
         ShowDialog
           ("You dont have that much " &
            Exception_Message(An_Exception) &
            " in ship cargo.",
            ParentWindow);
      when An_Exception : Trade_No_Money_In_Base =>
         ShowDialog
           ("You can't sell so much " &
            Exception_Message(An_Exception) &
            " because " &
            Trader &
            " don't have that much " &
            To_String(MoneyName) &
            " to buy it.",
            ParentWindow);
      when Trade_No_Trader =>
         ShowDialog
           ("You don't have assigned anyone in crew to talk in bases duty.",
            ParentWindow);
   end TradeItem;

   procedure CreateTradeUI is
      Error: aliased GError;
   begin
      if Builder /= null then
         return;
      end if;
      Gtk_New(Builder);
      if Add_From_File
          (Builder,
           To_String(DataDirectory) & "ui" & Dir_Separator & "trades.glade",
           Error'Access) =
        Guint(0) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      Override_Background_Color
        (Gtk_Widget(Get_Object(Builder, "lblinfo")),
         0,
         Black_RGBA);
      Override_Color
        (Gtk_Widget(Get_Object(Builder, "lblinfo")),
         0,
         White_RGBA);
      Register_Handler(Builder, "Hide_Trade", HideTrade'Access);
      Register_Handler(Builder, "Hide_Last_Message", HideLastMessage'Access);
      Register_Handler(Builder, "Show_Item_Info", ShowItemInfo'Access);
      Register_Handler(Builder, "Show_Help", ShowHelp'Access);
      Register_Handler(Builder, "Hide_Window", HideWindow'Access);
      Register_Handler(Builder, "Show_Trade_Item", ShowTradeItem'Access);
      Register_Handler(Builder, "Trade_Item", TradeItem'Access);
      Do_Connect(Builder);
   end CreateTradeUI;

   procedure ShowTradeUI is
      ItemsIter: Gtk_Tree_Iter;
      ItemsList: Gtk_List_Store;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseType: Positive;
      IndexesList: Positive_Container.Vector;
      BaseCargoIndex: Natural;
      BaseCargo: BaseCargo_Container.Vector;
   begin
      if BaseIndex > 0 then
         BaseType := Bases_Types'Pos(SkyBases(BaseIndex).BaseType) + 1;
         BaseCargo := SkyBases(BaseIndex).Cargo;
      else
         BaseType := 1;
         BaseCargo := TraderCargo;
      end if;
      ItemsList := Gtk_List_Store(Get_Object(Builder, "itemslist"));
      Clear(ItemsList);
      for I in PlayerShip.Cargo.Iterate loop
         if Items_List(PlayerShip.Cargo(I).ProtoIndex).Prices(BaseType) >
           0 then
            Append(ItemsList, ItemsIter);
            Set
              (ItemsList,
               ItemsIter,
               0,
               To_String(Items_List(PlayerShip.Cargo(I).ProtoIndex).Name));
            Set
              (ItemsList,
               ItemsIter,
               1,
               Gint(Inventory_Container.To_Index(I)));
            BaseCargoIndex :=
              FindBaseCargo
                (PlayerShip.Cargo(I).ProtoIndex,
                 PlayerShip.Cargo(I).Durability);
            if BaseCargoIndex > 0 then
               IndexesList.Append(New_Item => BaseCargoIndex);
            end if;
            Set(ItemsList, ItemsIter, 2, Gint(BaseCargoIndex));
         end if;
      end loop;
      for I in BaseCargo.First_Index .. BaseCargo.Last_Index loop
         if IndexesList.Find_Index(Item => I) = 0 and
           Items_List(BaseCargo(I).ProtoIndex).Buyable(BaseType) then
            Append(ItemsList, ItemsIter);
            Set
              (ItemsList,
               ItemsIter,
               0,
               To_String(Items_List(BaseCargo(I).ProtoIndex).Name));
            Set(ItemsList, ItemsIter, 1, 0);
            Set(ItemsList, ItemsIter, 2, Gint(I));
         end if;
      end loop;
      Show_All(Gtk_Widget(Get_Object(Builder, "tradewindow")));
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treeitems")),
         Gtk_Tree_Path_New_From_String("0"),
         Gtk_Tree_View_Column(Get_Object(Builder, "columnname")),
         False);
      ShowLastMessage(Builder);
   end ShowTradeUI;

end Trades.UI;
