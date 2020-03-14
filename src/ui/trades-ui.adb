--    Copyright 2018-2019 Bartek thindil Jasicki
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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Label; use Gtk.Label;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Gtkada.Builder; use Gtkada.Builder;
with Game; use Game;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Events; use Events;
with Items; use Items;
with Bases.Cargo; use Bases.Cargo;
with Utils.UI; use Utils.UI;
with Crew; use Crew;

package body Trades.UI is

   -- ****iv* Trades.UI/SettingTime
   -- FUNCTION
   -- If true, UI is in setting mode
   -- SOURCE
   SettingTime: Boolean;
   -- ****

   -- ****if* Trades.UI/CloseTrade
   -- FUNCTION
   -- Close trade screen and back to the map
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure CloseTrade(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      EventIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex;
   begin
      if BaseIndex = 0 and EventIndex > 0 then
         DeleteEvent(EventIndex);
      end if;
      ShowSkyMap;
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Object, "gamestack")), "skymap");
   end CloseTrade;

   -- ****if* Trades.UI/ShowItemTradeInfo
   -- FUNCTION
   -- Show detailed information about selected item to trade
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure ShowItemTradeInfo(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      ItemInfo, ProtoIndex: Unbounded_String;
      Price: Positive;
      CargoIndex, BaseCargoIndex, BaseCargoIndex2: Natural := 0;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseType: Positive;
      MoneyIndex2: Natural;
      ItemTypes: constant array(Positive range <>) of Unbounded_String :=
        (WeaponType, ChestArmor, HeadArmor, ArmsArmor, LegsArmor, ShieldType);
   begin
      declare
         ItemsIter: Gtk_Tree_Iter;
         ItemsModel: Gtk_Tree_Model;
      begin
         Get_Selected
           (Gtk.Tree_View.Get_Selection
              (Gtk_Tree_View(Get_Object(Object, "treeitems1"))),
            ItemsModel, ItemsIter);
         if ItemsIter = Null_Iter then
            return;
         end if;
         CargoIndex := Natural(Get_Int(ItemsModel, ItemsIter, 1));
         BaseCargoIndex := Natural(Get_Int(ItemsModel, ItemsIter, 2));
      end;
      if CargoIndex > Natural(PlayerShip.Cargo.Length) then
         return;
      end if;
      if BaseIndex > 0 then
         BaseType := Bases_Types'Pos(SkyBases(BaseIndex).BaseType) + 1;
      else
         BaseType := 1;
      end if;
      if BaseIndex = 0 and BaseCargoIndex > Natural(TraderCargo.Length) then
         return;
      elsif BaseIndex > 0
        and then BaseCargoIndex >
          Natural(SkyBases(BaseIndex).Cargo.Length) then
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
      declare
         EventIndex: constant Natural :=
           SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex;
      begin
         if EventIndex > 0 then
            if Events_List(EventIndex).EType = DoublePrice
              and then Events_List(EventIndex).ItemIndex = ProtoIndex then
               Price := Price * 2;
            end if;
         end if;
      end;
      Append
        (ItemInfo,
         "Weight:" & Integer'Image(Items_List(ProtoIndex).Weight) & " kg");
      if Items_List(ProtoIndex).IType = WeaponType then
         Append
           (ItemInfo,
            LF & "Skill: " &
            To_String(Skills_List(Items_List(ProtoIndex).Value(3)).Name) &
            "/" &
            To_String
              (Attributes_List
                 (Skills_List(Items_List(ProtoIndex).Value(3)).Attribute)
                 .Name));
         if Items_List(ProtoIndex).Value(4) = 1 then
            Append(ItemInfo, LF & "Can be used with shield.");
         else
            Append
              (ItemInfo,
               LF & "Can't be used with shield (two-handed weapon).");
         end if;
         Append(ItemInfo, LF & "Damage type: ");
         case Items_List(ProtoIndex).Value(5) is
            when 1 =>
               Append(ItemInfo, "cutting");
            when 2 =>
               Append(ItemInfo, "impaling");
            when 3 =>
               Append(ItemInfo, "blunt");
            when others =>
               null;
         end case;
      end if;
      for ItemType of ItemTypes loop
         if Items_List(ProtoIndex).IType = ItemType then
            Append
              (ItemInfo,
               LF & "Damage chance: " &
               GetItemChanceToDamage(Items_List(ProtoIndex).Value(1)));
            Append
              (ItemInfo,
               LF & "Strength:" &
               Integer'Image(Items_List(ProtoIndex).Value(2)));
            exit;
         end if;
      end loop;
      if Tools_List.Contains(Items_List(ProtoIndex).IType) then
         Append
           (ItemInfo,
            LF & "Damage chance: " &
            GetItemChanceToDamage(Items_List(ProtoIndex).Value(1)));
      end if;
      if Length(Items_List(ProtoIndex).IType) > 4
        and then
        (Slice(Items_List(ProtoIndex).IType, 1, 4) = "Ammo" or
         Items_List(ProtoIndex).IType = To_Unbounded_String("Harpoon")) then
         Append
           (ItemInfo,
            LF & "Strength:" & Integer'Image(Items_List(ProtoIndex).Value(1)));
      end if;
      if Items_List(ProtoIndex).Description /= Null_Unbounded_String then
         Append
           (ItemInfo, LF & LF & To_String(Items_List(ProtoIndex).Description));
      end if;
      Set_Label
        (Gtk_Label(Get_Object(Object, "lbltradeinfo")), To_String(ItemInfo));
      Hide(Gtk_Widget(Get_Object(Object, "sellbox")));
      Hide(Gtk_Widget(Get_Object(Object, "sellbox2")));
      if CargoIndex > 0 then
         declare
            MaxSellAmount: Integer := PlayerShip.Cargo(CargoIndex).Amount;
            MaxPrice: Natural := MaxSellAmount * Price;
            AmountAdj: constant Gtk_Adjustment :=
              Gtk_Adjustment(Get_Object(Builder, "amountadj"));
            Weight: Integer;
         begin
            Set_Value(AmountAdj, 1.0);
            CountPrice(MaxPrice, FindMember(Talk), False);
            if BaseIndex > 0
              and then MaxPrice > SkyBases(BaseIndex).Cargo(1).Amount then
               MaxSellAmount :=
                 Natural
                   (Float'Floor
                      (Float(MaxSellAmount) *
                       (Float(SkyBases(BaseIndex).Cargo(1).Amount) /
                        Float(MaxPrice))));
            elsif BaseIndex = 0 and then MaxPrice > TraderCargo(1).Amount then
               MaxSellAmount :=
                 Natural
                   (Float'Floor
                      (Float(MaxSellAmount) *
                       (Float(TraderCargo(1).Amount) / Float(MaxPrice))));
            end if;
            MaxPrice := MaxSellAmount * Price;
            if MaxPrice > 0 then
               CountPrice(MaxPrice, FindMember(Talk), False);
            end if;
            Weight :=
              FreeCargo
                ((Items_List(ProtoIndex).Weight * MaxSellAmount) - MaxPrice);
            while Weight < 0 loop
               MaxSellAmount :=
                 Integer
                   (Float'Floor
                      (Float(MaxSellAmount) *
                       (Float(MaxPrice + Weight) / Float(MaxPrice))));
               exit when MaxSellAmount < 1;
               MaxPrice := MaxSellAmount * Price;
               CountPrice(MaxPrice, FindMember(Talk), False);
               Weight :=
                 FreeCargo
                   ((Items_List(ProtoIndex).Weight * MaxSellAmount) -
                    MaxPrice);
            end loop;
            if MaxSellAmount > 0 then
               Set_Upper(AmountAdj, Gdouble(MaxSellAmount));
               Set_Label
                 (Gtk_Label(Get_Object(Builder, "lblsellamount")),
                  "(max" & Natural'Image(MaxSellAmount) & "):");
               Show_All(Gtk_Widget(Get_Object(Object, "sellbox")));
               if MaxSellAmount = PlayerShip.Cargo(CargoIndex).Amount then
                  Show_All(Gtk_Widget(Get_Object(Object, "sellbox2")));
                  CheckAmount(Object);
               end if;
            end if;
         end;
      end if;
      MoneyIndex2 := FindItem(PlayerShip.Cargo, MoneyIndex);
      Hide(Gtk_Widget(Get_Object(Object, "buybox")));
      if BaseCargoIndex > 0 and MoneyIndex2 > 0 and
        Items_List(ProtoIndex).Buyable(BaseType) then
         if BaseIndex > 0
           and then SkyBases(BaseIndex).Cargo(BaseCargoIndex).Amount > 0 then
            Show_All(Gtk_Widget(Get_Object(Object, "buybox")));
         elsif BaseIndex = 0
           and then TraderCargo(BaseCargoIndex).Amount > 0 then
            Show_All(Gtk_Widget(Get_Object(Object, "buybox")));
         end if;
         if Is_Visible(Gtk_Widget(Get_Object(Object, "buybox"))) then
            declare
               MaxBuyAmount: Integer :=
                 PlayerShip.Cargo(MoneyIndex2).Amount / Price;
               MaxPrice: Natural := MaxBuyAmount * Price;
               AmountAdj: constant Gtk_Adjustment :=
                 Gtk_Adjustment(Get_Object(Builder, "amountadj1"));
               Weight: Integer;
            begin
               if MaxBuyAmount > 0 then
                  Set_Value(AmountAdj, 1.0);
                  CountPrice(MaxPrice, FindMember(Talk));
                  if MaxPrice < (MaxBuyAmount * Price) then
                     MaxBuyAmount :=
                       Natural
                         (Float'Floor
                            (Float(MaxBuyAmount) *
                             ((Float(MaxBuyAmount) * Float(Price)) /
                              Float(MaxPrice))));
                  end if;
                  if BaseIndex > 0
                    and then MaxBuyAmount >
                      SkyBases(BaseIndex).Cargo(BaseCargoIndex).Amount then
                     MaxBuyAmount :=
                       SkyBases(BaseIndex).Cargo(BaseCargoIndex).Amount;
                  elsif BaseIndex = 0
                    and then MaxBuyAmount >
                      TraderCargo(BaseCargoIndex).Amount then
                     MaxBuyAmount := TraderCargo(BaseCargoIndex).Amount;
                  end if;
                  MaxPrice := MaxBuyAmount * Price;
                  CountPrice(MaxPrice, FindMember(Talk));
                  Weight :=
                    FreeCargo
                      (MaxPrice -
                       (Items_List(ProtoIndex).Weight * MaxBuyAmount));
                  while Weight < 0 loop
                     MaxBuyAmount :=
                       MaxBuyAmount +
                       (Weight / Items_List(ProtoIndex).Weight) - 1;
                     if MaxBuyAmount < 0 then
                        MaxBuyAmount := 0;
                     end if;
                     exit when MaxBuyAmount = 0;
                     MaxPrice := MaxBuyAmount * Price;
                     CountPrice(MaxPrice, FindMember(Talk));
                     Weight :=
                       FreeCargo
                         (MaxPrice -
                          (Items_List(ProtoIndex).Weight * MaxBuyAmount));
                  end loop;
                  if MaxBuyAmount > 0 then
                     Set_Upper(AmountAdj, Gdouble(MaxBuyAmount));
                     Set_Label
                       (Gtk_Label(Get_Object(Builder, "lblbuyamount")),
                        "(max" & Natural'Image(MaxBuyAmount) & "):");
                  else
                     Hide(Gtk_Widget(Get_Object(Object, "buybox")));
                  end if;
               else
                  Hide(Gtk_Widget(Get_Object(Object, "buybox")));
               end if;
            end;
         end if;
      end if;
      if MoneyIndex2 > 0 then
         Set_Label
           (Gtk_Label(Get_Object(Object, "lblshipmoney")),
            "You have" & Natural'Image(PlayerShip.Cargo(MoneyIndex2).Amount) &
            " " & To_String(MoneyName) & ".");
      else
         Set_Label
           (Gtk_Label(Get_Object(Object, "lblshipmoney")),
            "You don't have any " & To_String(MoneyName) &
            " to buy anything.");
      end if;
      declare
         FreeSpace: Integer := FreeCargo(0);
      begin
         if FreeSpace < 0 then
            FreeSpace := 0;
         end if;
         Set_Label
           (Gtk_Label(Get_Object(Object, "lblshipspace1")),
            "Free cargo space:" & Integer'Image(FreeSpace) & " kg");
      end;
      if BaseIndex > 0 then
         if SkyBases(BaseIndex).Cargo(1).Amount = 0 then
            Set_Label
              (Gtk_Label(Get_Object(Object, "lblbasemoney")),
               "Base don't have any " & To_String(MoneyName) &
               "to buy anything.");
         else
            Set_Label
              (Gtk_Label(Get_Object(Object, "lblbasemoney")),
               "Base have" &
               Positive'Image(SkyBases(BaseIndex).Cargo(1).Amount) & " " &
               To_String(MoneyName) & ".");
         end if;
      else
         if TraderCargo(1).Amount = 0 then
            Set_Label
              (Gtk_Label(Get_Object(Object, "lblbasemoney")),
               "Ship don't have any " & To_String(MoneyName) &
               "to buy anything.");
         else
            Set_Label
              (Gtk_Label(Get_Object(Object, "lblbasemoney")),
               "Ship have" & Positive'Image(TraderCargo(1).Amount) & " " &
               To_String(MoneyName) & ".");
         end if;
      end if;
   end ShowItemTradeInfo;

   -- ****if* Trades.UI/TradeItem
   -- FUNCTION
   -- Buy or sell selected item
   -- PARAMETERS
   -- User_Data - Button clicked
   -- SOURCE
   procedure TradeItem(User_Data: access GObject_Record'Class) is
      -- ****
      ItemsIter: Gtk_Tree_Iter;
      ItemsModel: Gtk_Tree_Model;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseCargoIndex, CargoIndex: Natural := 0;
      Trader: String(1 .. 4);
      Amount: Natural;
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Builder, "treeitems1"))),
         ItemsModel, ItemsIter);
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
         Amount :=
           Natural
             (Get_Value(Gtk_Adjustment(Get_Object(Builder, "amountadj1"))));
         BuyItems(BaseCargoIndex, Natural'Image(Amount));
      else
         Amount :=
           Natural
             (Get_Value(Gtk_Adjustment(Get_Object(Builder, "amountadj"))));
         if User_Data = Get_Object(Builder, "btnsellitem") then
            SellItems(CargoIndex, Natural'Image(Amount));
         else
            SellItems
              (CargoIndex,
               Positive'Image(PlayerShip.Cargo.Element(CargoIndex).Amount));
         end if;
      end if;
      ShowTradeUI;
      UpdateHeader;
   exception
      when An_Exception : Trade_Cant_Buy =>
         ShowDialog
           ("You can't buy " & Exception_Message(An_Exception) & " in this " &
            Trader & ".");
      when An_Exception : Trade_Not_For_Sale_Now =>
         ShowDialog
           ("You can't buy " & Exception_Message(An_Exception) &
            " in this base at this moment.");
      when An_Exception : Trade_Buying_Too_Much =>
         ShowDialog
           (Trader & " don't have that much " &
            Exception_Message(An_Exception) & " for sale.");
      when Trade_No_Free_Cargo =>
         ShowDialog("You don't have that much free space in your ship cargo.");
      when An_Exception : Trade_No_Money =>
         ShowDialog
           ("You don't have any " & To_String(MoneyName) & " to buy " &
            Exception_Message(An_Exception) & ".");
      when An_Exception : Trade_Not_Enough_Money =>
         ShowDialog
           ("You don't have enough " & To_String(MoneyName) &
            " to buy so much " & Exception_Message(An_Exception) & ".");
      when Trade_Invalid_Amount =>
         if User_Data = Get_Object(Builder, "btnbuyitem") then
            ShowDialog("You entered invalid amount to buy.");
         else
            ShowDialog("You entered invalid amount to sell.");
         end if;
      when An_Exception : Trade_Too_Much_For_Sale =>
         ShowDialog
           ("You dont have that much " & Exception_Message(An_Exception) &
            " in ship cargo.");
      when An_Exception : Trade_No_Money_In_Base =>
         ShowDialog
           ("You can't sell so much " & Exception_Message(An_Exception) &
            " because " & Trader & " don't have that much " &
            To_String(MoneyName) & " to buy it.");
      when Trade_No_Trader =>
         ShowDialog
           ("You don't have assigned anyone in crew to talk in bases duty.");
   end TradeItem;

   -- ****if* Trades.UI/SearchTrade
   -- FUNCTION
   -- Search item to trade by its name
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure SearchTrade(Object: access Gtkada_Builder_Record'Class) is
   -- ****
   begin
      Refilter(Gtk_Tree_Model_Filter(Get_Object(Object, "tradefilter")));
      if N_Children
          (Gtk_List_Store(Get_Object(Builder, "itemslist1")), Null_Iter) >
        0 then
         Set_Cursor
           (Gtk_Tree_View(Get_Object(Builder, "treeitems1")),
            Gtk_Tree_Path_New_From_String("0"), null, False);
      end if;
   end SearchTrade;

   -- ****if* Trades.UI/VisibleTrade
   -- FUNCTION
   -- Check if selected item is visible on trade list
   -- PARAMETERS
   -- Model - Gtk_Tree_Model with list of items to check
   -- Iter  - Gtk_Tree_Iter of selected item to check
   -- RESULT
   -- True if selected item should be visible, otherwise false
   -- SOURCE
   function VisibleTrade
     (Model: Gtk_Tree_Model; Iter: Gtk_Tree_Iter) return Boolean is
      -- ****
      SearchEntry: constant Gtk_GEntry :=
        Gtk_GEntry(Get_Object(Builder, "tradesearch"));
      IType: constant Unbounded_String :=
        To_Unbounded_String
          (Get_Active_Text
             (Gtk_Combo_Box_Text(Get_Object(Builder, "cmbtradetype"))));
      ProtoIndex: Unbounded_String;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseCargo: BaseCargo_Container.Vector;
      ShowItem: Boolean := False;
   begin
      if SettingTime then
         return True;
      end if;
      if IType = To_Unbounded_String("All") then
         ShowItem := True;
      else
         if Get_Int(Model, Iter, 1) > 0 then
            ProtoIndex :=
              PlayerShip.Cargo(Positive(Get_Int(Model, Iter, 1))).ProtoIndex;
         else
            if BaseIndex > 0 then
               BaseCargo := SkyBases(BaseIndex).Cargo;
            else
               BaseCargo := TraderCargo;
            end if;
            ProtoIndex :=
              BaseCargo(Positive(Get_Int(Model, Iter, 2))).ProtoIndex;
         end if;
         if Items_List(ProtoIndex).IType = IType or
           Items_List(ProtoIndex).ShowType = IType then
            ShowItem := True;
         end if;
      end if;
      if Get_Text(SearchEntry) = "" then
         return ShowItem;
      end if;
      if Index
          (To_Lower(Get_String(Model, Iter, 0)),
           To_Lower(Get_Text(SearchEntry)), 1) >
        0 and
        ShowItem then
         return True;
      end if;
      return False;
   end VisibleTrade;

   procedure CreateTradeUI is
   begin
      Register_Handler
        (Builder, "Show_Item_Trade_Info", ShowItemTradeInfo'Access);
      Register_Handler(Builder, "Trade_Item", TradeItem'Access);
      Register_Handler(Builder, "Close_Trade", CloseTrade'Access);
      Register_Handler(Builder, "Search_Trade", SearchTrade'Access);
      Register_Handler(Builder, "Check_Amount", CheckAmount'Access);
      On_Key_Press_Event
        (Gtk_Widget(Get_Object(Builder, "spintradebuy")), SelectElement'Access,
         Get_Object(Builder, "btnbuyitem"));
      On_Key_Press_Event
        (Gtk_Widget(Get_Object(Builder, "spintradesell")),
         SelectElement'Access, Get_Object(Builder, "btnsellitem"));
      Set_Visible_Func
        (Gtk_Tree_Model_Filter(Get_Object(Builder, "tradefilter")),
         VisibleTrade'Access);
      On_Key_Press_Event
        (Gtk_Widget(Get_Object(Builder, "tradesearch")), SelectElement'Access,
         Get_Object(Builder, "btnmenu"));
   end CreateTradeUI;

   procedure ShowTradeUI is
      ItemsIter: Gtk_Tree_Iter;
      ItemsList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "itemslist1"));
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseType, Price: Positive;
      IndexesList: Positive_Container.Vector;
      BaseCargoIndex: Natural;
      BaseCargo: BaseCargo_Container.Vector;
      Visible: Boolean := False;
      EventIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex;
      ProtoIndex: Unbounded_String;
      ItemsTypes: UnboundedString_Container.Vector;
      Profit: Integer;
      procedure AddType is
      begin
         if not ItemsTypes.Contains(Items_List(ProtoIndex).IType) and
           not ItemsTypes.Contains(Items_List(ProtoIndex).ShowType) then
            if Items_List(ProtoIndex).ShowType = Null_Unbounded_String then
               ItemsTypes.Append(Items_List(ProtoIndex).IType);
            else
               ItemsTypes.Append(Items_List(ProtoIndex).ShowType);
            end if;
         end if;
      end AddType;
   begin
      ItemsTypes.Append(To_Unbounded_String("All"));
      if BaseIndex > 0 then
         BaseType := Bases_Types'Pos(SkyBases(BaseIndex).BaseType) + 1;
         BaseCargo := SkyBases(BaseIndex).Cargo;
      else
         BaseType := 1;
         BaseCargo := TraderCargo;
      end if;
      SettingTime := True;
      Clear(ItemsList);
      for I in PlayerShip.Cargo.Iterate loop
         if Items_List(PlayerShip.Cargo(I).ProtoIndex).Prices(BaseType) >
           0 then
            Append(ItemsList, ItemsIter);
            Set
              (ItemsList, ItemsIter, 0,
               GetItemName(PlayerShip.Cargo(I), False));
            Set
              (ItemsList, ItemsIter, 1, Gint(Inventory_Container.To_Index(I)));
            ProtoIndex := PlayerShip.Cargo(I).ProtoIndex;
            BaseCargoIndex :=
              FindBaseCargo(ProtoIndex, PlayerShip.Cargo(I).Durability);
            if BaseCargoIndex > 0 then
               IndexesList.Append(New_Item => BaseCargoIndex);
            end if;
            Set(ItemsList, ItemsIter, 2, Gint(BaseCargoIndex));
            if Items_List(ProtoIndex).ShowType = Null_Unbounded_String then
               Set
                 (ItemsList, ItemsIter, 3,
                  To_String(Items_List(ProtoIndex).IType));
            else
               Set
                 (ItemsList, ItemsIter, 3,
                  To_String(Items_List(ProtoIndex).ShowType));
            end if;
            AddType;
            if PlayerShip.Cargo(I).Durability < 100 then
               Set
                 (ItemsList, ItemsIter, 9,
                  " " & GetItemDamage(PlayerShip.Cargo(I).Durability) & " ");
               Set(ItemsList, ItemsIter, 10, True);
               Visible := True;
            end if;
            Set(ItemsList, ItemsIter, 4, Gint(PlayerShip.Cargo(I).Durability));
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
               if Events_List(EventIndex).EType = DoublePrice
                 and then Events_List(EventIndex).ItemIndex = ProtoIndex then
                  Price := Price * 2;
               end if;
            end if;
            Set(ItemsList, ItemsIter, 5, Gint(Price));
            Profit := Price - PlayerShip.Cargo(I).Price;
            Set(ItemsList, ItemsIter, 6, Gint(Profit));
            if Profit < 0 then
               Set(ItemsList, ItemsIter, 11, "red");
            elsif Profit > 0 then
               Set(ItemsList, ItemsIter, 11, "green");
            end if;
            Set(ItemsList, ItemsIter, 7, Gint(PlayerShip.Cargo(I).Amount));
            if BaseCargoIndex > 0 and
              Items_List(ProtoIndex).Buyable(BaseType) then
               if BaseIndex = 0 then
                  Set
                    (ItemsList, ItemsIter, 8,
                     Gint(TraderCargo(BaseCargoIndex).Amount));
               else
                  Set
                    (ItemsList, ItemsIter, 8,
                     Gint(SkyBases(BaseIndex).Cargo(BaseCargoIndex).Amount));
               end if;
            end if;
         end if;
      end loop;
      Set_Visible
        (Gtk_Tree_View_Column(Get_Object(Builder, "columntradedurability")),
         Visible);
      for I in BaseCargo.First_Index .. BaseCargo.Last_Index loop
         if IndexesList.Find_Index(Item => I) = 0 and
           Items_List(BaseCargo(I).ProtoIndex).Buyable(BaseType) then
            Append(ItemsList, ItemsIter);
            ProtoIndex := BaseCargo(I).ProtoIndex;
            Set
              (ItemsList, ItemsIter, 0,
               To_String(Items_List(ProtoIndex).Name));
            Set(ItemsList, ItemsIter, 1, 0);
            Set(ItemsList, ItemsIter, 2, Gint(I));
            if Items_List(ProtoIndex).ShowType = Null_Unbounded_String then
               Set
                 (ItemsList, ItemsIter, 3,
                  To_String(Items_List(ProtoIndex).IType));
            else
               Set
                 (ItemsList, ItemsIter, 3,
                  To_String(Items_List(ProtoIndex).ShowType));
            end if;
            AddType;
            if BaseCargo(I).Durability < 100 then
               Set
                 (ItemsList, ItemsIter, 9,
                  " " & GetItemDamage(BaseCargo(I).Durability) & " ");
               Set(ItemsList, ItemsIter, 10, True);
               Visible := True;
            end if;
            Set(ItemsList, ItemsIter, 4, Gint(BaseCargo(I).Durability));
            if BaseIndex > 0 then
               Price := SkyBases(BaseIndex).Cargo(I).Price;
            else
               Price := TraderCargo(I).Price;
            end if;
            if EventIndex > 0 then
               if Events_List(EventIndex).EType = DoublePrice
                 and then Events_List(EventIndex).ItemIndex = ProtoIndex then
                  Price := Price * 2;
               end if;
            end if;
            Set(ItemsList, ItemsIter, 5, Gint(Price));
            Set(ItemsList, ItemsIter, 6, Gint(0 - Price));
            Set(ItemsList, ItemsIter, 11, "red");
            if BaseIndex = 0 then
               Set(ItemsList, ItemsIter, 8, Gint(TraderCargo(I).Amount));
            else
               Set
                 (ItemsList, ItemsIter, 8,
                  Gint(SkyBases(BaseIndex).Cargo(I).Amount));
            end if;
         end if;
      end loop;
      declare
         TypesCombo: constant Gtk_Combo_Box_Text :=
           Gtk_Combo_Box_Text(Get_Object(Builder, "cmbtradetype"));
      begin
         Remove_All(TypesCombo);
         for IType of ItemsTypes loop
            Append_Text(TypesCombo, To_String(IType));
         end loop;
         Set_Active(TypesCombo, 0);
      end;
      Set_Text(Gtk_GEntry(Get_Object(Builder, "tradesearch")), "");
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")), "trade");
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treeitems1")),
         Gtk_Tree_Path_New_From_String("0"), null, False);
      UpdateMessages;
      Hide(Gtk_Widget(Get_Object(Builder, "lblsellwarning")));
      SettingTime := False;
   end ShowTradeUI;

end Trades.UI;
