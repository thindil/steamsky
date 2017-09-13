--    Copyright 2016-2017 Bartek thindil Jasicki
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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Terminal_Interface.Curses.Forms.Field_Types.IntField;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with Maps; use Maps;
with Items; use Items;
with UserInterface; use UserInterface;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Ships.UI.Cargo; use Ships.UI.Cargo;
with Help.UI; use Help.UI;
with Events; use Events;
with Header; use Header;
with Utils.UI; use Utils.UI;
with Bases.UI; use Bases.UI;

package body Trades.UI is

   Buy: Boolean;
   TradeForm: Form;
   FormWindow: Window;

   procedure ShowItemInfo is
      ItemIndex, Price: Positive;
      InfoWindow, ClearWindow, BoxWindow: Window;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseType: Positive;
      CurrentLine: Line_Position := 4;
      StartColumn, WindowWidth: Column_Position;
      EventIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex;
      WindowHeight: Line_Position := 7;
      MoneyIndex2, BaseItemIndex, BaseItemIndex2, CargoIndex: Natural := 0;
      FreeSpace: Integer;
      PriceText: Unbounded_String;
      Buyable: Boolean := False;
   begin
      ClearWindow := Create(Lines - 3, (Columns / 2), 3, (Columns / 2));
      Refresh_Without_Update(ClearWindow);
      Delete(ClearWindow);
      for I in Items_List.Iterate loop
         if To_String(Items_List(I).Name) = Name(Current(TradeMenu)) then
            ItemIndex := Objects_Container.To_Index(I);
            exit;
         end if;
      end loop;
      if BaseIndex > 0 then
         BaseType := Bases_Types'Pos(SkyBases(BaseIndex).BaseType) + 1;
      else
         BaseType := 1;
      end if;
      if Integer'Value(Description(Current(TradeMenu))) > 0 then
         CargoIndex := Integer'Value(Description(Current(TradeMenu)));
         if BaseIndex > 0 then
            BaseItemIndex :=
              FindBaseCargo
                (ItemIndex,
                 PlayerShip.Cargo(CargoIndex).Durability);
         else
            for I in TraderCargo.Iterate loop
               if TraderCargo(I).ProtoIndex = ItemIndex and
                 TraderCargo(I).Durability =
                   PlayerShip.Cargo(CargoIndex).Durability then
                  BaseItemIndex := BaseCargo_Container.To_Index(I);
                  exit;
               end if;
            end loop;
         end if;
      else
         BaseItemIndex :=
           Integer'Value(Description(Current(TradeMenu))) * (-1);
         if BaseIndex > 0 then
            CargoIndex :=
              FindItem
                (Inventory => PlayerShip.Cargo,
                 ProtoIndex => ItemIndex,
                 Durability =>
                   SkyBases(BaseIndex).Cargo(BaseItemIndex).Durability);
         else
            CargoIndex :=
              FindItem
                (Inventory => PlayerShip.Cargo,
                 ProtoIndex => ItemIndex,
                 Durability => TraderCargo(BaseItemIndex).Durability);
         end if;
      end if;
      if CargoIndex > 0 then
         WindowHeight := WindowHeight + 1;
         if PlayerShip.Cargo(CargoIndex).Durability < 100 then
            WindowHeight := WindowHeight + 1;
         end if;
      end if;
      if
        (BaseIndex > 0 and
         BaseItemIndex > 0 and
         Items_List(ItemIndex).Buyable(BaseType)) or
        (BaseIndex = 0 and BaseItemIndex > 0) then
         WindowHeight := WindowHeight + 1;
      end if;
      WindowHeight :=
        WindowHeight +
        Line_Position
          ((Length(Items_List(ItemIndex).Description) /
            (Natural(Columns / 2) - 4)));
      if Length(Items_List(ItemIndex).Description) >=
        (Natural(Columns / 2) - 4) then
         WindowWidth := Columns / 2;
      else
         WindowWidth :=
           Column_Position(Length(Items_List(ItemIndex).Description)) + 4;
      end if;
      BoxWindow := Create(WindowHeight, (Columns / 2), 3, (Columns / 2));
      InfoWindow :=
        Create(WindowHeight - 2, (Columns / 2) - 4, 4, (Columns / 2) + 2);
      Add(Win => InfoWindow, Str => "Type: ");
      if Items_List(ItemIndex).ShowType = Null_Unbounded_String then
         Add(Win => InfoWindow, Str => To_String(Items_List(ItemIndex).IType));
      else
         Add
           (Win => InfoWindow,
            Str => To_String(Items_List(ItemIndex).ShowType));
      end if;
      Move_Cursor(Win => InfoWindow, Line => 1, Column => 0);
      if Items_List(ItemIndex).Buyable(BaseType) then
         PriceText := To_Unbounded_String("Base buy/sell price:");
      else
         PriceText := To_Unbounded_String("Base sell price:");
      end if;
      if BaseIndex > 0 then
         BaseItemIndex2 := FindBaseCargo(ItemIndex);
      else
         for I in TraderCargo.Iterate loop
            if TraderCargo(I).ProtoIndex = ItemIndex then
               BaseItemIndex2 := BaseCargo_Container.To_Index(I);
               exit;
            end if;
         end loop;
      end if;
      if BaseItemIndex2 = 0 then
         Price := Items_List(ItemIndex).Prices(BaseType);
      else
         if BaseIndex > 0 then
            Price := SkyBases(BaseIndex).Cargo(BaseItemIndex2).Price;
         else
            Price := TraderCargo(BaseItemIndex2).Price;
         end if;
      end if;
      if EventIndex > 0 then
         if Events_List(EventIndex).EType = DoublePrice and
           Events_List(EventIndex).Data = ItemIndex then
            Price := Price * 2;
         end if;
      end if;
      Append(PriceText, Integer'Image(Price));
      Append(PriceText, " ");
      Append(PriceText, MoneyName);
      if WindowWidth < Column_Position(Length(PriceText) + 4) then
         WindowWidth := Column_Position(Length(PriceText) + 4);
      end if;
      Add(Win => InfoWindow, Str => To_String(PriceText));
      Move_Cursor(Win => InfoWindow, Line => 2, Column => 0);
      Add
        (Win => InfoWindow,
         Str =>
           "Weight:" & Integer'Image(Items_List(ItemIndex).Weight) & " kg");
      if CargoIndex > 0 then
         Move_Cursor(Win => InfoWindow, Line => 3, Column => 0);
         Add
           (Win => InfoWindow,
            Str =>
              "Owned:" & Integer'Image(PlayerShip.Cargo(CargoIndex).Amount));
         if PlayerShip.Cargo(CargoIndex).Durability < 100 then
            Move_Cursor(Win => InfoWindow, Line => 4, Column => 0);
            Add(Win => InfoWindow, Str => "Status: ");
            ShowCargoStatus(CargoIndex, InfoWindow, 4);
            CurrentLine := 5;
         end if;
         CurrentLine := CurrentLine + 1;
      end if;
      if BaseIndex > 0 and
        BaseItemIndex > 0 and
        Items_List(ItemIndex).Buyable(BaseType) then
         Move_Cursor(Win => InfoWindow, Line => CurrentLine - 1, Column => 0);
         Add
           (Win => InfoWindow,
            Str =>
              "In base:" &
              Integer'Image(SkyBases(BaseIndex).Cargo(BaseItemIndex).Amount));
         CurrentLine := CurrentLine + 1;
      elsif BaseIndex = 0 and BaseItemIndex > 0 then
         Move_Cursor(Win => InfoWindow, Line => CurrentLine - 1, Column => 0);
         Add
           (Win => InfoWindow,
            Str =>
              "In ship:" & Integer'Image(TraderCargo(BaseItemIndex).Amount));
         CurrentLine := CurrentLine + 1;
      end if;
      if Items_List(ItemIndex).Description /= Null_Unbounded_String then
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
         Add
           (Win => InfoWindow,
            Str => To_String(Items_List(ItemIndex).Description));
         Get_Cursor_Position
           (Win => InfoWindow,
            Line => CurrentLine,
            Column => StartColumn);
      end if;
      Resize(BoxWindow, WindowHeight, WindowWidth);
      WindowFrame(BoxWindow, 2, "Item info");
      Resize(InfoWindow, WindowHeight - 2, WindowWidth - 4);
      CurrentLine := WindowHeight + 3;
      if BaseIndex > 0 and BaseItemIndex > 0 then
         if Items_List(ItemIndex).Buyable(BaseType) and
           SkyBases(BaseIndex).Cargo(BaseItemIndex).Amount > 0 then
            Buyable := True;
         end if;
      elsif BaseIndex = 0 and BaseItemIndex > 0 then
         if TraderCargo(BaseItemIndex).Amount > 0 then
            Buyable := True;
         end if;
      end if;
      Move_Cursor(Line => CurrentLine, Column => (Columns / 2));
      if
        ((BaseIndex > 0 and Buyable) or
         (BaseIndex = 0 and BaseItemIndex > 0)) and
        CargoIndex > 0 then
         Add(Str => "Press Enter to buy, Space for sell.");
         Change_Attributes
           (Line => CurrentLine,
            Column => (Columns / 2) + 6,
            Count => 5,
            Color => 1,
            Attr => BoldCharacters);
         Change_Attributes
           (Line => CurrentLine,
            Column => (Columns / 2) + 20,
            Count => 5,
            Color => 1,
            Attr => BoldCharacters);
         CurrentLine := CurrentLine + 1;
      elsif
        ((BaseIndex > 0 and Buyable) or
         (BaseIndex = 0 and BaseItemIndex > 0)) and
        CargoIndex = 0 then
         Add(Str => "Press Enter to buy.");
         Change_Attributes
           (Line => CurrentLine,
            Column => (Columns / 2) + 6,
            Count => 5,
            Color => 1,
            Attr => BoldCharacters);
         CurrentLine := CurrentLine + 1;
      elsif
        ((BaseIndex > 0 and not Buyable) or
         (BaseIndex = 0 and BaseItemIndex = 0)) and
        CargoIndex > 0 then
         Add(Str => "Press Space for sell.");
         Change_Attributes
           (Line => CurrentLine,
            Column => (Columns / 2) + 6,
            Count => 5,
            Color => 1,
            Attr => BoldCharacters);
         CurrentLine := CurrentLine + 1;
      end if;
      Move_Cursor(Line => CurrentLine, Column => (Columns / 2));
      Add(Str => "Press Escape to back to sky map");
      Change_Attributes
        (Line => CurrentLine,
         Column => (Columns / 2) + 6,
         Count => 6,
         Color => 1,
         Attr => BoldCharacters);
      CurrentLine := CurrentLine + 1;
      Move_Cursor(Line => CurrentLine, Column => (Columns / 2));
      Add(Str => "Press F1 for help");
      Change_Attributes
        (Line => CurrentLine,
         Column => (Columns / 2) + 6,
         Count => 2,
         Color => 1,
         Attr => BoldCharacters);
      CurrentLine := CurrentLine + 1;
      MoneyIndex2 := FindItem(PlayerShip.Cargo, FindProtoItem(MoneyIndex));
      Move_Cursor(Line => CurrentLine, Column => (Columns / 2));
      if MoneyIndex2 > 0 then
         Add
           (Str =>
              "You have" &
              Natural'Image(PlayerShip.Cargo(MoneyIndex2).Amount) &
              " " &
              To_String(MoneyName) &
              ".");
      else
         Add
           (Str =>
              "You don't have any " &
              To_String(MoneyName) &
              " to buy anything.");
      end if;
      CurrentLine := CurrentLine + 1;
      Move_Cursor(Line => CurrentLine, Column => (Columns / 2));
      FreeSpace := FreeCargo(0);
      if FreeSpace < 0 then
         FreeSpace := 0;
      end if;
      Add(Str => "Free cargo space:" & Integer'Image(FreeSpace) & " kg");
      CurrentLine := CurrentLine + 1;
      Move_Cursor(Line => CurrentLine, Column => (Columns / 2));
      if BaseIndex > 0 then
         if SkyBases(BaseIndex).Cargo(1).Amount = 0 then
            Add
              (Str =>
                 "Base don't have any " &
                 To_String(MoneyName) &
                 "to buy anything.");
         else
            Add
              (Str =>
                 "Base have" &
                 Positive'Image(SkyBases(BaseIndex).Cargo(1).Amount) &
                 " " &
                 To_String(MoneyName) &
                 ".");
         end if;
      else
         if TraderCargo(1).Amount = 0 then
            Add
              (Str =>
                 "Ship don't have any " &
                 To_String(MoneyName) &
                 "to buy anything.");
         else
            Add
              (Str =>
                 "Ship have" &
                 Positive'Image(TraderCargo(1).Amount) &
                 " " &
                 To_String(MoneyName) &
                 ".");
         end if;
      end if;
      Refresh_Without_Update;
      Refresh_Without_Update(BoxWindow);
      Delete(BoxWindow);
      Refresh_Without_Update(InfoWindow);
      Delete(InfoWindow);
      Refresh_Without_Update(Bases.UI.MenuWindow);
      Update_Screen;
   end ShowItemInfo;

   procedure ShowTrade is
      Trade_Items: Item_Array_Access;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseType: Positive;
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      MenuIndex: Integer := 1;
      ItemsAmount: Positive := 1;
      BaseItemIndex: Positive;
   begin
      if BaseIndex > 0 then
         BaseType := Bases_Types'Pos(SkyBases(BaseIndex).BaseType) + 1;
      else
         BaseType := 1;
      end if;
      for Item of PlayerShip.Cargo loop
         if Items_List(Item.ProtoIndex).Prices(BaseType) > 0 then
            ItemsAmount := ItemsAmount + 1;
         end if;
      end loop;
      if BaseIndex > 0 then
         for I in Items_List.Iterate loop
            if Items_List(I).Buyable(BaseType) and
              FindItem(PlayerShip.Cargo, Objects_Container.To_Index(I)) =
                0 then
               ItemsAmount := ItemsAmount + 1;
            end if;
         end loop;
      else
         for Item of TraderCargo loop
            if FindItem(PlayerShip.Cargo, Item.ProtoIndex) = 0 and
              Item.Price > 0 then
               ItemsAmount := ItemsAmount + 1;
            end if;
         end loop;
      end if;
      Trade_Items := new Item_Array(1 .. ItemsAmount);
      for I in PlayerShip.Cargo.Iterate loop
         if Items_List(PlayerShip.Cargo(I).ProtoIndex).Prices(BaseType) >
           0 then
            Trade_Items.all(MenuIndex) :=
              New_Item
                (To_String(Items_List(PlayerShip.Cargo(I).ProtoIndex).Name),
                 Positive'Image(Inventory_Container.To_Index(I)));
            MenuIndex := MenuIndex + 1;
         end if;
      end loop;
      if BaseIndex > 0 then
         for I in Items_List.Iterate loop
            if Items_List(I).Buyable(BaseType) and
              FindItem(PlayerShip.Cargo, Objects_Container.To_Index(I)) =
                0 then
               BaseItemIndex := FindBaseCargo(Objects_Container.To_Index(I));
               Trade_Items.all(MenuIndex) :=
                 New_Item
                   (To_String(Items_List(I).Name),
                    Integer'Image(BaseItemIndex * (-1)));
               MenuIndex := MenuIndex + 1;
            end if;
         end loop;
      else
         for I in TraderCargo.Iterate loop
            if FindItem(PlayerShip.Cargo, TraderCargo(I).ProtoIndex) = 0 and
              TraderCargo(I).Price > 0 then
               BaseItemIndex := BaseCargo_Container.To_Index(I);
               Trade_Items.all(MenuIndex) :=
                 New_Item
                   (To_String(Items_List(TraderCargo(I).ProtoIndex).Name),
                    Integer'Image(BaseItemIndex * (-1)));
               MenuIndex := MenuIndex + 1;
            end if;
         end loop;
      end if;
      Trade_Items.all(MenuIndex) := Null_Item;
      TradeMenu := New_Menu(Trade_Items);
      Set_Options(TradeMenu, (Show_Descriptions => False, others => True));
      Set_Format(TradeMenu, Lines - 3, 1);
      Scale(TradeMenu, MenuHeight, MenuLength);
      Bases.UI.MenuWindow := Create(MenuHeight, MenuLength, 3, 2);
      Set_Window(TradeMenu, Bases.UI.MenuWindow);
      Set_Sub_Window
        (TradeMenu,
         Derived_Window(Bases.UI.MenuWindow, MenuHeight, MenuLength, 0, 0));
      Post(TradeMenu);
      if Trade_Items.all(CurrentMenuIndex) = Null_Item then
         CurrentMenuIndex := 1;
      end if;
      Set_Current(TradeMenu, Trade_Items.all(CurrentMenuIndex));
      ShowItemInfo;
   end ShowTrade;

   function ShowTradeForm return GameStates is
      Trade_Fields: constant Field_Array_Access := new Field_Array(1 .. 6);
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseType: Positive;
      FieldOptions: Field_Option_Set;
      FormHeight: Line_Position;
      FormLength: Column_Position;
      Visibility: Cursor_Visibility := Normal;
      ItemIndex, Price: Positive;
      MaxAmount: Natural := 0;
      FieldText: Unbounded_String := To_Unbounded_String("Enter amount");
      EventIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex;
      CaptionText: Unbounded_String;
      MoneyIndex2: constant Natural :=
        FindItem(PlayerShip.Cargo, FindProtoItem(MoneyIndex));
      BaseItemIndex: Natural := 0;
      CargoIndex: Integer;
   begin
      for I in Items_List.Iterate loop
         if To_String(Items_List(I).Name) = Name(Current(TradeMenu)) then
            ItemIndex := Objects_Container.To_Index(I);
            exit;
         end if;
      end loop;
      if BaseIndex > 0 then
         BaseType := Bases_Types'Pos(SkyBases(BaseIndex).BaseType) + 1;
         BaseItemIndex := FindBaseCargo(ItemIndex);
      else
         BaseType := 1;
         for I in TraderCargo.Iterate loop
            if TraderCargo(I).ProtoIndex = ItemIndex then
               BaseItemIndex := BaseCargo_Container.To_Index(I);
               exit;
            end if;
         end loop;
      end if;
      if BaseItemIndex = 0 then
         Price := Items_List(ItemIndex).Prices(BaseType);
      else
         if BaseIndex > 0 then
            Price := SkyBases(BaseIndex).Cargo(BaseItemIndex).Price;
         else
            Price := TraderCargo(BaseItemIndex).Price;
         end if;
      end if;
      if EventIndex > 0 then
         if Events_List(EventIndex).EType = DoublePrice and
           Events_List(EventIndex).Data = ItemIndex then
            Price := Price * 2;
         end if;
      end if;
      if Buy then
         if BaseIndex > 0 then
            if not Items_List(ItemIndex).Buyable(BaseType) then
               ShowDialog
                 ("You can't buy " &
                  To_String(Items_List(ItemIndex).Name) &
                  " in this base.");
               DrawGame(Trade_View);
               return Trade_View;
            elsif BaseItemIndex > 0 then
               if SkyBases(BaseIndex).Cargo(BaseItemIndex).Amount = 0 then
                  ShowDialog
                    ("Base don't have any " &
                     To_String(Items_List(ItemIndex).Name) &
                     " for sale at this moment.");
                  DrawGame(Trade_View);
                  return Trade_View;
               end if;
            end if;
         elsif BaseIndex = 0 and BaseItemIndex = 0 then
            ShowDialog
              ("You can't buy " &
               To_String(Items_List(ItemIndex).Name) &
               " from this ship.");
            DrawGame(Trade_View);
            return Trade_View;
         end if;
         if MoneyIndex2 = 0 then
            ShowDialog
              ("You can't buy " &
               To_String(Items_List(ItemIndex).Name) &
               " because you don't have any " &
               To_String(MoneyName) &
               ".");
            DrawGame(Trade_View);
            return Trade_View;
         end if;
         MaxAmount := PlayerShip.Cargo(MoneyIndex2).Amount / Price;
         Append(FieldText, " to buy");
         CaptionText := To_Unbounded_String("Buying ");
         Append(CaptionText, Items_List(ItemIndex).Name);
      else
         CargoIndex := Integer'Value(Description(Current(TradeMenu)));
         if CargoIndex < 1 then
            ShowDialog
              ("You don't have any " &
               To_String(Items_List(ItemIndex).Name) &
               " for sale.");
            DrawGame(Trade_View);
            return Trade_View;
         end if;
         MaxAmount := PlayerShip.Cargo(CargoIndex).Amount;
         Append(FieldText, " to sell");
         CaptionText := To_Unbounded_String("Selling ");
         Append(CaptionText, Items_List(ItemIndex).Name);
      end if;
      Append(FieldText, " (max" & Natural'Image(MaxAmount) & "): ");
      if TradeForm = Null_Form then
         Set_Cursor_Visibility(Visibility);
         Trade_Fields.all(1) :=
           New_Field(1, Column_Position(Length(FieldText)), 0, 0, 0, 0);
         FieldOptions := Get_Options(Trade_Fields.all(1));
         Set_Buffer(Trade_Fields.all(1), 0, To_String(FieldText));
         FieldOptions.Active := False;
         Set_Options(Trade_Fields.all(1), FieldOptions);
         Trade_Fields.all(2) :=
           New_Field(1, 20, 0, Column_Position(Length(FieldText)), 0, 0);
         FieldOptions := Get_Options(Trade_Fields.all(2));
         FieldOptions.Auto_Skip := False;
         Set_Options(Trade_Fields.all(2), FieldOptions);
         Set_Foreground(Trade_Fields.all(2), BoldCharacters, 11);
         Set_Background(Trade_Fields.all(2), BoldCharacters, 11);
         Terminal_Interface.Curses.Forms.Field_Types.IntField.Set_Field_Type
           (Trade_Fields.all(2),
            (0, 0, MaxAmount));
         Trade_Fields.all(3) :=
           New_Field(1, 8, 2, (Column_Position(Length(FieldText)) / 2), 0, 0);
         Set_Buffer(Trade_Fields.all(3), 0, "[Cancel]");
         FieldOptions := Get_Options(Trade_Fields.all(3));
         FieldOptions.Edit := False;
         Set_Options(Trade_Fields.all(3), FieldOptions);
         Trade_Fields.all(4) :=
           New_Field
             (1,
              4,
              2,
              (Column_Position(Length(FieldText)) / 2) + 10,
              0,
              0);
         FieldOptions := Get_Options(Trade_Fields.all(4));
         FieldOptions.Edit := False;
         Set_Options(Trade_Fields.all(4), FieldOptions);
         Set_Buffer(Trade_Fields.all(4), 0, "[Ok]");
         if Buy then
            Trade_Fields.all(5) := Null_Field;
         else
            Trade_Fields.all(5) :=
              New_Field
                (1,
                 10,
                 2,
                 (Column_Position(Length(FieldText)) / 2) + 16,
                 0,
                 0);
            FieldOptions := Get_Options(Trade_Fields.all(5));
            FieldOptions.Edit := False;
            Set_Options(Trade_Fields.all(5), FieldOptions);
            Set_Buffer(Trade_Fields.all(5), 0, "[Sell all]");
         end if;
         Trade_Fields.all(6) := Null_Field;
         TradeForm := New_Form(Trade_Fields);
         Scale(TradeForm, FormHeight, FormLength);
         FormWindow :=
           Create
             (FormHeight + 2,
              FormLength + 2,
              ((Lines / 3) - (FormHeight / 2)),
              ((Columns / 2) - (FormLength / 2)));
         Box(FormWindow);
         WindowFrame(FormWindow, 5, To_String(CaptionText));
         Set_Window(TradeForm, FormWindow);
         Set_Sub_Window
           (TradeForm,
            Derived_Window(FormWindow, FormHeight, FormLength, 1, 1));
         Post(TradeForm);
      end if;
      Refresh_Without_Update;
      Refresh_Without_Update(FormWindow);
      Update_Screen;
      return Trade_Form;
   end ShowTradeForm;

   function TradeResult return GameStates is
      Visibility: Cursor_Visibility := Invisible;
      FieldIndex: constant Positive := Get_Index(Current(TradeForm));
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseItemIndex, CargoIndex: Natural := 0;
      Trader: String(1 .. 4);
      procedure ShowErrorMessage(Message: String) is
      begin
         Set_Cursor_Visibility(Visibility);
         Post(TradeForm, False);
         Delete(TradeForm);
         ShowDialog(Message);
         DrawGame(Trade_View);
      end ShowErrorMessage;
   begin
      if BaseIndex > 0 then
         Trader := "base";
      else
         Trader := "ship";
      end if;
      if FieldIndex < 3 then
         return Trade_Form;
      elsif FieldIndex > 3 then
         if Integer'Value(Description(Current(TradeMenu))) > 0 then
            CargoIndex := Integer'Value(Description(Current(TradeMenu)));
            if BaseIndex > 0 then
               BaseItemIndex :=
                 FindBaseCargo
                   (PlayerShip.Cargo(CargoIndex).ProtoIndex,
                    PlayerShip.Cargo(CargoIndex).Durability);
               if BaseItemIndex = 0 then
                  BaseItemIndex :=
                    FindBaseCargo(PlayerShip.Cargo(CargoIndex).ProtoIndex);
               end if;
            else
               for I in TraderCargo.Iterate loop
                  if TraderCargo(I).ProtoIndex =
                    PlayerShip.Cargo(CargoIndex).ProtoIndex and
                    TraderCargo(I).Durability =
                      PlayerShip.Cargo(CargoIndex).Durability then
                     BaseItemIndex := BaseCargo_Container.To_Index(I);
                     exit;
                  end if;
               end loop;
               if BaseItemIndex = 0 then
                  for I in TraderCargo.Iterate loop
                     if TraderCargo(I).ProtoIndex =
                       PlayerShip.Cargo(CargoIndex).ProtoIndex then
                        BaseItemIndex := BaseCargo_Container.To_Index(I);
                        exit;
                     end if;
                  end loop;
               end if;
            end if;
         else
            BaseItemIndex :=
              Integer'Value(Description(Current(TradeMenu))) * (-1);
            if BaseIndex > 0 then
               CargoIndex :=
                 FindItem
                   (Inventory => PlayerShip.Cargo,
                    ProtoIndex =>
                      SkyBases(BaseIndex).Cargo(BaseItemIndex).ProtoIndex,
                    Durability =>
                      SkyBases(BaseIndex).Cargo(BaseItemIndex).Durability);
               if CargoIndex = 0 then
                  CargoIndex :=
                    FindItem
                      (PlayerShip.Cargo,
                       SkyBases(BaseIndex).Cargo(BaseItemIndex).ProtoIndex);
               end if;
            else
               CargoIndex :=
                 FindItem
                   (Inventory => PlayerShip.Cargo,
                    ProtoIndex => TraderCargo(BaseItemIndex).ProtoIndex,
                    Durability => TraderCargo(BaseItemIndex).Durability);
               if CargoIndex = 0 then
                  CargoIndex :=
                    FindItem
                      (PlayerShip.Cargo,
                       TraderCargo(BaseItemIndex).ProtoIndex);
               end if;
            end if;
         end if;
         if not Buy then
            if FieldIndex = 4 then
               SellItems(CargoIndex, Get_Buffer(Fields(TradeForm, 2)));
            else
               SellItems
                 (CargoIndex,
                  Positive'Image(PlayerShip.Cargo.Element(CargoIndex).Amount));
            end if;
         else
            BuyItems(BaseItemIndex, Get_Buffer(Fields(TradeForm, 2)));
         end if;
      end if;
      Set_Cursor_Visibility(Visibility);
      Post(TradeForm, False);
      Delete(TradeForm);
      DrawGame(Trade_View);
      return Trade_View;
   exception
      when An_Exception : Trade_Cant_Buy =>
         ShowErrorMessage
           ("You can't buy " &
            Exception_Message(An_Exception) &
            " in this " &
            Trader &
            ".");
         return Trade_View;
      when An_Exception : Trade_Not_For_Sale_Now =>
         ShowErrorMessage
           ("You can't buy " &
            Exception_Message(An_Exception) &
            " in this base at this moment.");
         return Trade_View;
      when An_Exception : Trade_Buying_Too_Much =>
         ShowErrorMessage
           (Trader &
            " don't have that much " &
            Exception_Message(An_Exception) &
            " for sale.");
         return Trade_View;
      when Trade_No_Free_Cargo =>
         ShowErrorMessage
           ("You don't have that much free space in your ship cargo.");
         return Trade_View;
      when An_Exception : Trade_No_Money =>
         ShowErrorMessage
           ("You don't have any " &
            To_String(MoneyName) &
            " to buy " &
            Exception_Message(An_Exception) &
            ".");
         return Trade_View;
      when An_Exception : Trade_Not_Enough_Money =>
         ShowErrorMessage
           ("You don't have enough " &
            To_String(MoneyName) &
            " to buy so much " &
            Exception_Message(An_Exception) &
            ".");
         return Trade_View;
      when Trade_Invalid_Amount =>
         if Buy then
            ShowErrorMessage("You entered invalid amount to buy.");
         else
            ShowErrorMessage("You entered invalid amount to sell.");
         end if;
         return Trade_View;
      when An_Exception : Trade_Too_Much_For_Sale =>
         ShowErrorMessage
           ("You dont have that much " &
            Exception_Message(An_Exception) &
            " in ship cargo.");
         return Trade_View;
      when An_Exception : Trade_No_Money_In_Base =>
         ShowErrorMessage
           ("You can't sell so much " &
            Exception_Message(An_Exception) &
            " because " &
            Trader &
            " don't have that much " &
            To_String(MoneyName) &
            " to buy it.");
         return Trade_View;
      when Trade_No_Trader =>
         ShowErrorMessage
           ("You don't have assigned anyone in crew to talk in bases duty.");
         return Trade_View;
   end TradeResult;

   function TradeKeys(Key: Key_Code) return GameStates is
      Result: Menus.Driver_Result;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      EventIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex;
   begin
      case Key is
         when 27 => -- Back to sky map
            if BaseIndex = 0 and EventIndex > 0 then
               DeleteEvent(EventIndex);
            end if;
            CurrentMenuIndex := 1;
            DrawGame(Sky_Map_View);
            return Sky_Map_View;
         when 56 | KEY_UP => -- Select previous item to trade
            Result := Driver(TradeMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(TradeMenu, M_Last_Item);
            end if;
         when 50 | KEY_DOWN => -- Select next item to trade
            Result := Driver(TradeMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(TradeMenu, M_First_Item);
            end if;
         when 32 => -- Sell item
            Buy := False;
            return ShowTradeForm;
         when 10 => -- Buy item
            Buy := True;
            return ShowTradeForm;
         when Key_F1 => -- Show help
            Erase;
            ShowGameHeader(Help_Topic);
            ShowHelp(Trade_View, 3);
            return Help_Topic;
         when others =>
            Result := Driver(TradeMenu, Key);
            if Result /= Menu_Ok then
               Result := Driver(TradeMenu, M_Clear_Pattern);
               Result := Driver(TradeMenu, Key);
            end if;
      end case;
      if Result = Menu_Ok then
         ShowItemInfo;
      end if;
      CurrentMenuIndex := Menus.Get_Index(Current(TradeMenu));
      return Trade_View;
   end TradeKeys;

   function TradeFormKeys(Key: Key_Code) return GameStates is
      Result: Forms.Driver_Result;
      FieldIndex: Positive := Get_Index(Current(TradeForm));
      Visibility: Cursor_Visibility := Invisible;
      MaxIndex: Positive;
   begin
      case Key is
         when KEY_UP => -- Select previous field
            Result := Driver(TradeForm, F_Previous_Field);
            FieldIndex := Get_Index(Current(TradeForm));
            if FieldIndex = 2 then
               Result := Driver(TradeForm, F_End_Line);
            end if;
         when KEY_DOWN => -- Select next field
            Result := Driver(TradeForm, F_Next_Field);
            FieldIndex := Get_Index(Current(TradeForm));
            if FieldIndex = 2 then
               Result := Driver(TradeForm, F_End_Line);
            end if;
         when 10 => -- quit/buy/sell
            if FieldIndex = 2 then
               Result := Driver(TradeForm, F_Next_Field);
               if Result = Form_Ok then
                  if Get_Buffer(Fields(TradeForm, 2)) =
                    "                    " then
                     if Buy then
                        FieldIndex := 3;
                     else
                        FieldIndex := 5;
                     end if;
                  else
                     FieldIndex := 4;
                  end if;
                  Set_Current(TradeForm, Fields(TradeForm, FieldIndex));
               end if;
            else
               return TradeResult;
            end if;
         when Key_Backspace => -- delete last character
            if FieldIndex = 2 then
               Result := Driver(TradeForm, F_Delete_Previous);
               if Result = Form_Ok then
                  FieldIndex := Get_Index(Current(TradeForm));
                  if FieldIndex /= 2 then
                     FieldIndex := 2;
                     Set_Current(TradeForm, Fields(TradeForm, 2));
                  end if;
               end if;
            end if;
         when KEY_DC => -- delete character at cursor
            if FieldIndex = 2 then
               Result := Driver(TradeForm, F_Delete_Char);
            end if;
         when KEY_RIGHT => -- Move cursor right
            if FieldIndex = 2 then
               Result := Driver(TradeForm, F_Right_Char);
            end if;
         when KEY_LEFT => -- Move cursor left
            if FieldIndex = 2 then
               Result := Driver(TradeForm, F_Left_Char);
            end if;
         when 27 => -- Escape select cancel button, second time closes form
            if FieldIndex /= 3 then
               FieldIndex := 3;
               Set_Current(TradeForm, Fields(TradeForm, 3));
               Result := Form_Ok;
            else
               return TradeResult;
            end if;
         when others =>
            Result := Driver(TradeForm, Key);
      end case;
      if Result = Form_Ok then
         if Buy then
            MaxIndex := 4;
         else
            MaxIndex := 5;
         end if;
         for I in 2 .. MaxIndex loop
            Set_Foreground(Fields(TradeForm, I));
            Set_Background(Fields(TradeForm, I));
         end loop;
         Set_Foreground(Current(TradeForm), BoldCharacters, 11);
         Set_Background(Current(TradeForm), BoldCharacters, 11);
         if FieldIndex = 2 then
            Visibility := Normal;
         end if;
         Set_Cursor_Visibility(Visibility);
         Refresh(FormWindow);
      end if;
      return Trade_Form;
   end TradeFormKeys;

end Trades.UI;
