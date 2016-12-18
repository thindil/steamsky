--    Copyright 2016 Bartek thindil Jasicki
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

with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Terminal_Interface.Curses.Forms.Field_Types.IntField;
with Maps; use Maps;
with Items; use Items;
with UserInterface; use UserInterface;
with Ships; use Ships;

package body Bases.UI.Trade is
    
    Buy : Boolean;
    TradeForm : Form;
    FormWindow : Window;

    procedure ShowItemInfo is
        ItemIndex : Positive;
        InfoWindow : Window;
        BaseType : constant Positive := Bases_Types'Pos(SkyBases(SkyMap(PlayerShip.SkyX,
            PlayerShip.SkyY).BaseIndex).BaseType) + 1;
    begin
        for I in Items_List.First_Index..Items_List.Last_Index loop
            if To_String(Items_List.Element(I).Name) = Name(Current(TradeMenu)) then
                ItemIndex := I;
                exit;
            end if;
        end loop;
        InfoWindow := Create(5, (Columns / 2), 3, (Columns / 2));
        if Items_List.Element(ItemIndex).Buyable(BaseType) then
            Add(Win => InfoWindow, Str => "Base buy/sell price:");
        else
            Add(Win => InfoWindow, Str => "Base sell price:");
        end if;
        Add(Win => InfoWindow, Str => Integer'Image(Items_List.Element(ItemIndex).Prices(BaseType)) & " Charcollum");
        Move_Cursor(Win => InfoWindow, Line => 1, Column => 0);
        Add(Win => InfoWindow, Str => "Weight:" & Integer'Image(Items_List.Element(ItemIndex).Weight) & 
            " kg");
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            if PlayerShip.Cargo.Element(I).ProtoIndex = ItemIndex then
                Move_Cursor(Win => InfoWindow, Line => 2, Column => 0);
                Add(Win => InfoWindow, Str => "Owned:" & Integer'Image(PlayerShip.Cargo.Element(I).Amount));
                exit;
            end if;
        end loop;
        Refresh;
        Refresh(InfoWindow);
        Delete(InfoWindow);
    end ShowItemInfo;

    procedure ShowTrade is
        Trade_Items: constant Item_Array_Access := new Item_Array(1..Items_List.Last_Index);
        BaseType : constant Positive := Bases_Types'Pos(SkyBases(SkyMap(PlayerShip.SkyX,
            PlayerShip.SkyY).BaseIndex).BaseType) + 1;
        MenuHeight : Line_Position;
        MenuLength : Column_Position;
        MoneyIndex : Natural := 0;
        ShowItem : Boolean := False;
        MenuIndex : Integer := 1;
        FreeSpace : Integer;
    begin
        for I in 1..(Items_List.Last_Index) loop
            for J in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                if PlayerShip.Cargo.Element(J).ProtoIndex = I and Items_List.Element(I).Prices(BaseType) > 0 then
                    ShowItem := True;
                    exit;
                end if;
            end loop;
            if Items_List.Element(I).Buyable(BaseType) then
                ShowItem := True;
            end if;
            if ShowItem then
                Trade_Items.all(MenuIndex) := New_Item(To_String(Items_List.Element(I).Name));
                MenuIndex := MenuIndex + 1;
            end if;
            ShowItem := False;
        end loop;
        for I in MenuIndex..Items_List.Last_Index loop
            Trade_Items.all(I) := Null_Item;
        end loop;
        TradeMenu := New_Menu(Trade_Items);
        Set_Format(TradeMenu, Lines - 10, 1);
        Set_Mark(TradeMenu, "");
        Scale(TradeMenu, MenuHeight, MenuLength);
        MenuWindow := Create(MenuHeight, MenuLength, 3, 2);
        Set_Window(TradeMenu, MenuWindow);
        Set_Sub_Window(TradeMenu, Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
        Post(TradeMenu);
        if Trade_Items.all(CurrentMenuIndex) = Null_Item then
            CurrentMenuIndex := 1;
        end if;
        Set_Current(TradeMenu, Trade_Items.all(CurrentMenuIndex));
        MoneyIndex := FindMoney;
        Move_Cursor(Line => (MenuHeight + 4), Column => 2);
        if MoneyIndex > 0 then
            Add(Str => "You have" & Natural'Image(PlayerShip.Cargo.Element(MoneyIndex).Amount) &
                " Charcollum.");
        else
            Add(Str => "You don't have any Charcollum to buy anything.");
        end if;
        Move_Cursor(Line => (MenuHeight + 5), Column => 2);
        FreeSpace := FreeCargo(0);
        if FreeSpace < 0 then
            FreeSpace := 0;
        end if;
        Add(Str => "Free cargo space:" & Integer'Image(FreeSpace) & " kg");
        Move_Cursor(Line => (Lines - 1), Column => 2);
        Add(Str => "ENTER to buy selected item, SPACE for sell.");
        Change_Attributes(Line => (Lines - 1), Column => 2, Count => 5, Color => 1);
        Change_Attributes(Line => (Lines - 1), Column => 30, Count => 5, Color => 1);
        ShowItemInfo;
        Refresh(MenuWindow);
    end ShowTrade;

    function ShowTradeForm return GameStates is
        Trade_Fields : constant Field_Array_Access := new Field_Array(1..5);
        BaseType : constant Positive := Bases_Types'Pos(SkyBases(SkyMap(PlayerShip.SkyX,
            PlayerShip.SkyY).BaseIndex).BaseType) + 1;
        FieldOptions : Field_Option_Set;
        FormHeight : Line_Position;
        FormLength : Column_Position;
        Visibility : Cursor_Visibility := Normal;
        ItemIndex : Positive;
        CargoIndex, MaxAmount : Natural := 0;
        FieldText : Unbounded_String := To_Unbounded_String("Enter amount of ");
    begin
        for I in Items_List.First_Index..Items_List.Last_Index loop
            if To_String(Items_List.Element(I).Name) = Name(Current(TradeMenu)) then
                ItemIndex := I;
                exit;
            end if;
        end loop;
        Append(FieldText, Items_List.Element(ItemIndex).Name);
        if Buy then
            if not Items_List.Element(ItemIndex).Buyable(BaseType) then
                ShowDialog("You can't buy " & To_String(Items_List.Element(ItemIndex).Name) &
                    " in this base.");
                DrawGame(Trade_View);
                return Trade_View;
            end if;
            for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                if PlayerShip.Cargo.Element(I).ProtoIndex = 1 then
                    MaxAmount := PlayerShip.Cargo.Element(I).Amount / Items_List.Element(ItemIndex).Prices(BaseType);
                    exit;
                end if;
            end loop;
            Append(FieldText, " to buy");
        else
            for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                if PlayerShip.Cargo.Element(I).ProtoIndex = ItemIndex then
                    CargoIndex := I;
                    MaxAmount := PlayerShip.Cargo.Element(I).Amount;
                    exit;
                end if;
            end loop;
            if CargoIndex = 0 then
                ShowDialog("You don't have any " & To_String(Items_List.Element(ItemIndex).Name) &
                    " for sale.");
                DrawGame(Trade_View);
                return Trade_View;
            end if;
            Append(FieldText, " to sell");
        end if;
        Append(FieldText, " (max" & Natural'Image(MaxAmount) & "): ");
        if TradeForm = Null_Form then
            Set_Cursor_Visibility(Visibility);
            Trade_Fields.all(1) := New_Field(1, Column_Position(Length(FieldText)), 0, 0, 0, 0);
            FieldOptions := Get_Options(Trade_Fields.all(1));
            Set_Buffer(Trade_Fields.all(1), 0, To_String(FieldText));
            FieldOptions.Active := False;
            Set_Options(Trade_Fields.all(1), FieldOptions);
            Trade_Fields.all(2) := New_Field(1, 20, 0, Column_Position(Length(FieldText)), 0, 0);
            FieldOptions := Get_Options(Trade_Fields.all(2));
            FieldOptions.Auto_Skip := False;
            Set_Options(Trade_Fields.all(2), FieldOptions);
            Set_Background(Trade_Fields.all(2), (Reverse_Video => True, others => False));
            Terminal_Interface.Curses.Forms.Field_Types.IntField.Set_Field_Type(Trade_Fields.all(2), (0, 0, MaxAmount));
            Trade_Fields.all(3) := New_Field(1, 8, 2, (Column_Position(Length(FieldText)) / 2), 0, 0);
            Set_Buffer(Trade_Fields.all(3), 0, "[Cancel]");
            FieldOptions := Get_Options(Trade_Fields.all(3));
            FieldOptions.Edit := False;
            Set_Options(Trade_Fields.all(3), FieldOptions);
            Trade_Fields.all(4) := New_Field(1, 4, 2, (Column_Position(Length(FieldText)) / 2) + 10, 0, 0);
            FieldOptions := Get_Options(Trade_Fields.all(4));
            FieldOptions.Edit := False;
            Set_Options(Trade_Fields.all(4), FieldOptions);
            Set_Buffer(Trade_Fields.all(4), 0, "[Ok]");
            Trade_Fields.all(5) := Null_Field;
            TradeForm := New_Form(Trade_Fields);
            Scale(TradeForm, FormHeight, FormLength);
            FormWindow := Create(FormHeight + 2, FormLength + 2, ((Lines / 3) - (FormHeight / 2)), ((Columns / 2) - (FormLength / 2)));
            Box(FormWindow);
            Set_Window(TradeForm, FormWindow);
            Set_Sub_Window(TradeForm, Derived_Window(FormWindow, FormHeight, FormLength, 1, 1));
            Post(TradeForm);
        end if;
        Refresh;
        Refresh(FormWindow);
        return Trade_Form;
    end ShowTradeForm;

    function TradeResult return GameStates is
        ItemIndex : Positive;
        CargoIndex : Natural := 0;
        Visibility : Cursor_Visibility := Invisible;
        FieldIndex : constant Positive := Get_Index(Current(TradeForm));
    begin
        if FieldIndex < 3 then
            return Trade_Form;
        elsif FieldIndex = 4 then
            for I in Items_List.First_Index..Items_List.Last_Index loop
                if To_String(Items_List.Element(I).Name) = Name(Current(TradeMenu)) then
                    ItemIndex := I;
                    exit;
                end if;
            end loop;
            if not Buy then
                for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                    if PlayerShip.Cargo.Element(I).ProtoIndex = ItemIndex then
                        CargoIndex := I;
                        exit;
                    end if;
                end loop;
                SellItems(CargoIndex, Get_Buffer(Fields(TradeForm, 2)));
            else
                BuyItems(ItemIndex, Get_Buffer(Fields(TradeForm, 2)));
            end if;
        end if;
        Set_Cursor_Visibility(Visibility);
        Post(TradeForm, False);
        Delete(TradeForm);
        DrawGame(Trade_View);
        return Trade_View;
    end TradeResult;

    function TradeKeys(Key : Key_Code) return GameStates is
        Result : Menus.Driver_Result;
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                CurrentMenuIndex := 1;
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when 56 | KEY_UP => -- Select previous item to trade
                Result := Driver(TradeMenu, M_Up_Item);
                if Result = Request_Denied then
                    Result := Driver(TradeMenu, M_Last_Item);
                end if;
                if Result = Menu_Ok then
                    ShowItemInfo;
                    Refresh(MenuWindow);
                end if;
            when 50 | KEY_DOWN => -- Select next item to trade
                Result := Driver(TradeMenu, M_Down_Item);
                if Result = Request_Denied then
                    Result := Driver(TradeMenu, M_First_Item);
                end if;
                if Result = Menu_Ok then
                    ShowItemInfo;
                    Refresh(MenuWindow);
                end if;
            when 32 => -- Sell item
                Buy := False;
                return ShowTradeForm;
            when 10 => -- Buy item
                Buy := True;
                return ShowTradeForm;
            when others =>
                Result := Driver(TradeMenu, Key);
                if Result = Menu_Ok then
                    ShowItemInfo;
                    Refresh(MenuWindow);
                else
                    Result := Driver(TradeMenu, M_CLEAR_PATTERN);
                    Result := Driver(TradeMenu, Key);
                    if Result = Menu_Ok then
                        ShowItemInfo;
                        Refresh(MenuWindow);
                    end if;
                end if;
        end case;
        CurrentMenuIndex := Menus.Get_Index(Current(TradeMenu));
        return Trade_View;
    end TradeKeys;

    function TradeFormKeys(Key : Key_Code) return GameStates is
        Result : Forms.Driver_Result;
        FieldIndex : Positive := Get_Index(Current(TradeForm));
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
                return TradeResult;
            when KEY_BACKSPACE => -- delete last character
                if FieldIndex = 2 then
                    Result := Driver(TradeForm, F_Delete_Previous);
                    if Result = Form_Ok then
                        FieldIndex := Get_Index(Current(TradeForm));
                        if FieldIndex /= 2 then
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
            when others =>
                Result := Driver(TradeForm, Key);
        end case;
        if Result = Form_Ok then
            if FieldIndex = 2 then
                Set_Background(Current(TradeForm), (Reverse_Video => True, others => False));
            else
                Set_Background(Fields(TradeForm, 2), (others => False));
            end if;
            Refresh(FormWindow);
        end if;
        return Trade_Form;
    end TradeFormKeys;

end Bases.UI.Trade;
