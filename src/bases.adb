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

with Ada.Numerics.Discrete_Random; use Ada.Numerics;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with Ships; use Ships;
with Maps; use Maps;
with Messages; use Messages;
with Items; use Items;
with UserInterface; use UserInterface;
with Crew; use Crew;

package body Bases is
    
    TradeMenu : Menu;
    MenuWindow : Window;

    procedure BuyItems(ItemIndex : Positive; Amount : String) is
        BuyAmount : Positive;
        BaseType : constant Positive := Bases_Types'Pos(SkyBases(SkyMap(PlayerShip.SkyX,
            PlayerShip.SkyY).BaseIndex).BaseType) + 1;
        ItemName : constant String := To_String(Items_List.Element(ItemIndex).Name);
        Cost : Positive;
        MoneyIndex : Natural := 0;
    begin
        BuyAmount := Positive'Value(Amount);
        if not Items_List.Element(ItemIndex).Buyable(BaseType) then
            ShowDialog("You can't buy " & ItemName & " in this base.");
            return;
        end if;
        Cost := BuyAmount * Items_List.Element(ItemIndex).Prices(BaseType);
        Cost := Cost - Integer(Float'Floor(Float(Cost) *
                (Float(PlayerShip.Crew.Element(1).Skills(4, 1)) / 200.0)));
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            if PlayerShip.Cargo.Element(I).ProtoIndex = 1 then
                MoneyIndex := I;
            end if;
        end loop;
        if FreeCargo(Cost - (Items_List.Element(ItemIndex).Weight * BuyAmount)) < 0 then
            ShowDialog("You don't have that much free space in your ship cargo.");
            return;
        end if;
        if MoneyIndex = 0 then
            ShowDialog("You don't have charcollum to buy " & ItemName & ".");
            return;
        end if;
        if Cost > PlayerShip.Cargo.Element(MoneyIndex).Amount then
            ShowDialog("You don't have enough charcollum to buy so much " & ItemName & ".");
            return;
        end if;
        UpdateCargo(1, (0 - Cost));
        UpdateCargo(ItemIndex, BuyAmount);
        GainExp(1, 4, 1);
        AddMessage("You bought" & Positive'Image(BuyAmount) & " " & ItemName &
            " for" & Positive'Image(Cost) & " Charcollum.", TradeMessage);
        UpdateGame(5);
    exception
        when CONSTRAINT_ERROR =>
            ShowDialog("You must enter number as an amount to buy.");
    end BuyItems;

    procedure SellItems(ItemIndex : Positive; Amount : String) is
        SellAmount : Positive;
        BaseType : constant Positive := Bases_Types'Pos(SkyBases(SkyMap(PlayerShip.SkyX,
            PlayerShip.SkyY).BaseIndex).BaseType) + 1;
        ProtoIndex : constant Positive := PlayerShip.Cargo.Element(ItemIndex).ProtoIndex;
        ItemName : constant String := To_String(Items_List.Element(ProtoIndex).Name);
        Profit : Positive;
    begin
        SellAmount := Positive'Value(Amount);
        if PlayerShip.Cargo.Element(ItemIndex).Amount < SellAmount then
            ShowDialog("You dont have that much " & ItemName & " in ship cargo.");
            return;
        end if;
        Profit := Items_List.Element(ProtoIndex).Prices(BaseType) * SellAmount;
        Profit := Profit + Integer(Float'Floor(Float(Profit) *
                (Float(PlayerShip.Crew.Element(1).Skills(4, 1)) / 200.0)));
        if FreeCargo((Items_List.Element(ProtoIndex).Weight * SellAmount) - Profit) < 0 then
            ShowDialog("You don't have enough free cargo space in your ship for Charcollum.");
            return;
        end if;
        UpdateCargo(ProtoIndex, (0 - SellAmount));
        UpdateCargo(1, Profit);
        GainExp(1, 4, 1);
        AddMessage("You sold" & Positive'Image(SellAmount) & " " & ItemName & " for" & 
            Positive'Image(Profit) & " Charcollum.", TradeMessage);
        UpdateGame(5);
    exception
        when CONSTRAINT_ERROR =>
            ShowDialog("You must enter number as an amount to sell.");
    end SellItems;

    function GenerateBaseName return Unbounded_String is -- based on name generator from libtcod
        type Syllabes_Range is range 1..34;
        StartSyllabes : constant array (Syllabes_Range) of Unbounded_String := 
            (To_Unbounded_String("Ael"), To_Unbounded_String("Ash"),
            To_Unbounded_String("Barrow"), To_Unbounded_String("Bel"),
            To_Unbounded_String("Black"), To_Unbounded_String("Clear"),
            To_Unbounded_String("Cold"), To_Unbounded_String("Crystal"),
            To_Unbounded_String("Deep"), To_Unbounded_String("Edge"),
            To_Unbounded_String("Falcon"), To_Unbounded_String("Fair"),
            To_Unbounded_String("Fall"), To_Unbounded_String("Glass"),
            To_Unbounded_String("Gold"), To_Unbounded_String("Ice"),
            To_Unbounded_String("Iron"), To_Unbounded_String("Mill"),
            To_Unbounded_String("Moon"), To_Unbounded_String("Moor"),
            To_Unbounded_String("Ray"), To_Unbounded_String("Red"),
            To_Unbounded_String("Rock"), To_Unbounded_String("Rose"),
            To_Unbounded_String("Shadow"), To_Unbounded_String("Silver"),
            To_Unbounded_String("Spell"), To_Unbounded_String("Spring"),
            To_Unbounded_String("Stone"), To_Unbounded_String("Strong"),
            To_Unbounded_String("Summer"), To_Unbounded_String("Swyn"),
            To_Unbounded_String("Wester"), To_Unbounded_String("Winter"));
        EndSyllabes : constant array (Syllabes_Range) of Unbounded_String :=
            (To_Unbounded_String("ash"), To_Unbounded_String("burn"),
            To_Unbounded_String("barrow"), To_Unbounded_String("bridge"),
            To_Unbounded_String("castle"), To_Unbounded_String("cliff"),
            To_Unbounded_String("coast"), To_Unbounded_String("crest"),
            To_Unbounded_String("dale"), To_Unbounded_String("dell"),
            To_Unbounded_String("dor"), To_Unbounded_String("fall"),
            To_Unbounded_String("field"), To_Unbounded_String("ford"),
            To_Unbounded_String("fort"), To_Unbounded_String("gate"),
            To_Unbounded_String("haven"), To_Unbounded_String("hill"),
            To_Unbounded_String("hold"), To_Unbounded_String("hollow"),
            To_Unbounded_String("iron"), To_Unbounded_String("lake"),
            To_Unbounded_String("marsh"), To_Unbounded_String("mill"),
            To_Unbounded_String("mist"), To_Unbounded_String("mount"),
            To_Unbounded_String("moor"), To_Unbounded_String("pond"),
            To_Unbounded_String("shade"), To_Unbounded_String("shore"),
            To_Unbounded_String("summer"), To_Unbounded_String("town"),
            To_Unbounded_String("wick"), To_Unbounded_String("berg"));
        package Rand_Syllabe is new Discrete_Random(Syllabes_Range);
        Generator : Rand_Syllabe.Generator;
        NewName : Unbounded_String;
    begin
        Rand_Syllabe.Reset(Generator);
        NewName := StartSyllabes(Rand_Syllabe.Random(Generator)) & EndSyllabes(Rand_Syllabe.Random(Generator));
        return NewName;
    end GenerateBaseName;

    procedure ShowItemInfo is
        ItemIndex : constant Positive := Get_Index(Current(TradeMenu)) + 1;
        InfoWindow : Window;
        BaseType : constant Positive := Bases_Types'Pos(SkyBases(SkyMap(PlayerShip.SkyX,
            PlayerShip.SkyY).BaseIndex).BaseType) + 1;
    begin
        InfoWindow := Create(5, (Columns / 2), 2, (Columns / 2));
        Add(Win => InfoWindow, Str => "Price:" & Integer'Image(Items_List.Element(ItemIndex).Prices(BaseType)) & " Charcollum");
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            if PlayerShip.Cargo.Element(I).ProtoIndex = ItemIndex then
                Move_Cursor(Win => InfoWindow, Line => 1, Column => 0);
                Add(Win => InfoWindow, Str => "Owned:" & Integer'Image(PlayerShip.Cargo.Element(I).Amount));
                exit;
            end if;
        end loop;
        Refresh;
        Refresh(InfoWindow);
    end ShowItemInfo;

    procedure ShowTrade2 is
        Trade_Items: constant Item_Array_Access := new Item_Array(1..Items_List.Last_Index);
        MenuHeight : Line_Position;
        MenuLength : Column_Position;
        MoneyIndex : Natural := 0;
    begin
        for I in 2..(Items_List.Last_Index) loop
            Trade_Items.all(I - 1) := New_Item(To_String(Items_List.Element(I).Name));
        end loop;
        Trade_Items.all(Items_List.Last_Index) := Null_Item;
        TradeMenu := New_Menu(Trade_Items);
        Set_Format(TradeMenu, Lines - 10, 1);
        Set_Mark(TradeMenu, "");
        Scale(TradeMenu, MenuHeight, MenuLength);
        MenuWindow := Create(MenuHeight, MenuLength, 2, 2);
        Set_Window(TradeMenu, MenuWindow);
        Set_Sub_Window(TradeMenu, Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
        Post(TradeMenu);
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            if PlayerShip.Cargo.Element(I).ProtoIndex = 1 then
                MoneyIndex := I;
                exit;
            end if;
        end loop;
        Move_Cursor(Line => (MenuHeight + 3), Column => 2);
        if MoneyIndex > 0 then
            Add(Str => "You have" & Natural'Image(PlayerShip.Cargo.Element(MoneyIndex).Amount) &
                " Charcollum.");
        else
            Add(Str => "You don't have any Charcollum to buy anything.");
        end if;
        Move_Cursor(Line => (Lines - 1), Column => 2);
        Add(Str => "ENTER to buy selected item, SPACE for sell.");
        Change_Attributes(Line => (Lines - 1), Column => 2, Count => 5, Color => 1);
        Change_Attributes(Line => (Lines - 1), Column => 30, Count => 5, Color => 1);
        ShowItemInfo;
        Refresh(MenuWindow);
    end ShowTrade2;
    
    procedure ShowTrade(Key : Key_Code) is
        BaseType : constant Positive := Bases_Types'Pos(SkyBases(SkyMap(PlayerShip.SkyX,
            PlayerShip.SkyY).BaseIndex).BaseType) + 1;
        BuyLetter, SellLetter : Character;
        BuyLetters : array (2..Items_List.Last_Index) of Character;
        SellLetters : array (1..PlayerShip.Cargo.Last_Index) of Character := (others => ' ');
        Visibility : Cursor_Visibility := Normal;
        Amount : String(1..6);
        ItemIndex : Natural := 0;
        CargoAmount : Natural;
        CurrentLine : Line_Position := 2;
        MoneyIndex: Natural := 0;
    begin
        if Key /= KEY_NONE then
            Erase;
            Refresh;
            ShowGameHeader(Trade_View);
        end if;
        Move_Cursor(Line => 2, Column => 2);
        Add(Str => "BUY SELL     NAME");
        Move_Cursor(Line => 2, Column => 35);
        Add(Str => "PRICE");
        Move_Cursor(Line => 2, Column => 50);
        Add(Str => "OWNED");
        for I in 2..Items_List.Last_Index loop
            if Items_List.Element(I).Buyable(BaseType) then
                BuyLetter := Character'Val(95 + I);
            else
                BuyLetter := ' ';
            end if;
            BuyLetters(I) := BuyLetter;
            SellLetter := ' ';
            CargoAmount := 0;
            for J in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                if PlayerShip.Cargo.Element(J).ProtoIndex = I then
                    SellLetter := Character'Val(63 + I);
                    SellLetters(J) := SellLetter;
                    CargoAmount := PlayerShip.Cargo.Element(J).Amount;
                    exit;
                end if;
                if PlayerShip.Cargo.Element(J).ProtoIndex = 1 then
                    MoneyIndex := J;
                end if;
            end loop;
            CurrentLine := CurrentLine + 1;
            Move_Cursor(Line => CurrentLine, Column => 3);
            Add(Str => BuyLetter & "   " & SellLetter & "   " &
                To_String(Items_List.Element(I).Name));
            Move_Cursor(Line => CurrentLine, Column => 30);
            Add(Str => Positive'Image(Items_List.Element(I).Prices(BaseType)) & " charcollum");
            Move_Cursor(Line => CurrentLine, Column => 50);
            Add(Str => Natural'Image(CargoAmount));
            if BuyLetter /= ' ' then
                Change_Attributes(Line => CurrentLine, Column => 3, Count => 1, Color => 1);
            end if;
            if SellLetter /= ' ' then
                Change_Attributes(Line => CurrentLine, Column => 7, Count => 1, Color => 1);
            end if;
        end loop;
        Move_Cursor(Line => (CurrentLine + 2), Column => 1);
        if MoneyIndex > 0 then
            Add(Str => "You have" & Natural'Image(PlayerShip.Cargo.Element(MoneyIndex).Amount) &
                " Charcollum.");
        else
            Add(Str => "You don't have any charcollum to buy Items_List.");
        end if;
        if Key /= KEY_NONE then -- start buying/selling items from/to base
            for I in BuyLetters'Range loop
                if Key = Character'Pos(BuyLetters(I)) and BuyLetters(I) /= ' ' then
                    ItemIndex := I;
                    exit;
                end if;
            end loop;
            if ItemIndex > 0 then -- Buy item from base
                Set_Echo_Mode(True);
                Set_Cursor_Visibility(Visibility);
                Move_Cursor(Line => (Lines / 2), Column => 2);
                Add(Str => "Enter amount of " & To_String(Items_List.Element(ItemIndex).Name)
                    & " to buy: ");
                Get(Str => Amount, Len => 6);
                BuyItems(ItemIndex, Amount);
                ItemIndex := 0;
            else
                for I in SellLetters'Range loop
                    if Key = Character'Pos(SellLetters(I)) and SellLetters(I) /= ' ' then
                        ItemIndex := I;
                        exit;
                    end if;
                end loop;
                if ItemIndex > 0 then -- Sell item to base
                    Set_Echo_Mode(True);
                    Set_Cursor_Visibility(Visibility);
                    Move_Cursor(Line => (Lines / 2), Column => 2);
                    Add(Str => "Enter amount of " &
                        To_String(Items_List.Element(PlayerShip.Cargo.Element(ItemIndex).ProtoIndex).Name)
                        & " to sell: ");
                    Get(Str => Amount, Len => 6);
                    SellItems(ItemIndex, Amount);
                    ItemIndex := 0;
                end if;
            end if;
            if ItemIndex = 0 then
                Visibility := Invisible;
                Set_Echo_Mode(False);
                Set_Cursor_Visibility(Visibility);
                DrawGame(Trade_View);
            end if;
        end if;
    end ShowTrade;

    function TradeKeys(Key : Key_Code) return GameStates is
        Result : Driver_Result;
        NewKey : Key_Code;
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when 56 => -- Select previous item to trade
                Result := Driver(TradeMenu, M_Up_Item);
                if Result = Menu_Ok then
                    ShowItemInfo;
                    Refresh(MenuWindow);
                end if;
            when 50 => -- Select next item to trade
                Result := Driver(TradeMenu, M_Down_Item);
                if Result = Menu_Ok then
                    ShowItemInfo;
                    Refresh(MenuWindow);
                end if;
            when 27 => 
                NewKey := Get_KeyStroke;
                if NewKey = 91 then
                    NewKey := Get_KeyStroke;
                    if NewKey = 65 then
                        Result := Driver(TradeMenu, M_Up_Item);
                        if Result = Menu_Ok then
                            ShowItemInfo;
                            Refresh(MenuWindow);
                        end if;
                    elsif NewKey = 66 then
                        Result := Driver(TradeMenu, M_Down_Item);
                        if Result = Menu_Ok then
                            ShowItemInfo;
                            Refresh(MenuWindow);
                        end if;
                    end if;
                end if;
            when others =>
                null;
        end case;
        return Trade_View;
    end TradeKeys;

end Bases;
