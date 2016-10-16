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

with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with Maps; use Maps;
with Items; use Items;
with UserInterface; use UserInterface;
with ShipModules; use ShipModules;
with Ships; use Ships;

package body Bases.UI is
    
    TradeMenu : Menu;
    MenuWindow : Window;
    InstallView : Boolean := True;

    procedure RepairCost(Cost, Time, ModuleIndex : in out Natural) is
        BaseType : constant Positive := Bases_Types'Pos(SkyBases(SkyMap(PlayerShip.SkyX,
            PlayerShip.SkyY).BaseIndex).BaseType) + 1;
    begin
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            if To_String(PlayerShip.Modules.Element(I).Name) = Name(Current(TradeMenu)) then
                Time := PlayerShip.Modules.Element(I).MaxDurability - PlayerShip.Modules.Element(I).Durability;
                for J in Items_List.First_Index..Items_List.Last_Index loop
                   if Items_List.Element(J).IType = Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).RepairMaterial then
                       Cost := Time * Items_List.Element(J).Prices(BaseType);
                       ModuleIndex := I;
                       exit;
                   end if;
                end loop;
                exit;
            end if;
        end loop;
        if Cost = 0 then
            for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                if PlayerShip.Modules.Element(I).Durability < PlayerShip.Modules.Element(I).MaxDurability then
                    Time := Time + PlayerShip.Modules.Element(I).MaxDurability - PlayerShip.Modules.Element(I).Durability;
                    for J in Items_List.First_Index..Items_List.Last_Index loop
                        if Items_List.Element(J).IType = Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).RepairMaterial
                        then
                            Cost := Cost + ((PlayerShip.Modules.Element(I).MaxDurability - PlayerShip.Modules.Element(I).Durability) *
                                Items_List.Element(J).Prices(BaseType));
                            exit;
                        end if;
                    end loop;
                end if;
            end loop;
            if Name(Current(TradeMenu))(1) = 'R' then
                Cost := Cost * 2;
                Time := Time / 2;
            elsif Name(Current(TradeMenu))(1) = 'F' then
                Cost := Cost * 4;
                Time := Time / 4;
            end if;
        end if;
        if Bases_Types'Val(BaseType - 1) = Shipyard then
            Cost := Cost / 2;
        end if;
    end RepairCost;

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
            Add(Win => InfoWindow, Str => "Buy/Sell price:");
        else
            Add(Win => InfoWindow, Str => "Sell price:");
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

    procedure ShowForm(Buy : Boolean := False) is
        FormWindow : Window;
        ItemIndex : Positive;
        CargoIndex : Natural := 0;
        Amount : String(1..6);
        Visibility : Cursor_Visibility := Normal;
        BaseType : constant Positive := Bases_Types'Pos(SkyBases(SkyMap(PlayerShip.SkyX,
            PlayerShip.SkyY).BaseIndex).BaseType) + 1;
        FormText : Unbounded_String := To_Unbounded_String("Enter amount of ");
        Width : Column_Position;
        MaxAmount : Natural := 0;
    begin
        for I in Items_List.First_Index..Items_List.Last_Index loop
            if To_String(Items_List.Element(I).Name) = Name(Current(TradeMenu)) then
                ItemIndex := I;
                exit;
            end if;
        end loop;
        Append(FormText, Items_List.Element(ItemIndex).Name);
        if Buy then
            if not Items_List.Element(ItemIndex).Buyable(BaseType) then
                ShowDialog("You can't buy " & To_String(Items_List.Element(ItemIndex).Name) &
                    " in this base.");
                DrawGame(Trade_View);
                return;
            end if;
            for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                if PlayerShip.Cargo.Element(I).ProtoIndex = 1 then
                    MaxAmount := PlayerShip.Cargo.Element(I).Amount / Items_List.Element(ItemIndex).Prices(BaseType);
                    exit;
                end if;
            end loop;
            Append(FormText, " to buy");
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
                return;
            end if;
            Append(FormText, " to sell");
        end if;
        Append(FormText, " (max" & Natural'Image(MaxAmount) & "): ");
        Width := Column_Position(Length(FormText) + 10);
        FormWindow := Create(3, Width, ((Lines / 2) - 1), ((Columns / 2) - Column_Position(Width / 2)));
        Box(FormWindow);
        Set_Echo_Mode(True);
        Set_Cursor_Visibility(Visibility);
        Move_Cursor(Win => FormWindow, Line => 1, Column => 2);
        Add(Win => FormWindow, Str => To_String(FormText));
        Get(Win => FormWindow, Str => Amount, Len => 6);
        if Buy then
            BuyItems(ItemIndex, Amount);
        else
            SellItems(CargoIndex, Amount);
        end if;
        Delete(FormWindow);
        Visibility := Invisible;
        Set_Echo_Mode(False);
        Set_Cursor_Visibility(Visibility);
        DrawGame(Trade_View);
    end ShowForm;

    procedure ShowRepairInfo is
        Cost, Time, ModuleIndex : Natural := 0;
        InfoWindow : Window;
    begin
        RepairCost(Cost, Time, ModuleIndex);
        InfoWindow := Create(5, (Columns / 2), 3, (Columns / 2));
        Add(Win => InfoWindow, Str => "Repair cost:" & Natural'Image(Cost) & " Charcollum");
        Move_Cursor(Win => InfoWindow, Line => 1, Column => 0);
        Add(Win => InfoWindow, Str => "Repair time:" & Natural'Image(Time) & " minutes");
        Move_Cursor(Win => InfoWindow, Line => 3, Column => 0);
        Add(Win => InfoWindow, Str => "Press Enter to start repairing");
        Change_Attributes(Win => InfoWindow, Line => 3, Column => 6, Count => 5, Color => 1);
        Refresh;
        Refresh(InfoWindow);
        Delete(InfoWindow);
    end ShowRepairInfo;
    
    procedure ShowRepair is
        BaseIndex : constant Positive := SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
        Repair_Items: constant Item_Array_Access := new Item_Array(PlayerShip.Modules.First_Index..(PlayerShip.Modules.Last_Index
            + 4));
        MenuHeight : Line_Position;
        MenuLength : Column_Position;
        MenuIndex : Integer := 1;
        MoneyIndex : Natural := 0;
    begin
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            if PlayerShip.Modules.Element(I).Durability < PlayerShip.Modules.Element(I).MaxDurability then
                Repair_Items.all(MenuIndex) := New_Item(To_String(PlayerShip.Modules.Element(I).Name));
                MenuIndex := MenuIndex + 1;
            end if;
        end loop;
        if MenuIndex = 1 then
            Move_Cursor(Line => (Lines / 3), Column => (Columns / 3));
            Add(Str => "You have nothing to repair.");
            Refresh;
            return;
        end if;
        Repair_Items.all(MenuIndex) := New_Item("Slowly repair whole ship");
        if SkyBases(BaseIndex).Population > 149 then
            MenuIndex := MenuIndex + 1;
            Repair_Items.all(MenuIndex) := New_Item("Repair whole ship");
        end if;
        if SkyBases(BaseIndex).Population > 299 then
            MenuIndex := MenuIndex + 1;
            Repair_Items.all(MenuIndex) := New_Item("Fast repair whole ship");
        end if;
        MenuIndex := MenuIndex + 1;
        for I in MenuIndex..Repair_Items'Last loop
            Repair_Items.all(I) := Null_Item;
        end loop;
        Repair_Items.all(MenuIndex + 1) := Null_Item;
        TradeMenu := New_Menu(Repair_Items);
        Set_Format(TradeMenu, Lines - 10, 1);
        Set_Mark(TradeMenu, "");
        Scale(TradeMenu, MenuHeight, MenuLength);
        MenuWindow := Create(MenuHeight, MenuLength, 3, 2);
        Set_Window(TradeMenu, MenuWindow);
        Set_Sub_Window(TradeMenu, Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
        Post(TradeMenu);
        MoneyIndex := FindMoney;
        Move_Cursor(Line => (MenuHeight + 4), Column => 2);
        if MoneyIndex > 0 then
            Add(Str => "You have" & Natural'Image(PlayerShip.Cargo.Element(MoneyIndex).Amount) &
                " Charcollum.");
        else
            Add(Str => "You don't have any Charcollum to repair anything.");
        end if;
        ShowRepairInfo;
        Refresh(MenuWindow);
    end ShowRepair;

    procedure ShowModuleInfo is
        ModuleIndex : constant Positive := Positive'Value(Description(Current(TradeMenu)));
        InfoWindow : Window;
        TextCost, TextTime : Unbounded_String;
        CurrentLine : Line_Position := 3;
        Cost, MTime : Positive;
        type DamageFactor is digits 2 range 0.0..1.0;
        Damage : DamageFactor := 0.0;
    begin
        if InstallView then
            TextCost := To_Unbounded_String("Install cost:");
            TextTime := To_Unbounded_String("Installation time:");
            Cost := Modules_List(ModuleIndex).Price;
            MTime := Modules_List(ModuleIndex).InstallTime;
        else
            TextCost := To_Unbounded_String("Remove gain:");
            TextTime := To_Unbounded_String("Removing time:");
            Damage := 1.0 - DamageFactor(Float(PlayerShip.Modules.Element(ModuleIndex).Durability) / 
                Float(PlayerShip.Modules.Element(ModuleIndex).MaxDurability));
            Cost := Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).Price -
                Integer(Float(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).Price) * 
                Float(Damage));
            MTime := Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).InstallTime;
        end if;
        InfoWindow := Create(8, (Columns / 2), 3, (Columns / 2));
        Add(Win => InfoWindow, Str => To_String(TextCost) & Positive'Image(Cost) & " Charcollum");
        Move_Cursor(Win => InfoWindow, Line => 1, Column => 0);
        Add(Win => InfoWindow, Str => To_String(TextTime) & Positive'Image(MTime) & " minutes");
        Move_Cursor(Win => InfoWindow, Line => 2, Column => 0);
        if InstallView then
            case Modules_List.Element(ModuleIndex).MType is
                when HULL =>
                    Add(Win => InfoWindow, Str => "Ship hull can be only replaced.");
                    Move_Cursor(Win => InfoWindow, Line => 3, Column => 0);
                    Add(Win => InfoWindow, Str => "Modules space:" & Positive'Image(Modules_List.Element(ModuleIndex).MaxValue));
                    CurrentLine := 5;
                when ENGINE =>
                    Add(Win => InfoWindow, Str => "Max power:" & Positive'Image(Modules_List.Element(ModuleIndex).MaxValue));
                    Move_Cursor(Win => InfoWindow, Line => 3, Column => 0);
                    Add(Win => InfoWindow, Str => "Fuel usage:" & Positive'Image(Modules_List.Element(ModuleIndex).Value));
                    CurrentLine := 5;
                when CARGO =>
                    Add(Win => InfoWindow, Str => "Max cargo:" & Positive'Image(Modules_List.Element(ModuleIndex).MaxValue) & " kg");
                    CurrentLine := 4;
                when others =>
                    null;
            end case;
            if Modules_List.Element(ModuleIndex).Size > 0 then
                Move_Cursor(Win => InfoWindow, Line => CurrentLine - 1, Column => 0);
                Add(Win => InfoWindow, Str => "Size:" & Natural'Image(Modules_List.Element(ModuleIndex).Size));
                CurrentLine := CurrentLine + 1;
            end if;
            Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
            Add(Win => InfoWindow, Str => "Press ENTER to install module");
            Change_Attributes(Win => InfoWindow, Line => CurrentLine, Column => 6, Count => 5, Color => 1);
        else
            case Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).MType is
                when ENGINE =>
                    Add(Win => InfoWindow, Str => "Max power:" & Positive'Image(PlayerShip.Modules.Element(ModuleIndex).Max_Value));
                    CurrentLine := 4;
                when CARGO =>
                    Add(Win => InfoWindow, Str => "Max cargo:" & Positive'Image(PlayerShip.Modules.Element(ModuleIndex).Max_Value)
                        & " kg");
                    CurrentLine := 4;
                when others =>
                    null;
            end case;
            if  Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).Size > 0 then
                Move_Cursor(Win => InfoWindow, Line => CurrentLine - 1, Column => 0);
                Add(Win => InfoWindow, Str => "Size:" & 
                    Natural'Image(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).Size));
                CurrentLine := CurrentLine + 1;
            end if;
            Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
            Add(Win => InfoWindow, Str => "Press ENTER to remove module");
            Change_Attributes(Win => InfoWindow, Line => CurrentLine, Column => 6, Count => 5, Color => 1);
        end if;
        Refresh;
        Refresh(InfoWindow);
        Delete(InfoWindow);
    end ShowModuleInfo;

    procedure ShowShipyard is
        Modules_Items: Item_Array_Access;
        MenuHeight : Line_Position;
        MenuLength : Column_Position;
        MenuIndex : Integer := 1;
        MenuOptions : Menu_Option_Set;
        MoneyIndex : Natural;
    begin
        Move_Cursor(Line => 2, Column => 2);
        Add(Str => "[Install] [Remove]");
        Change_Attributes(Line => 2, Column => 3, Count => 1, Color => 1);
        Change_Attributes(Line => 2, Column => 13, Count => 1, Color => 1);
        if InstallView then
            Modules_Items := new Item_Array(Modules_List.First_Index..(Modules_List.Last_Index + 1));
            for J in ModuleType'Range loop
                for I in Modules_List.First_Index..Modules_List.Last_Index loop
                    if Modules_List.Element(I).Price > 0 and Modules_List.Element(I).MType = J then
                        Modules_Items.all(MenuIndex) := New_Item(To_String(Modules_List.Element(I).Name), 
                        Positive'Image(I));
                        MenuIndex := MenuIndex + 1;
                    end if;
                end loop;
            end loop;
        else
            Modules_Items := new Item_Array(PlayerShip.Modules.First_Index..(PlayerShip.Modules.Last_Index + 1));
            for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
                if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType /= HULL then
                    Modules_Items.all(MenuIndex) := New_Item(To_String(PlayerShip.Modules.Element(I).Name), 
                        Positive'Image(I));
                    MenuIndex := MenuIndex + 1;
                end if;
            end loop;
        end if;
        for I in MenuIndex..Modules_Items'Last loop
            Modules_Items.all(I) := Null_Item;
        end loop;
        TradeMenu := New_Menu(Modules_Items);
        MenuOptions := Get_Options(TradeMenu);
        MenuOptions.Show_Descriptions := False;
        Set_Options(TradeMenu, MenuOptions);
        Set_Format(TradeMenu, Lines - 10, 1);
        Set_Mark(TradeMenu, "");
        Scale(TradeMenu, MenuHeight, MenuLength);
        MenuWindow := Create(MenuHeight, MenuLength, 4, 2);
        Set_Window(TradeMenu, MenuWindow);
        Set_Sub_Window(TradeMenu, Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
        Post(TradeMenu);
        MoneyIndex := FindMoney;
        Move_Cursor(Line => (MenuHeight + 5), Column => 2);
        if MoneyIndex > 0 then
            Add(Str => "You have" & Natural'Image(PlayerShip.Cargo.Element(MoneyIndex).Amount) & " Charcollum.");
        elsif InstallView then
            Add(Str => "You don't have any Charcollum to install anything.");
        end if;
        Move_Cursor(Line => (MenuHeight + 6), Column => 2);
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            if Modules_List.Element(PlayerShip.Modules.Element(I).ProtoIndex).MType = HULL then
                Add(Str => "You have used" & Natural'Image(PlayerShip.Modules.Element(I).Current_Value) & " modules space from max" &
                    Natural'Image(PlayerShip.Modules.Element(I).Max_Value) & " allowed.");
                exit;
            end if;
        end loop;
        ShowModuleInfo;
        Refresh(MenuWindow);
    end ShowShipyard;

    procedure ShowRecruitInfo is
        InfoWindow : Window;
        BaseIndex : constant Positive := SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
        RecruitIndex : constant Positive := Get_Index(Current(TradeMenu));
        Recruit : constant Recruit_Data := SkyBases(BaseIndex).Recruits.Element(RecruitIndex);
        CurrentLine : Line_Position := 1;
        SkillLevel : Unbounded_String;
    begin
        InfoWindow := Create((Lines - 5), (Columns / 2), 3, (Columns / 2));
        if Recruit.Gender = 'M' then
            Add(Win => InfoWindow, Str => "Male");
        else
            Add(Win => InfoWindow, Str => "Female");
        end if;
        CurrentLine := CurrentLine + 1;
        for I in Recruit.Skills.First_Index..Recruit.Skills.Last_Index loop
            if Recruit.Skills.Element(I)(2) > 0 and Recruit.Skills.Element(I)(2) < 20 then
                SkillLevel := To_Unbounded_String("Novice");
            elsif Recruit.Skills.Element(I)(2) > 19 and Recruit.Skills.Element(I)(2) < 40 then
                SkillLevel := To_Unbounded_String("Beginner");
            elsif Recruit.Skills.Element(I)(2) > 39 and Recruit.Skills.Element(I)(2) < 60 then
                SkillLevel := To_Unbounded_String("Competent");
            elsif Recruit.Skills.Element(I)(2) > 59 and Recruit.Skills.Element(I)(2) < 80 then
                SkillLevel := To_Unbounded_String("Expert");
            elsif Recruit.Skills.Element(I)(2) > 79 and Recruit.Skills.Element(I)(2) < 100 then
                SkillLevel := To_Unbounded_String("Master");
            elsif Recruit.Skills.Element(I)(2) > 99 then
                SkillLevel := To_Unbounded_String("Grandmaster");
            end if;
            Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
            Add(Win => InfoWindow, Str => To_String(Skills_Names.Element(Recruit.Skills.Element(I)(1))) & ": " & To_String(SkillLevel));
            CurrentLine := CurrentLine + 1;
        end loop;
        CurrentLine := CurrentLine + 1;
        Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
        Add(Win => InfoWindow, Str => "Hire for" & Positive'Image(Recruit.Price) & " Charcollum.");
        Change_Attributes(Win => InfoWindow, Line => CurrentLine, Column => 0, Count => 1, Color => 1);
        Refresh;
        Refresh(InfoWindow);
        Delete(InfoWindow);
    end ShowRecruitInfo;

    procedure ShowRecruits is
        BaseIndex : constant Positive := SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
        Recruits_Items : Item_Array_Access;
        MenuHeight : Line_Position;
        MenuLength : Column_Position;
        MoneyIndex : Natural := 0;
    begin
        if SkyBases(BaseIndex).Recruits.Length = 0 then
            Move_Cursor(Line => (Lines / 3), Column => (Columns / 3));
            Add(Str => "Here no recruits to hire.");
            Refresh;
            return;
        end if;
        Recruits_Items := new Item_Array(SkyBases(BaseIndex).Recruits.First_Index..(SkyBases(BaseIndex).Recruits.Last_Index + 1));
        for I in SkyBases(BaseIndex).Recruits.First_Index..SkyBases(BaseIndex).Recruits.Last_Index loop
            Recruits_Items.all(I) := New_Item(To_String(SkyBases(BaseIndex).Recruits.Element(I).Name));
        end loop;
        Recruits_Items.all(Recruits_Items'Last) := Null_Item;
        TradeMenu := New_Menu(Recruits_Items);
        Set_Format(TradeMenu, Lines - 10, 1);
        Set_Mark(TradeMenu, "");
        Scale(TradeMenu, MenuHeight, MenuLength);
        MenuWindow := Create(MenuHeight, MenuLength, 3, 2);
        Set_Window(TradeMenu, MenuWindow);
        Set_Sub_Window(TradeMenu, Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
        Post(TradeMenu);
        MoneyIndex := FindMoney;
        Move_Cursor(Line => (MenuHeight + 4), Column => 2);
        if MoneyIndex > 0 then
            Add(Str => "You have" & Natural'Image(PlayerShip.Cargo.Element(MoneyIndex).Amount) &
                " Charcollum.");
        else
            Add(Str => "You don't have any Charcollum to hire anyone.");
        end if;
        ShowRecruitInfo;
        Refresh(MenuWindow);
    end ShowRecruits;
    
    function TradeKeys(Key : Key_Code) return GameStates is
        Result : Driver_Result;
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
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
                ShowForm;
            when 10 => -- Buy item
                ShowForm(True);
            when others =>
                null;
        end case;
        return Trade_View;
    end TradeKeys;

    function RepairKeys(Key : Key_Code) return GameStates is
        Result : Driver_Result;
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when 56 | KEY_UP => -- Select previous repair option
                Result := Driver(TradeMenu, M_Up_Item);
                if Result = Request_Denied then
                    Result := Driver(TradeMenu, M_Last_Item);
                end if;
                if Result = Menu_Ok then
                    ShowRepairInfo;
                    Refresh(MenuWindow);
                end if;
            when 50 | KEY_DOWN => -- Select next repair option
                Result := Driver(TradeMenu, M_Down_Item);
                if Result = Request_Denied then
                    Result := Driver(TradeMenu, M_First_Item);
                end if;
                if Result = Menu_Ok then
                    ShowRepairInfo;
                    Refresh(MenuWindow);
                end if;
            when 10 => -- Repair ship
                RepairShip;
                DrawGame(Repairs_View);
            when others =>
                null;
        end case;
        return Repairs_View;
    end RepairKeys;

    function ShipyardKeys(Key : Key_Code) return GameStates is
        Result : Driver_Result;
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                InstallView := True;
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when 56 | KEY_UP => -- Select previous repair option
                Result := Driver(TradeMenu, M_Up_Item);
                if Result = Request_Denied then
                    Result := Driver(TradeMenu, M_Last_Item);
                end if;
                if Result = Menu_Ok then
                    ShowModuleInfo;
                    Refresh(MenuWindow);
                end if;
            when 50 | KEY_DOWN => -- Select next repair option
                Result := Driver(TradeMenu, M_Down_Item);
                if Result = Request_Denied then
                    Result := Driver(TradeMenu, M_First_Item);
                end if;
                if Result = Menu_Ok then
                    ShowModuleInfo;
                    Refresh(MenuWindow);
                end if;
            when Character'Pos('i') | Character'Pos('I') => -- Show modules to install
                InstallView := True;
                DrawGame(Shipyard_View);
            when Character'Pos('r') | Character'Pos('R') => -- Show modules to remove
                InstallView := False;
                DrawGame(Shipyard_View);
            when 10 => -- Install/remove module
                Bases.UpgradeShip(InstallView, Positive'Value(Description(Current(TradeMenu))));
                DrawGame(Shipyard_View);
            when others =>
                null;
        end case;
        return Shipyard_View;
    end ShipyardKeys;

    function RecruitKeys(Key : Key_Code) return GameStates is
        Result : Driver_Result;
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                InstallView := True;
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when 56 | KEY_UP => -- Select previous recruit to hire
                Result := Driver(TradeMenu, M_Up_Item);
                if Result = Request_Denied then
                    Result := Driver(TradeMenu, M_Last_Item);
                end if;
                if Result = Menu_Ok then
                    ShowRecruitInfo;
                    Refresh(MenuWindow);
                end if;
            when 50 | KEY_DOWN => -- Select next recruit to hire
                Result := Driver(TradeMenu, M_Down_Item);
                if Result = Request_Denied then
                    Result := Driver(TradeMenu, M_First_Item);
                end if;
                if Result = Menu_Ok then
                    ShowRecruitInfo;
                    Refresh(MenuWindow);
                end if;
            when Character'Pos('h') | Character'Pos('H') => -- Show modules to install
                HireRecruit(Get_Index(Current(TradeMenu)));
                DrawGame(Recruits_View);
            when others =>
                null;
        end case;
        return Recruits_View;
    end RecruitKeys;

end Bases.UI;
