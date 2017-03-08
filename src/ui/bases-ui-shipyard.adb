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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Items; use Items;
with UserInterface; use UserInterface;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with ShipModules; use ShipModules;

package body Bases.UI.Shipyard is
    
    TypesMenu : Menu;
    MenuWindow2 : Window;
    InstallView : Boolean := True;
    ModulesType : ModuleType := ANY;
    ModulesNames : constant array (Natural range <>) of Unbounded_String :=
        (To_Unbounded_String("Any"), To_Unbounded_String("Engines"),
        To_Unbounded_String("Cabins"), To_Unbounded_String("Cockpits"),
        To_Unbounded_String("Turrets"), To_Unbounded_String("Guns"),
        To_Unbounded_String("Cargo bays"), To_Unbounded_String("Hulls"),
        To_Unbounded_String("Armors"), To_Unbounded_String("Battering rams"),
        To_Unbounded_String("Alchemy labs"), To_Unbounded_String("Furnaces"),
        To_Unbounded_String("Water collectors"), To_Unbounded_String("Workshops"),
        To_Unbounded_String("Greenhouses"), To_Unbounded_String("Medical rooms"));

    procedure ShowModuleInfo is
        ModuleIndex : constant Positive := Positive'Value(Description(Current(TradeMenu)));
        InfoWindow : Window;
        TextCost, TextTime : Unbounded_String;
        CurrentLine : Line_Position := 3;
        Cost, MTime : Positive;
        type DamageFactor is digits 2 range 0.0..1.0;
        Damage : DamageFactor := 0.0;
        MAmount : Natural;
        StartColumn : Column_Position;
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
        InfoWindow := Create(15, (Columns / 2), 4, (Columns / 2));
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
                when ShipModules.CARGO =>
                    Add(Win => InfoWindow, Str => "Max cargo:" & Positive'Image(Modules_List.Element(ModuleIndex).MaxValue) & " kg");
                    CurrentLine := 4;
                when CABIN =>
                    Add(Win => InfoWindow, Str => "Quality: ");
                    if Modules_List.Element(ModuleIndex).MaxValue < 30 then
                        Add(Win => InfoWindow, Str => "minimal");
                    elsif Modules_List.Element(ModuleIndex).MaxValue > 29 and Modules_List.Element(ModuleIndex).MaxValue < 60 then
                        Add(Win => InfoWindow, Str => "basic");
                    elsif Modules_List.Element(ModuleIndex).MaxValue > 59 and Modules_List.Element(ModuleIndex).MaxValue < 80 then
                        Add(Win => InfoWindow, Str => "extended");
                    else
                        Add(Win => InfoWindow, Str => "luxury");
                    end if;
                    CurrentLine := 4;
                when GUN =>
                    Add(Win => InfoWindow, Str => "Ammunition: ");
                    MAmount := 0;
                    for I in Items_List.First_Index..Items_List.Last_Index loop
                        if Items_List.Element(I).IType = Items_Types.Element(Modules_List.Element(ModuleIndex).Value) then
                            if MAmount > 0 then
                                Add(Win => InfoWindow, Str => " or ");
                            end if;
                            Add(Win => InfoWindow, Str => To_String(Items_List.Element(I).Name));
                            MAmount := MAmount + 1;
                        end if;
                    end loop;
                    CurrentLine := 4;
                when others =>
                    null;
            end case;
            if Modules_List.Element(ModuleIndex).Size > 0 then
                Move_Cursor(Win => InfoWindow, Line => CurrentLine - 1, Column => 0);
                Add(Win => InfoWindow, Str => "Size:" & Natural'Image(Modules_List.Element(ModuleIndex).Size));
                CurrentLine := CurrentLine + 1;
            end if;
            if Modules_List.Element(ModuleIndex).Weight > 0 then
                Move_Cursor(Win => InfoWindow, Line => CurrentLine - 1, Column => 0);
                Add(Win => InfoWindow, Str => "Weight:" & Natural'Image(Modules_List.Element(ModuleIndex).Weight) & " kg");
                CurrentLine := CurrentLine + 1;
            end if;
            Move_Cursor(Win => InfoWindow, Line => CurrentLine - 1, Column => 0);
            Add(Win => InfoWindow, Str => "Repair/Upgrade material: ");
            MAmount := 0;
            for I in Items_List.First_Index..Items_List.Last_Index loop
                if Items_List.Element(I).IType = Modules_List.Element(ModuleIndex).RepairMaterial
                then
                    if MAmount > 0 then
                        Add(Win => InfoWindow, Str => " or ");
                    end if;
                    Add(Win => InfoWindow, Str => To_String(Items_List.Element(I).Name));
                    MAmount := MAmount + 1;
                end if;
            end loop;
            Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
            Add(Win => InfoWindow, Str => "Repair/Upgrade skill: " &
            To_String(Skills_Names.Element(Modules_List.Element(ModuleIndex).RepairSkill)));
            if Modules_List.Element(ModuleIndex).Description /= Null_Unbounded_String then
                CurrentLine := CurrentLine + 2;
                Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
                Add(Win => InfoWindow, Str => To_String(Modules_List.Element(ModuleIndex).Description));
                Get_Cursor_Position(Win => InfoWindow, Line => CurrentLine, Column => StartColumn);
            end if;
            CurrentLine := CurrentLine + 2;
            Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
            Add(Win => InfoWindow, Str => "Press ENTER to install module");
            Change_Attributes(Win => InfoWindow, Line => CurrentLine, Column => 6, Count => 5, Color => 1);
        else
            case Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).MType is
                when ENGINE =>
                    Add(Win => InfoWindow, Str => "Max power:" & Positive'Image(PlayerShip.Modules.Element(ModuleIndex).Max_Value));
                    CurrentLine := 4;
                when ShipModules.CARGO =>
                    Add(Win => InfoWindow, Str => "Max cargo:" & Positive'Image(PlayerShip.Modules.Element(ModuleIndex).Max_Value)
                        & " kg");
                    CurrentLine := 4;
                when CABIN =>
                    Add(Win => InfoWindow, Str => "Quality: ");
                    if PlayerShip.Modules.Element(ModuleIndex).Max_Value < 30 then
                        Add(Win => InfoWindow, Str => "minimal");
                    elsif PlayerShip.Modules.Element(ModuleIndex).Max_Value > 29 and PlayerShip.Modules.Element(ModuleIndex).Max_Value < 60 
                    then
                        Add(Win => InfoWindow, Str => "basic");
                    elsif PlayerShip.Modules.Element(ModuleIndex).Max_Value > 59 and PlayerShip.Modules.Element(ModuleIndex).Max_Value < 80 
                    then
                        Add(Win => InfoWindow, Str => "extended");
                    else
                        Add(Win => InfoWindow, Str => "luxury");
                    end if;
                    CurrentLine := 4;
                when GUN =>
                    Add(Win => InfoWindow, Str => "Ammunition: ");
                    MAmount := 0;
                    for I in Items_List.First_Index..Items_List.Last_Index loop
                        if Items_List.Element(I).IType = 
                            Items_Types.Element(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).Value) 
                        then
                            if MAmount > 0 then
                                Add(Win => InfoWindow, Str => " or ");
                            end if;
                            Add(Win => InfoWindow, Str => To_String(Items_List.Element(I).Name));
                            MAmount := MAmount + 1;
                        end if;
                    end loop;
                    CurrentLine := 4;
                when others =>
                    null;
            end case;
            if Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).Size > 0 then
                Move_Cursor(Win => InfoWindow, Line => CurrentLine - 1, Column => 0);
                Add(Win => InfoWindow, Str => "Size:" & 
                    Natural'Image(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).Size));
                CurrentLine := CurrentLine + 1;
            end if;
            if PlayerShip.Modules.Element(ModuleIndex).Weight > 0 then
                Move_Cursor(Win => InfoWindow, Line => CurrentLine - 1, Column => 0);
                Add(Win => InfoWindow, Str => "Weight:" & Natural'Image(PlayerShip.Modules.Element(ModuleIndex).Weight) & " kg");
                CurrentLine := CurrentLine + 1;
            end if;
            if Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).Description /= Null_Unbounded_String then
                Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
                Add(Win => InfoWindow, 
                    Str => To_String(Modules_List.Element(PlayerShip.Modules.Element(ModuleIndex).ProtoIndex).Description));
                Get_Cursor_Position(Win => InfoWindow, Line => CurrentLine, Column => StartColumn);
                CurrentLine := CurrentLine + 2;
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
        procedure AddMenuItems(MType : ModuleType) is
        begin
            for I in Modules_List.First_Index..Modules_List.Last_Index loop
                if Modules_List.Element(I).Price > 0 and Modules_List.Element(I).MType = MType then
                    Modules_Items.all(MenuIndex) := New_Item(To_String(Modules_List.Element(I).Name), 
                    Positive'Image(I));
                    MenuIndex := MenuIndex + 1;
                end if;
            end loop;
        end AddMenuItems;
    begin
        Move_Cursor(Line => 2, Column => 2);
        if InstallView then
            Add(Str => "[Install] [F2 Remove]");
            Change_Attributes(Line => 2, Column => 13, Count => 2, Color => 1);
            Move_Cursor(Line => 2, Column => 24);
            Add(Str => "[F3 Show modules: " & To_Lower(To_String(ModulesNames(ModuleType'Pos(ModulesType)))) & "]");
            Change_Attributes(Line => 2, Column => 25, Count => 2, Color => 1);
            Modules_Items := new Item_Array(Modules_List.First_Index..(Modules_List.Last_Index + 1));
            if ModulesType = ANY then
                for I in ModuleType'Range loop
                    AddMenuItems(I);
                end loop;
            else
                AddMenuItems(ModulesType);
            end if;
        else
            Add(Str => "[F2 Install] [Remove]");
            Change_Attributes(Line => 2, Column => 3, Count => 2, Color => 1);
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
        Set_Format(TradeMenu, Lines - 7, 1);
        Set_Mark(TradeMenu, "");
        Scale(TradeMenu, MenuHeight, MenuLength);
        MenuWindow := Create(MenuHeight, MenuLength, 4, 2);
        Set_Window(TradeMenu, MenuWindow);
        Set_Sub_Window(TradeMenu, Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
        Post(TradeMenu);
        if CurrentMenuIndex >= Modules_Items'Last then
            CurrentMenuIndex := 1;
        end if;
        if Modules_Items.all(CurrentMenuIndex) = Null_Item then
            CurrentMenuIndex := 1;
        end if;
        Set_Current(TradeMenu, Modules_Items.all(CurrentMenuIndex));
        MoneyIndex := FindCargo(1);
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

    procedure ShowTypesMenu is
        Types_Items : constant Item_Array_access := new Item_Array(1..(ModulesNames'Last + 3));
        MenuHeight : Line_Position;
        MenuLength : Column_Position;
    begin
        for I in ModulesNames'Range loop
            Types_Items.all(I + 1) := New_Item(To_String(ModulesNames(I)));
        end loop;
        Types_Items.all(Types_Items'Last - 1) := New_Item("Quit");
        Types_Items.all(Types_Items'Last) := Null_Item;
        TypesMenu := New_Menu(Types_Items);
        Set_Mark(TypesMenu, "");
        Scale(TypesMenu, MenuHeight, MenuLength);
        MenuWindow2 := Create(MenuHeight + 2, MenuLength + 2, ((Lines / 3) - (MenuHeight / 2)), ((Columns / 2) - (MenuLength / 2)));
        Box(MenuWindow2);
        Set_Window(TypesMenu, MenuWindow2);
        Set_Sub_Window(TypesMenu, Derived_Window(MenuWindow2, MenuHeight, MenuLength, 1, 1));
        Post(TypesMenu);
        Refresh;
        Refresh(MenuWindow2);
    end ShowTypesMenu;
    
    function ShipyardKeys(Key : Key_Code) return GameStates is
        Result : Menus.Driver_Result;
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                CurrentMenuIndex := 1;
                InstallView := True;
                ModulesType := ANY;
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
            when KEY_F2 => -- Switch modules to install/remove
                if not InstallView then
                    InstallView := True;
                else
                    InstallView := False;
                end if;
                CurrentMenuIndex := 1;
                DrawGame(Shipyard_View);
            when KEY_F3 => -- Show select modules type menu
                if InstallView then
                    ShowTypesMenu;
                    return ShipyardTypesMenu;
                end if;
            when 10 => -- Install/remove module
                Bases.UpgradeShip(InstallView, Positive'Value(Description(Current(TradeMenu))));
                DrawGame(Shipyard_View);
            when others =>
                Result := Driver(TradeMenu, Key);
                if Result = Menu_Ok then
                    ShowModuleInfo;
                    Refresh(MenuWindow);
                else
                    Result := Driver(TradeMenu, M_CLEAR_PATTERN);
                    Result := Driver(TradeMenu, Key);
                    if Result = Menu_Ok then
                        ShowModuleInfo;
                        Refresh(MenuWindow);
                    end if;
                end if;
        end case;
        CurrentMenuIndex := Menus.Get_Index(Current(TradeMenu));
        return Shipyard_View;
    end ShipyardKeys;

    function ShipyardTypesKeys(Key : Key_Code) return GameStates is
        Result : Menus.Driver_Result;
    begin
        case Key is
            when 56 | KEY_UP => -- Select previous type option
                Result := Driver(TypesMenu, M_Up_Item);
                if Result = Request_Denied then
                    Result := Driver(TypesMenu, M_Last_Item);
                end if;
                if Result = Menu_Ok then
                    Refresh(MenuWindow2);
                end if;
            when 50 | KEY_DOWN => -- Select next type option
                Result := Driver(TypesMenu, M_Down_Item);
                if Result = Request_Denied then
                    Result := Driver(TypesMenu, M_First_Item);
                end if;
                if Result = Menu_Ok then
                    Refresh(MenuWindow2);
                end if;
            when 10 => -- Set modules type to show
                if Name(Current(TypesMenu)) /= "Quit" then
                    CurrentMenuIndex := 1;
                    ModulesType := ModuleType'Val(Get_Index(Current(TypesMenu)) - 1);
                end if;
                DrawGame(Shipyard_View);
                return Shipyard_View;
            when others =>
                Result := Driver(TypesMenu, Key);
                if Result = Menu_Ok then
                    Refresh(MenuWindow2);
                else
                    Result := Driver(TypesMenu, M_CLEAR_PATTERN);
                    Result := Driver(TypesMenu, Key);
                    if Result = Menu_Ok then
                        Refresh(MenuWindow2);
                    end if;
                end if;
        end case;
        return ShipyardTypesMenu;
    end ShipyardTypesKeys;

end Bases.UI.Shipyard;
