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

with Items; use Items;
with UserInterface; use UserInterface;
with Ships.Cargo; use Ships.Cargo;

package body Ships.UI.Cargo is

    MenuWindow : Window;
    CurrentMenuIndex : Positive := 1;

    procedure ShowItemInfo is
        InfoWindow : Window;
        ItemIndex : constant Positive := Get_Index(Current(ShipsMenu));
        ItemWeight : constant Positive := PlayerShip.Cargo.Element(ItemIndex).Amount * 
            Items_List.Element(PlayerShip.Cargo.Element(ItemIndex).ProtoIndex).Weight;
        CurrentLine : Line_Position := 1;
        DamagePercent : Natural;
    begin
        InfoWindow := Create(10, (Columns / 2), 3, (Columns / 2));
        if PlayerShip.Cargo.Element(ItemIndex).Durability < 100 then
            Add(Win => InfoWindow, Str => "Status: ");
            DamagePercent := 100 - Natural((Float(PlayerShip.Cargo.Element(ItemIndex).Durability) / 100.0) * 100.0);
            if DamagePercent > 0 and DamagePercent < 20 then
                Add(Win => InfoWindow, Str => "Slightly used");
            elsif DamagePercent > 19 and DamagePercent < 50 then
                Add(Win => InfoWindow, Str => "Damaged");
            elsif DamagePercent > 49 and DamagePercent < 80 then
                Add(Win => InfoWindow, Str => "Heavily damaged");
            elsif DamagePercent > 79 and DamagePercent < 100 then
                Add(Win => InfoWindow, Str => "Almost destroyed");
            end if;
            Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
            CurrentLine := 2;
        end if;
        Add(Win => InfoWindow, Str => "Type: ");
        if Items_List.Element(PlayerShip.Cargo.Element(ItemIndex).ProtoIndex).ShowType = Null_Unbounded_String then
            Add(Win => InfoWindow, Str => To_String(Items_List.Element(PlayerShip.Cargo.Element(ItemIndex).ProtoIndex).IType));
        else
            Add(Win => InfoWindow, Str => To_String(Items_List.Element(PlayerShip.Cargo.Element(ItemIndex).ProtoIndex).ShowType));
        end if;
        Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
        Add(Win => InfoWindow, Str => "Amount:" & Positive'Image(PlayerShip.Cargo.Element(ItemIndex).Amount));
        CurrentLine := CurrentLine + 1;
        Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
        Add(Win => InfoWindow, Str => "Weight:" &
            Positive'Image(Items_List.Element(PlayerShip.Cargo.Element(ItemIndex).ProtoIndex).Weight) & " kg");
        CurrentLine := CurrentLine + 1;
        Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
        CurrentLine := CurrentLine + 1;
        Add(Win => InfoWindow, Str => "Total weight:" & Positive'Image(ItemWeight) & " kg");
        CurrentLine := CurrentLine + 2;
        Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
        Add(Win => InfoWindow, Str => "Press Enter to drop cargo");
        Change_Attributes(Win => InfoWindow, Line => CurrentLine, Column => 6, 
            Count => 5, Color => 1);
        Refresh;
        Refresh(InfoWindow);
        Delete(InfoWindow);
    end ShowItemInfo;

    procedure ShowCargoInfo is
        Cargo_Items: constant Item_Array_Access := new Item_Array(1..(PlayerShip.Cargo.Last_Index + 1));
        MenuHeight : Line_Position;
        MenuLength : Column_Position;
        FreeSpace : Integer;
    begin
        for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
            Cargo_Items.all(I) := New_Item(GetCargoName(I));
        end loop;
        Cargo_Items.all(Cargo_Items'Last) := Null_Item;
        ShipsMenu := New_Menu(Cargo_Items);
        Set_Format(ShipsMenu, Lines - 10, 1);
        Set_Mark(ShipsMenu, "");
        Scale(ShipsMenu, MenuHeight, MenuLength);
        MenuWindow := Create(MenuHeight, MenuLength, 3, 2);
        Set_Window(ShipsMenu, MenuWindow);
        Set_Sub_Window(ShipsMenu, Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
        Post(ShipsMenu);
        if Cargo_Items.all(CurrentMenuIndex) = Null_Item then
            CurrentMenuIndex := 1;
        end if;
        Set_Current(ShipsMenu, Cargo_Items.all(CurrentMenuIndex));
        ShowItemInfo;
        Refresh(MenuWindow);
        Move_Cursor(Line => MenuHeight + 4, Column => 2);
        FreeSpace := FreeCargo(0);
        if FreeSpace < 0 then
            FreeSpace := 0;
        end if;
        Add(Str => "Free cargo space:" & Integer'Image(FreeSpace) & " kg");
    end ShowCargoInfo;

    function CargoInfoKeys(Key : Key_Code; OldState : GameStates) return GameStates is
        Result : Menus.Driver_Result;
        ItemName : constant String := To_String(Items_List.Element(PlayerShip.Cargo.Element(CurrentMenuIndex).ProtoIndex).Name);
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back sky map or combat screen
                CurrentMenuIndex := 1;
                DrawGame(OldState);
                return OldState;
            when 56 | KEY_UP => -- Select previous item
                Result := Driver(ShipsMenu, M_Up_Item);
                if Result = Request_Denied then
                    Result := Driver(ShipsMenu, M_Last_Item);
                end if;
                if Result = Menu_Ok then
                    ShowItemInfo;
                    Refresh(MenuWindow);
                end if;
            when 50 | KEY_DOWN => -- Select next item
                Result := Driver(ShipsMenu, M_Down_Item);
                if Result = Request_Denied then
                    Result := Driver(ShipsMenu, M_First_Item);
                end if;
                if Result = Menu_Ok then
                    ShowItemInfo;
                    Refresh(MenuWindow);
                end if;
            when 10 => -- Drop selected cargo
                ShowShipForm("Amount of " & ItemName & " to drop:", PlayerShip.Cargo.Element(CurrentMenuIndex).Amount);
                return Drop_Cargo;
            when others =>
                Result := Driver(ShipsMenu, Key);
                if Result = Menu_Ok then
                    ShowItemInfo;
                    Refresh(MenuWindow);
                else
                    Result := Driver(ShipsMenu, M_CLEAR_PATTERN);
                    Result := Driver(ShipsMenu, Key);
                    if Result = Menu_Ok then
                        ShowItemInfo;
                        Refresh(MenuWindow);
                    end if;
                end if;
        end case;
        CurrentMenuIndex := Get_Index(Current(ShipsMenu));
        return Cargo_Info;
    end CargoInfoKeys;

end Ships.UI.Cargo;
