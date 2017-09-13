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

with UserInterface; use UserInterface;
with Ships.Cargo; use Ships.Cargo;
with Utils.UI; use Utils.UI;

package body Ships.UI.Cargo is

   MenuWindow: Window;
   CurrentMenuIndex: Positive := 1;

   procedure ShowItemInfo is
      InfoWindow, ClearWindow, BoxWindow: Window;
      ItemIndex: constant Positive := Get_Index(Current(ShipsMenu));
      ItemWeight: constant Positive :=
        PlayerShip.Cargo(ItemIndex).Amount *
        Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).Weight;
      CurrentLine: Line_Position := 1;
      WindowHeight: Line_Position := 8;
      FreeSpace: Integer;
      WindowWidth: Column_Position;
   begin
      ClearWindow := Create(Lines - 3, (Columns / 2), 3, (Columns / 2));
      Refresh_Without_Update(ClearWindow);
      Delete(ClearWindow);
      if PlayerShip.Cargo(ItemIndex).Durability < 100 then
         WindowHeight := WindowHeight + 1;
      end if;
      WindowHeight :=
        WindowHeight +
        Line_Position
          ((Length
              (Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex)
                 .Description) /
            (Natural(Columns / 2) - 4)));
      WindowWidth :=
        Column_Position
          (Length
             (Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).Description) +
           5);
      if WindowWidth <
        Column_Position
          (Positive'Image
             (Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).Weight)'
             Length +
           5) then
         WindowWidth :=
           Column_Position
             (Positive'Image
                (Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).Weight)'
                Length +
              5);
      end if;
      if WindowWidth > (Columns / 2) then
         WindowWidth := Columns / 2;
      end if;
      BoxWindow := Create(WindowHeight, WindowWidth, 3, (Columns / 2));
      WindowFrame(BoxWindow, 2, "Item info");
      InfoWindow :=
        Create(WindowHeight - 2, WindowWidth - 4, 4, (Columns / 2) + 2);
      Add(Win => InfoWindow, Str => "Type: ");
      if Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).ShowType =
        Null_Unbounded_String then
         Add
           (Win => InfoWindow,
            Str =>
              To_String
                (Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).IType));
      else
         Add
           (Win => InfoWindow,
            Str =>
              To_String
                (Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).ShowType));
      end if;
      Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
      Add
        (Win => InfoWindow,
         Str =>
           "Amount:" & Positive'Image(PlayerShip.Cargo(ItemIndex).Amount));
      CurrentLine := CurrentLine + 1;
      Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
      Add
        (Win => InfoWindow,
         Str =>
           "Weight:" &
           Positive'Image
             (Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).Weight) &
           " kg");
      CurrentLine := CurrentLine + 1;
      Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
      Add
        (Win => InfoWindow,
         Str => "Total weight:" & Positive'Image(ItemWeight) & " kg");
      CurrentLine := CurrentLine + 1;
      if PlayerShip.Cargo(ItemIndex).Durability < 100 then
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
         Add(Win => InfoWindow, Str => "Status: ");
         ShowCargoStatus(ItemIndex, InfoWindow, CurrentLine);
         CurrentLine := CurrentLine + 1;
      end if;
      if Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).Description /=
        Null_Unbounded_String then
         CurrentLine := CurrentLine + 1;
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
         Add
           (Win => InfoWindow,
            Str =>
              To_String
                (Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex)
                   .Description));
      end if;
      CurrentLine := WindowHeight + 3;
      Move_Cursor(Line => CurrentLine, Column => (Columns / 2));
      Add(Str => "Press Enter to drop cargo");
      Change_Attributes
        (Line => CurrentLine,
         Column => (Columns / 2) + 6,
         Count => 5,
         Color => 1,
         Attr => BoldCharacters);
      CurrentLine := CurrentLine + 1;
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
      FreeSpace := FreeCargo(0);
      if FreeSpace < 0 then
         FreeSpace := 0;
      end if;
      Add(Str => "Free cargo space:" & Integer'Image(FreeSpace) & " kg");
      Refresh_Without_Update;
      Refresh_Without_Update(BoxWindow);
      Delete(BoxWindow);
      Refresh_Without_Update(InfoWindow);
      Delete(InfoWindow);
      Refresh_Without_Update(MenuWindow);
      Update_Screen;
   end ShowItemInfo;

   procedure ShowCargoInfo is
      Cargo_Items: constant Item_Array_Access :=
        new Item_Array(1 .. (PlayerShip.Cargo.Last_Index + 1));
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
   begin
      for I in PlayerShip.Cargo.First_Index .. PlayerShip.Cargo.Last_Index loop
         Cargo_Items.all(I) := New_Item(GetItemName(PlayerShip.Cargo(I)));
      end loop;
      Cargo_Items.all(Cargo_Items'Last) := Null_Item;
      ShipsMenu := New_Menu(Cargo_Items);
      Set_Format(ShipsMenu, Lines - 10, 1);
      Scale(ShipsMenu, MenuHeight, MenuLength);
      MenuWindow := Create(MenuHeight, MenuLength, 3, 2);
      Set_Window(ShipsMenu, MenuWindow);
      Set_Sub_Window
        (ShipsMenu,
         Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
      Post(ShipsMenu);
      if Cargo_Items.all(CurrentMenuIndex) = Null_Item then
         CurrentMenuIndex := 1;
      end if;
      Set_Current(ShipsMenu, Cargo_Items.all(CurrentMenuIndex));
      ShowItemInfo;
   end ShowCargoInfo;

   procedure ShowCargoStatus
     (CargoIndex: Positive;
      InfoWindow: Window;
      Line: Line_Position) is
      DamagePercent: Natural;
      TextLength: Positive;
      TextColor: Color_Pair;
   begin
      DamagePercent :=
        100 -
        Natural
          ((Float(PlayerShip.Cargo(CargoIndex).Durability) / 100.0) * 100.0);
      if DamagePercent > 0 and DamagePercent < 20 then
         Add(Win => InfoWindow, Str => "Slightly used");
         TextLength := 13;
         TextColor := 2;
      elsif DamagePercent > 19 and DamagePercent < 50 then
         Add(Win => InfoWindow, Str => "Damaged");
         TextLength := 7;
         TextColor := 1;
      elsif DamagePercent > 49 and DamagePercent < 80 then
         Add(Win => InfoWindow, Str => "Heavily damaged");
         TextLength := 15;
         TextColor := 3;
      elsif DamagePercent > 79 and DamagePercent < 100 then
         Add(Win => InfoWindow, Str => "Almost destroyed");
         TextLength := 16;
         TextColor := 4;
      end if;
      Change_Attributes
        (Win => InfoWindow,
         Line => Line,
         Column => 8,
         Count => TextLength,
         Color => TextColor);
   end ShowCargoStatus;

   function CargoInfoKeys
     (Key: Key_Code;
      OldState: GameStates) return GameStates is
      Result: Menus.Driver_Result;
      ItemName: constant String :=
        To_String
          (Items_List(PlayerShip.Cargo(CurrentMenuIndex).ProtoIndex).Name);
   begin
      case Key is
         when 27 => -- Back sky map or combat screen
            CurrentMenuIndex := 1;
            DrawGame(OldState);
            return OldState;
         when 56 | KEY_UP => -- Select previous item
            Result := Driver(ShipsMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(ShipsMenu, M_Last_Item);
            end if;
         when 50 | KEY_DOWN => -- Select next item
            Result := Driver(ShipsMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(ShipsMenu, M_First_Item);
            end if;
         when 10 => -- Drop selected cargo
            ShowShipForm
              ("Amount of " & ItemName & " to drop:",
               PlayerShip.Cargo.Element(CurrentMenuIndex).Amount);
            return Drop_Cargo;
         when others =>
            Result := Driver(ShipsMenu, Key);
            if Result /= Menu_Ok then
               Result := Driver(ShipsMenu, M_Clear_Pattern);
               Result := Driver(ShipsMenu, Key);
            end if;
      end case;
      if Result = Menu_Ok then
         ShowItemInfo;
      end if;
      CurrentMenuIndex := Get_Index(Current(ShipsMenu));
      return Cargo_Info;
   end CargoInfoKeys;

end Ships.UI.Cargo;
