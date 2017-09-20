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
with Items.UI; use Items.UI;

package body Ships.UI.Cargo is

   MenuWindow, MenuWindow2: Window;
   CurrentMenuIndex: Positive := 1;
   OptionsMenu: Menu;

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
         ShowItemStatus(PlayerShip.Cargo, ItemIndex, InfoWindow, CurrentLine);
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
      Add(Str => "Press Enter to see item options");
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

   procedure ShowCargoMenu is
      Options_Items: constant Item_Array_Access := new Item_Array(1 .. 4);
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
   begin
      Options_Items.all :=
        (New_Item("Drop item"),
         New_Item("Give item to crew member"),
         New_Item("Close"),
         Null_Item);
      OptionsMenu := New_Menu(Options_Items);
      Set_Format(OptionsMenu, Lines - 10, 1);
      Scale(OptionsMenu, MenuHeight, MenuLength);
      MenuWindow2 :=
        Create
          (MenuHeight + 2,
           MenuLength + 2,
           ((Lines / 3) - (MenuHeight / 2)),
           ((Columns / 2) - (MenuLength / 2)));
      WindowFrame(MenuWindow2, 5, "Item options");
      Set_Window(OptionsMenu, MenuWindow2);
      Set_Sub_Window
        (OptionsMenu,
         Derived_Window(MenuWindow2, MenuHeight, MenuLength, 1, 1));
      Post(OptionsMenu);
      Refresh_Without_Update;
      Refresh_Without_Update(MenuWindow2);
      Update_Screen;
   end ShowCargoMenu;

   function CargoInfoKeys
     (Key: Key_Code;
      OldState: GameStates) return GameStates is
      Result: Menus.Driver_Result;
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
         when 10 => -- Show options for selected cargo
            ShowCargoMenu;
            return Cargo_Menu;
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

   function CargoMenuKeys(Key: Key_Code) return GameStates is
      Result: Menus.Driver_Result;
      Option: constant String := Name(Current(OptionsMenu));
      ItemName: constant String :=
        To_String
          (Items_List(PlayerShip.Cargo(CurrentMenuIndex).ProtoIndex).Name);
   begin
      case Key is
         when 56 | KEY_UP => -- Select previous option
            Result := Driver(OptionsMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(OptionsMenu, M_Last_Item);
            end if;
         when 50 | KEY_DOWN => -- Select next option
            Result := Driver(OptionsMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(OptionsMenu, M_First_Item);
            end if;
         when 10 => -- Select option
            Post(OptionsMenu, False);
            Delete(OptionsMenu);
            if Option = "Drop item" then
               ShowShipForm
                 ("Amount of " & ItemName & " to drop:",
                  PlayerShip.Cargo.Element(CurrentMenuIndex).Amount);
               return Drop_Cargo;
            end if;
            DrawGame(Cargo_Info);
            return Cargo_Info;
         when 27 => -- Esc select close option, used second time, close menu
            if Option = "Close" then
               Post(OptionsMenu, False);
               Delete(OptionsMenu);
               DrawGame(Cargo_Info);
               return Cargo_Info;
            end if;
            Result := Driver(OptionsMenu, M_Last_Item);
         when others =>
            Result := Driver(OptionsMenu, Key);
            if Result /= Menu_Ok then
               Result := Driver(OptionsMenu, M_Clear_Pattern);
               Result := Driver(OptionsMenu, Key);
            end if;
      end case;
      if Result = Menu_Ok then
         Refresh(MenuWindow2);
      end if;
      return Cargo_Menu;
   end CargoMenuKeys;

end Ships.UI.Cargo;
