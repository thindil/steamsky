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

with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Terminal_Interface.Curses.Forms.Field_Types.IntField;
with Terminal_Interface.Curses.Forms.Field_Types.Enumeration;
use Terminal_Interface.Curses.Forms.Field_Types.Enumeration;
with UserInterface; use UserInterface;
with Ships.Cargo; use Ships.Cargo;
with Utils.UI; use Utils.UI;
with Items.UI; use Items.UI;

package body Ships.UI.Cargo is

   MenuWindow, MenuWindow2: Window;
   CurrentMenuIndex: Positive := 1;
   OptionsMenu: Menu;
   MoveForm: Forms.Form;

   function MoveItemResult return GameStates is
      FieldIndex: constant Positive := Get_Index(Current(MoveForm));
      ItemIndex: constant Positive := Get_Index(Current(ShipsMenu));
      Item: constant InventoryData := PlayerShip.Cargo(ItemIndex);
      MemberName: Unbounded_String;
      Amount, MemberIndex: Positive;
      procedure RedrawScreen is
         Visibility: Cursor_Visibility := Invisible;
      begin
         Set_Cursor_Visibility(Visibility);
         Post(MoveForm, False);
         Delete(MoveForm);
         DrawGame(Cargo_Info);
      end RedrawScreen;
   begin
      if FieldIndex = 5 then
         RedrawScreen;
         return Cargo_Info;
      end if;
      MemberName := To_Unbounded_String(Get_Buffer(Fields(MoveForm, 4)));
      Trim(MemberName, Ada.Strings.Both);
      for I in PlayerShip.Crew.Iterate loop
         if PlayerShip.Crew(I).Name = MemberName then
            MemberIndex := Crew_Container.To_Index(I);
            exit;
         end if;
      end loop;
      Amount := Positive'Value(Get_Buffer(Fields(MoveForm, 2)));
      if FreeInventory
          (MemberIndex,
           0 - (Items_List(Item.ProtoIndex).Weight * Amount)) <
        0 then
         ShowDialog
           ("No free space in " &
            To_String(MemberName) &
            "'s inventory for that amount of " &
            GetItemName(Item));
         RedrawScreen;
         return Cargo_Info;
      end if;
      UpdateInventory(MemberIndex, Amount, Item.ProtoIndex, Item.Durability);
      UpdateCargo
        (Ship => PlayerShip,
         Amount => (0 - Amount),
         CargoIndex => ItemIndex);
      RedrawScreen;
      return Cargo_Info;
   exception
      when Constraint_Error =>
         ShowDialog("You entered wrong amount of item to move.");
         RedrawScreen;
         return Cargo_Info;
   end MoveItemResult;

   procedure ShowItemInfo is
      InfoWindow, ClearWindow, BoxWindow: Window;
      ItemIndex: constant Positive := Get_Index(Current(ShipsMenu));
      ProtoIndex: constant Positive := PlayerShip.Cargo(ItemIndex).ProtoIndex;
      ItemWeight: constant Positive :=
        PlayerShip.Cargo(ItemIndex).Amount * Items_List(ProtoIndex).Weight;
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
      if Items_List(ProtoIndex).IType = WeaponType then
         WindowHeight := WindowHeight + 1;
      end if;
      WindowHeight :=
        WindowHeight +
        Line_Position
          ((Length(Items_List(ProtoIndex).Description) /
            (Natural(Columns / 2) - 4)));
      WindowWidth :=
        Column_Position(Length(Items_List(ProtoIndex).Description) + 5);
      if WindowWidth <
        Column_Position
          (Positive'Image(Items_List(ProtoIndex).Weight)'Length + 5) then
         WindowWidth :=
           Column_Position
             (Positive'Image(Items_List(ProtoIndex).Weight)'Length + 5);
      end if;
      if WindowWidth > (Columns / 2) then
         WindowWidth := Columns / 2;
      end if;
      BoxWindow := Create(WindowHeight, WindowWidth, 3, (Columns / 2));
      WindowFrame(BoxWindow, 2, "Item info");
      InfoWindow :=
        Create(WindowHeight - 2, WindowWidth - 4, 4, (Columns / 2) + 2);
      Add(Win => InfoWindow, Str => "Type: ");
      if Items_List(ProtoIndex).ShowType = Null_Unbounded_String then
         Add
           (Win => InfoWindow,
            Str => To_String(Items_List(ProtoIndex).IType));
      else
         Add
           (Win => InfoWindow,
            Str => To_String(Items_List(ProtoIndex).ShowType));
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
           "Weight:" & Positive'Image(Items_List(ProtoIndex).Weight) & " kg");
      CurrentLine := CurrentLine + 1;
      Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
      Add
        (Win => InfoWindow,
         Str => "Total weight:" & Positive'Image(ItemWeight) & " kg");
      if Items_List(ProtoIndex).IType = WeaponType then
         CurrentLine := CurrentLine + 1;
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
         Add
           (Win => InfoWindow,
            Str =>
              "Skill: " &
              To_String(Skills_List(Items_List(ProtoIndex).Value(3)).Name) &
              "/" &
              To_String
                (Attributes_Names
                   (Skills_List(Items_List(ProtoIndex).Value(3)).Attribute)));
      end if;
      if PlayerShip.Cargo(ItemIndex).Durability < 100 then
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
         Add(Win => InfoWindow, Str => "Status: ");
         ShowItemStatus(PlayerShip.Cargo, ItemIndex, InfoWindow, CurrentLine);
         CurrentLine := CurrentLine + 1;
      end if;
      CurrentLine := CurrentLine + 1;
      if Items_List(ProtoIndex).Description /= Null_Unbounded_String then
         CurrentLine := CurrentLine + 1;
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
         Add
           (Win => InfoWindow,
            Str => To_String(Items_List(ProtoIndex).Description));
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
      if MoveForm /= Null_Form then
         Post(MoveForm, False);
         Delete(MoveForm);
      end if;
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

   procedure ShowCargoForm is
      Move_Fields: constant Field_Array_Access := new Field_Array(1 .. 7);
      FieldOptions: Field_Option_Set;
      FormHeight: Line_Position;
      FormLength: Column_Position;
      Visibility: Cursor_Visibility := Normal;
      ItemIndex: constant Positive := Menus.Get_Index(Current(ShipsMenu));
      MaxAmount: constant Positive := PlayerShip.Cargo(ItemIndex).Amount;
      FieldText: constant String :=
        "Enter amount (max" & Positive'Image(MaxAmount) & "): ";
      MembersList: Enumeration_Info (Integer(PlayerShip.Crew.Length));
   begin
      if MoveForm = Null_Form then
         Set_Cursor_Visibility(Visibility);
         Move_Fields.all(1) :=
           New_Field(1, Column_Position(FieldText'Length), 0, 0, 0, 0);
         FieldOptions := Get_Options(Move_Fields.all(1));
         Set_Buffer(Move_Fields.all(1), 0, FieldText);
         FieldOptions.Active := False;
         Set_Options(Move_Fields.all(1), FieldOptions);
         Move_Fields.all(2) :=
           New_Field(1, 20, 0, Column_Position(FieldText'Length), 0, 0);
         FieldOptions := Get_Options(Move_Fields.all(2));
         FieldOptions.Auto_Skip := False;
         Set_Options(Move_Fields.all(2), FieldOptions);
         Set_Foreground(Move_Fields.all(2), BoldCharacters, 11);
         Set_Background(Move_Fields.all(2), BoldCharacters, 11);
         Terminal_Interface.Curses.Forms.Field_Types.IntField.Set_Field_Type
           (Move_Fields.all(2),
            (0, 0, MaxAmount));
         Move_Fields.all(3) := New_Field(1, 3, 1, 0, 0, 0);
         FieldOptions := Get_Options(Move_Fields.all(3));
         Set_Buffer(Move_Fields.all(3), 0, "To:");
         FieldOptions.Active := False;
         Set_Options(Move_Fields.all(3), FieldOptions);
         for I in PlayerShip.Crew.Iterate loop
            MembersList.Names(Crew_Container.To_Index(I)) :=
              new String'(To_String(PlayerShip.Crew(I).Name));
         end loop;
         Move_Fields.all(4) := New_Field(1, 20, 1, 4, 0, 0);
         Set_Field_Type(Move_Fields.all(4), Create(MembersList, True));
         Set_Buffer(Move_Fields.all(4), 0, To_String(PlayerShip.Crew(1).Name));
         FieldOptions := Get_Options(Move_Fields.all(4));
         FieldOptions.Edit := False;
         Set_Options(Move_Fields.all(4), FieldOptions);
         Move_Fields.all(5) :=
           New_Field(1, 8, 3, (Column_Position(FieldText'Length) / 2), 0, 0);
         Set_Buffer(Move_Fields.all(5), 0, "[Cancel]");
         FieldOptions := Get_Options(Move_Fields.all(5));
         FieldOptions.Edit := False;
         Set_Options(Move_Fields.all(5), FieldOptions);
         Move_Fields.all(6) :=
           New_Field
             (1,
              4,
              3,
              (Column_Position(FieldText'Length) / 2) + 10,
              0,
              0);
         FieldOptions := Get_Options(Move_Fields.all(6));
         FieldOptions.Edit := False;
         Set_Options(Move_Fields.all(6), FieldOptions);
         Set_Buffer(Move_Fields.all(6), 0, "[Ok]");
         Move_Fields.all(7) := Null_Field;
         MoveForm := New_Form(Move_Fields);
         Scale(MoveForm, FormHeight, FormLength);
         MenuWindow2 :=
           Create
             (FormHeight + 2,
              FormLength + 2,
              ((Lines / 3) - (FormHeight / 2)),
              ((Columns / 2) - (FormLength / 2)));
         Box(MenuWindow2);
         WindowFrame(MenuWindow2, 5, "Give item to crew member");
         Set_Window(MoveForm, MenuWindow2);
         Set_Sub_Window
           (MoveForm,
            Derived_Window(MenuWindow2, FormHeight, FormLength, 1, 1));
         Post(MoveForm);
      end if;
      Refresh_Without_Update;
      Refresh_Without_Update(MenuWindow2);
      Update_Screen;
   end ShowCargoForm;

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
            elsif Option = "Give item to crew member" then
               ShowCargoForm;
               return CargoMove_Form;
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

   function CargoMoveFormKeys(Key: Key_Code) return GameStates is
      Result: Forms.Driver_Result;
      FieldIndex: Positive := Get_Index(Current(MoveForm));
      Visibility: Cursor_Visibility := Invisible;
   begin
      case Key is
         when KEY_UP => -- Select previous field
            Result := Driver(MoveForm, F_Previous_Field);
            FieldIndex := Get_Index(Current(MoveForm));
            if FieldIndex = 2 then
               Result := Driver(MoveForm, F_End_Line);
            end if;
         when KEY_DOWN => -- Select next field
            Result := Driver(MoveForm, F_Next_Field);
            FieldIndex := Get_Index(Current(MoveForm));
            if FieldIndex = 2 then
               Result := Driver(MoveForm, F_End_Line);
            end if;
         when 10 => -- quit/move item/change member
            if FieldIndex = 2 then
               Result := Driver(MoveForm, F_Next_Field);
               if Result = Form_Ok then
                  if Get_Buffer(Fields(MoveForm, 2)) =
                    "                    " then
                     FieldIndex := 5;
                  else
                     FieldIndex := 4;
                  end if;
                  Set_Current(MoveForm, Fields(MoveForm, FieldIndex));
               end if;
            elsif FieldIndex = 4 then
               Result := Driver(MoveForm, F_Next_Choice);
            else
               return MoveItemResult;
            end if;
         when Key_Backspace | 127 => -- delete last character
            if FieldIndex = 2 then
               Result := Driver(MoveForm, F_Delete_Previous);
               if Result = Form_Ok then
                  FieldIndex := Get_Index(Current(MoveForm));
                  if FieldIndex /= 2 then
                     FieldIndex := 2;
                     Set_Current(MoveForm, Fields(MoveForm, 2));
                  end if;
               end if;
            end if;
         when KEY_DC => -- delete character at cursor
            if FieldIndex = 2 then
               Result := Driver(MoveForm, F_Delete_Char);
            end if;
         when KEY_RIGHT => -- Move cursor right
            if FieldIndex = 2 then
               Result := Driver(MoveForm, F_Right_Char);
            elsif FieldIndex = 4 then
               Result := Driver(MoveForm, F_Next_Choice);
            end if;
         when KEY_LEFT => -- Move cursor left
            if FieldIndex = 2 then
               Result := Driver(MoveForm, F_Left_Char);
            elsif FieldIndex = 4 then
               Result := Driver(MoveForm, F_Previous_Choice);
            end if;
         when 27 => -- Escape select cancel button, second time closes form
            if FieldIndex /= 5 then
               FieldIndex := 5;
               Set_Current(MoveForm, Fields(MoveForm, 5));
               Result := Form_Ok;
            else
               Post(MoveForm, False);
               Delete(MoveForm);
               DrawGame(Cargo_Info);
               return Cargo_Info;
            end if;
         when others =>
            Result := Driver(MoveForm, Key);
      end case;
      if Result = Form_Ok then
         for I in 2 .. 6 loop
            Set_Foreground(Fields(MoveForm, I));
            Set_Background(Fields(MoveForm, I));
         end loop;
         Set_Foreground(Current(MoveForm), BoldCharacters, 11);
         Set_Background(Current(MoveForm), BoldCharacters, 11);
         if FieldIndex = 2 then
            Visibility := Normal;
         end if;
         Set_Cursor_Visibility(Visibility);
         Refresh(MenuWindow2);
      end if;
      return CargoMove_Form;
   exception
      when Eti_Invalid_Field =>
         return CargoMove_Form;
   end CargoMoveFormKeys;

end Ships.UI.Cargo;
