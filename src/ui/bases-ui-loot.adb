--    Copyright 2017 Bartek thindil Jasicki
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
with Ships.Cargo; use Ships.Cargo;
with Ships.UI.Cargo; use Ships.UI.Cargo;
with Bases; use Bases;
with Utils.UI; use Utils.UI;
with Messages; use Messages;

package body Bases.UI.Loot is

   Take: Boolean;
   LootForm: Form;
   FormWindow: Window;

   procedure ShowItemInfo is
      InfoWindow, ClearWindow, BoxWindow: Window;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      CurrentLine: Line_Position := 3;
      StartColumn, WindowWidth: Column_Position;
      WindowHeight: Line_Position := 6;
      CargoIndex, BaseItemIndex: Natural := 0;
      ItemIndex: Positive;
      FreeSpace: Integer;
   begin
      ClearWindow := Create(Lines - 3, (Columns / 2), 3, (Columns / 2));
      Refresh_Without_Update(ClearWindow);
      Delete(ClearWindow);
      if Integer'Value(Description(Current(TradeMenu))) > 0 then
         CargoIndex := Integer'Value(Description(Current(TradeMenu)));
         ItemIndex := PlayerShip.Cargo(CargoIndex).ProtoIndex;
         BaseItemIndex :=
           FindBaseCargo(ItemIndex, PlayerShip.Cargo(CargoIndex).Durability);
      else
         BaseItemIndex :=
           Integer'Value(Description(Current(TradeMenu))) * (-1);
         ItemIndex := SkyBases(BaseIndex).Cargo(BaseItemIndex).ProtoIndex;
         CargoIndex :=
           FindCargo
             (ProtoIndex => ItemIndex,
              Durability =>
                SkyBases(BaseIndex).Cargo(BaseItemIndex).Durability);
      end if;
      if CargoIndex > 0 then
         WindowHeight := WindowHeight + 1;
         if PlayerShip.Cargo(CargoIndex).Durability < 100 then
            WindowHeight := WindowHeight + 1;
         end if;
      end if;
      if BaseItemIndex > 0 then
         if SkyBases(BaseIndex).Cargo(BaseItemIndex).Amount > 0 then
            WindowHeight := WindowHeight + 1;
         end if;
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
      Add
        (Win => InfoWindow,
         Str =>
           "Weight:" & Integer'Image(Items_List(ItemIndex).Weight) & " kg");
      if CargoIndex > 0 then
         Move_Cursor(Win => InfoWindow, Line => 2, Column => 0);
         Add
           (Win => InfoWindow,
            Str =>
              "Owned:" & Integer'Image(PlayerShip.Cargo(CargoIndex).Amount));
         if PlayerShip.Cargo(CargoIndex).Durability < 100 then
            Move_Cursor(Win => InfoWindow, Line => 3, Column => 0);
            Add(Win => InfoWindow, Str => "Status: ");
            ShowCargoStatus(CargoIndex, InfoWindow, 3);
            CurrentLine := 4;
         end if;
         CurrentLine := CurrentLine + 1;
      end if;
      if BaseItemIndex > 0 then
         if SkyBases(BaseIndex).Cargo(BaseItemIndex).Amount > 0 then
            Move_Cursor
              (Win => InfoWindow,
               Line => CurrentLine - 1,
               Column => 0);
            Add
              (Win => InfoWindow,
               Str =>
                 "In base:" &
                 Integer'Image
                   (SkyBases(BaseIndex).Cargo(BaseItemIndex).Amount));
            CurrentLine := CurrentLine + 1;
         end if;
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
      Move_Cursor(Line => CurrentLine, Column => (Columns / 2));
      if CargoIndex > 0 then
         if BaseItemIndex > 0 then
            if SkyBases(BaseIndex).Cargo(BaseItemIndex).Amount > 0 then
               Add(Str => "Press ENTER to take, SPACE for drop.");
               Change_Attributes
                 (Line => CurrentLine,
                  Column => (Columns / 2) + 6,
                  Count => 5,
                  Color => 1);
               Change_Attributes
                 (Line => CurrentLine,
                  Column => (Columns / 2) + 21,
                  Count => 5,
                  Color => 1);
            else
               Add(Str => "Press SPACE for drop.");
               Change_Attributes
                 (Line => CurrentLine,
                  Column => (Columns / 2) + 6,
                  Count => 5,
                  Color => 1);
            end if;
         else
            Add(Str => "Press SPACE for drop.");
            Change_Attributes
              (Line => CurrentLine,
               Column => (Columns / 2) + 6,
               Count => 5,
               Color => 1);
         end if;
      elsif BaseItemIndex > 0 then
         if SkyBases(BaseIndex).Cargo(BaseItemIndex).Amount > 0 then
            Add(Str => "Press ENTER to take.");
            Change_Attributes
              (Line => CurrentLine,
               Column => (Columns / 2) + 6,
               Count => 5,
               Color => 1);
         end if;
      end if;
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

   procedure ShowLoot is
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Loot_Items: Item_Array_Access;
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      MenuIndex: Integer := 1;
      ItemsAmount: Positive := 1;
   begin
      ItemsAmount := ItemsAmount + Positive(PlayerShip.Cargo.Length);
      for Item of SkyBases(BaseIndex).Cargo loop
         if FindCargo(ProtoIndex => Item.ProtoIndex) = 0 then
            ItemsAmount := ItemsAmount + 1;
         end if;
      end loop;
      Loot_Items := new Item_Array(1 .. ItemsAmount);
      for I in PlayerShip.Cargo.Iterate loop
         Loot_Items.all(MenuIndex) :=
           New_Item
             (To_String(Items_List(PlayerShip.Cargo(I).ProtoIndex).Name),
              Positive'Image(Cargo_Container.To_Index(I)));
         MenuIndex := MenuIndex + 1;
      end loop;
      for I in SkyBases(BaseIndex).Cargo.Iterate loop
         if FindCargo(ProtoIndex => SkyBases(BaseIndex).Cargo(I).ProtoIndex) =
           0 then
            Loot_Items.all(MenuIndex) :=
              New_Item
                (To_String
                   (Items_List(SkyBases(BaseIndex).Cargo(I).ProtoIndex).Name),
                 Integer'Image(BaseCargo_Container.To_Index(I) * (-1)));
            MenuIndex := MenuIndex + 1;
         end if;
      end loop;
      Loot_Items.all(MenuIndex) := Null_Item;
      TradeMenu := New_Menu(Loot_Items);
      Set_Options(TradeMenu, (Show_Descriptions => False, others => True));
      Set_Format(TradeMenu, Lines - 3, 1);
      Scale(TradeMenu, MenuHeight, MenuLength);
      MenuWindow := Create(MenuHeight, MenuLength, 3, 2);
      Set_Window(TradeMenu, MenuWindow);
      Set_Sub_Window
        (TradeMenu,
         Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
      Post(TradeMenu);
      if Loot_Items.all(CurrentMenuIndex) = Null_Item then
         CurrentMenuIndex := 1;
      end if;
      Set_Current(TradeMenu, Loot_Items.all(CurrentMenuIndex));
      ShowItemInfo;
   end ShowLoot;

   function ShowLootForm return GameStates is
      Loot_Fields: constant Field_Array_Access := new Field_Array(1 .. 6);
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      FieldOptions: Field_Option_Set;
      FormHeight: Line_Position;
      FormLength: Column_Position;
      Visibility: Cursor_Visibility := Normal;
      ItemIndex: Positive;
      MaxAmount: Natural := 0;
      FieldText: Unbounded_String := To_Unbounded_String("Enter amount");
      CaptionText: Unbounded_String;
   begin
      for I in Items_List.Iterate loop
         if To_String(Items_List(I).Name) = Name(Current(TradeMenu)) then
            ItemIndex := Objects_Container.To_Index(I);
            exit;
         end if;
      end loop;
      if Take then
         for Item of SkyBases(BaseIndex).Cargo loop
            if Item.ProtoIndex = ItemIndex then
               MaxAmount := Item.Amount;
               exit;
            end if;
         end loop;
         if MaxAmount = 0 then
            ShowDialog
              ("You can't take any " &
               To_String(Items_List(ItemIndex).Name) &
               " from base.");
            DrawGame(Loot_View);
            return Loot_View;
         end if;
         Append(FieldText, " to take");
         CaptionText := To_Unbounded_String("Taking ");
         Append(CaptionText, Items_List(ItemIndex).Name);
      else
         if Description(Current(TradeMenu)) = "0" then
            ShowDialog
              ("You don't have any " &
               To_String(Items_List(ItemIndex).Name) &
               " to drop.");
            DrawGame(Loot_View);
            return Loot_View;
         end if;
         MaxAmount :=
           PlayerShip.Cargo(Integer'Value(Description(Current(TradeMenu))))
             .Amount;
         Append(FieldText, " to drop");
         CaptionText := To_Unbounded_String("Dropping ");
         Append(CaptionText, Items_List(ItemIndex).Name);
      end if;
      Append(FieldText, " (max" & Natural'Image(MaxAmount) & "): ");
      if LootForm = Null_Form then
         Set_Cursor_Visibility(Visibility);
         Loot_Fields.all(1) :=
           New_Field(1, Column_Position(Length(FieldText)), 0, 0, 0, 0);
         FieldOptions := Get_Options(Loot_Fields.all(1));
         Set_Buffer(Loot_Fields.all(1), 0, To_String(FieldText));
         FieldOptions.Active := False;
         Set_Options(Loot_Fields.all(1), FieldOptions);
         Loot_Fields.all(2) :=
           New_Field(1, 20, 0, Column_Position(Length(FieldText)), 0, 0);
         FieldOptions := Get_Options(Loot_Fields.all(2));
         FieldOptions.Auto_Skip := False;
         Set_Options(Loot_Fields.all(2), FieldOptions);
         Set_Foreground
           (Loot_Fields.all(2),
            (Bold_Character => True, others => False),
            1);
         Set_Background
           (Loot_Fields.all(2),
            (Under_Line => True, others => False));
         Terminal_Interface.Curses.Forms.Field_Types.IntField.Set_Field_Type
           (Loot_Fields.all(2),
            (0, 0, MaxAmount));
         Loot_Fields.all(3) :=
           New_Field(1, 8, 2, (Column_Position(Length(FieldText)) / 2), 0, 0);
         Set_Buffer(Loot_Fields.all(3), 0, "[Cancel]");
         FieldOptions := Get_Options(Loot_Fields.all(3));
         FieldOptions.Edit := False;
         Set_Options(Loot_Fields.all(3), FieldOptions);
         Loot_Fields.all(4) :=
           New_Field
             (1,
              4,
              2,
              (Column_Position(Length(FieldText)) / 2) + 10,
              0,
              0);
         FieldOptions := Get_Options(Loot_Fields.all(4));
         FieldOptions.Edit := False;
         Set_Options(Loot_Fields.all(4), FieldOptions);
         Set_Buffer(Loot_Fields.all(4), 0, "[Ok]");
         Loot_Fields.all(5) :=
           New_Field
             (1,
              11,
              2,
              (Column_Position(Length(FieldText)) / 2) + 16,
              0,
              0);
         FieldOptions := Get_Options(Loot_Fields.all(5));
         FieldOptions.Edit := False;
         Set_Options(Loot_Fields.all(5), FieldOptions);
         if Take then
            Set_Buffer(Loot_Fields.all(5), 0, "[Take all]");
         else
            Set_Buffer(Loot_Fields.all(5), 0, "[Drop all]");
         end if;
         Loot_Fields.all(6) := Null_Field;
         LootForm := New_Form(Loot_Fields);
         Scale(LootForm, FormHeight, FormLength);
         FormWindow :=
           Create
             (FormHeight + 2,
              FormLength + 2,
              ((Lines / 3) - (FormHeight / 2)),
              ((Columns / 2) - (FormLength / 2)));
         Box(FormWindow);
         WindowFrame(FormWindow, 5, To_String(CaptionText));
         Set_Window(LootForm, FormWindow);
         Set_Sub_Window
           (LootForm,
            Derived_Window(FormWindow, FormHeight, FormLength, 1, 1));
         Post(LootForm);
      end if;
      Refresh_Without_Update;
      Refresh_Without_Update(FormWindow);
      Update_Screen;
      return Loot_Form;
   end ShowLootForm;

   function LootResult return GameStates is
      ItemIndex, Amount: Positive;
      Visibility: Cursor_Visibility := Invisible;
      FieldIndex: constant Positive := Get_Index(Current(LootForm));
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseItemIndex, CargoIndex: Natural := 0;
   begin
      if FieldIndex < 3 then
         return Loot_Form;
      elsif FieldIndex > 3 then
         for I in Items_List.Iterate loop
            if To_String(Items_List(I).Name) = Name(Current(TradeMenu)) then
               ItemIndex := Objects_Container.To_Index(I);
               exit;
            end if;
         end loop;
         if Integer'Value(Description(Current(TradeMenu))) > 0 then
            CargoIndex := Integer'Value(Description(Current(TradeMenu)));
            BaseItemIndex :=
              FindBaseCargo
                (ItemIndex,
                 PlayerShip.Cargo(CargoIndex).Durability);
         else
            BaseItemIndex :=
              Integer'Value(Description(Current(TradeMenu))) * (-1);
            CargoIndex :=
              FindCargo
                (ProtoIndex => ItemIndex,
                 Durability =>
                   SkyBases(BaseIndex).Cargo(BaseItemIndex).Durability);
         end if;
         if FieldIndex = 4 then
            Amount := Positive'Value(Get_Buffer(Fields(LootForm, 2)));
         else
            if Take then
               Amount := SkyBases(BaseIndex).Cargo(BaseItemIndex).Amount;
            else
               Amount := PlayerShip.Cargo.Element(CargoIndex).Amount;
            end if;
         end if;
         if Take then
            if FreeCargo(0 - (Amount * Items_List(ItemIndex).Weight)) < 0 then
               ShowDialog
                 ("You can't take that much " &
                  To_String(Items_List(ItemIndex).Name) &
                  ".");
               Set_Cursor_Visibility(Visibility);
               Post(LootForm, False);
               Delete(LootForm);
               DrawGame(Loot_View);
               return Loot_View;
            end if;
            UpdateCargo
              (Ship => PlayerShip,
               CargoIndex => CargoIndex,
               Amount => Amount,
               Durability =>
                 SkyBases(BaseIndex).Cargo(BaseItemIndex).Durability);
            UpdateBaseCargo
              (CargoIndex => BaseItemIndex,
               Amount => (0 - Amount),
               Durability =>
                 SkyBases(BaseIndex).Cargo.Element(BaseItemIndex).Durability);
            AddMessage
              ("You took" &
               Positive'Image(Amount) &
               " " &
               To_String(Items_List(ItemIndex).Name) &
               ".",
               OrderMessage);
         else
            UpdateBaseCargo
              (CargoIndex => BaseItemIndex,
               Amount => Amount,
               Durability => PlayerShip.Cargo.Element(CargoIndex).Durability);
            UpdateCargo
              (Ship => PlayerShip,
               CargoIndex => ItemIndex,
               Amount => (0 - Amount),
               Durability => PlayerShip.Cargo.Element(CargoIndex).Durability);
            AddMessage
              ("You drop" &
               Positive'Image(Amount) &
               " " &
               To_String(Items_List(ItemIndex).Name) &
               ".",
               OrderMessage);
         end if;
         UpdateGame(10);
      end if;
      Set_Cursor_Visibility(Visibility);
      Post(LootForm, False);
      Delete(LootForm);
      DrawGame(Loot_View);
      return Loot_View;
   end LootResult;

   function LootKeys(Key: Key_Code) return GameStates is
      Result: Menus.Driver_Result;
   begin
      case Key is
         when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
            CurrentMenuIndex := 1;
            DrawGame(Sky_Map_View);
            return Sky_Map_View;
         when 56 | KEY_UP => -- Select previous item to loot
            Result := Driver(TradeMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(TradeMenu, M_Last_Item);
            end if;
         when 50 | KEY_DOWN => -- Select next item to loot
            Result := Driver(TradeMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(TradeMenu, M_First_Item);
            end if;
         when 32 => -- Drop item
            Take := False;
            return ShowLootForm;
         when 10 => -- Take item
            Take := True;
            return ShowLootForm;
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
      return Loot_View;
   end LootKeys;

   function LootFormKeys(Key: Key_Code) return GameStates is
      Result: Forms.Driver_Result;
      FieldIndex: Positive := Get_Index(Current(LootForm));
      Visibility: Cursor_Visibility := Invisible;
      MaxIndex: Positive;
   begin
      case Key is
         when KEY_UP => -- Select previous field
            Result := Driver(LootForm, F_Previous_Field);
            FieldIndex := Get_Index(Current(LootForm));
            if FieldIndex = 2 then
               Result := Driver(LootForm, F_End_Line);
            end if;
         when KEY_DOWN => -- Select next field
            Result := Driver(LootForm, F_Next_Field);
            FieldIndex := Get_Index(Current(LootForm));
            if FieldIndex = 2 then
               Result := Driver(LootForm, F_End_Line);
            end if;
         when 10 => -- quit/buy/sell
            if FieldIndex = 2 then
               Result := Driver(LootForm, F_Next_Field);
               if Result = Form_Ok then
                  if Get_Buffer(Fields(LootForm, 2)) =
                    "                    " then
                     FieldIndex := 5;
                  else
                     FieldIndex := 4;
                  end if;
                  Set_Current(LootForm, Fields(LootForm, FieldIndex));
               end if;
            else
               return LootResult;
            end if;
         when Key_Backspace => -- delete last character
            if FieldIndex = 2 then
               Result := Driver(LootForm, F_Delete_Previous);
               if Result = Form_Ok then
                  FieldIndex := Get_Index(Current(LootForm));
                  if FieldIndex /= 2 then
                     FieldIndex := 2;
                     Set_Current(LootForm, Fields(LootForm, 2));
                  end if;
               end if;
            end if;
         when KEY_DC => -- delete character at cursor
            if FieldIndex = 2 then
               Result := Driver(LootForm, F_Delete_Char);
            end if;
         when KEY_RIGHT => -- Move cursor right
            if FieldIndex = 2 then
               Result := Driver(LootForm, F_Right_Char);
            end if;
         when KEY_LEFT => -- Move cursor left
            if FieldIndex = 2 then
               Result := Driver(LootForm, F_Left_Char);
            end if;
         when 27 => -- Escape select cancel button
            FieldIndex := 3;
            Set_Current(LootForm, Fields(LootForm, 3));
            Result := Form_Ok;
         when others =>
            Result := Driver(LootForm, Key);
      end case;
      if Result = Form_Ok then
         MaxIndex := 5;
         if FieldIndex = 2 then
            Set_Background
              (Current(LootForm),
               (Under_Line => True, others => False));
            Visibility := Normal;
            for I in 3 .. MaxIndex loop
               Set_Foreground(Fields(LootForm, I));
            end loop;
         else
            Set_Background(Fields(LootForm, 2));
            for I in 2 .. MaxIndex loop
               if I /= FieldIndex then
                  Set_Foreground(Fields(LootForm, I));
               end if;
            end loop;
         end if;
         Set_Foreground
           (Current(LootForm),
            (Bold_Character => True, others => False),
            1);
         Set_Cursor_Visibility(Visibility);
         Refresh(FormWindow);
      end if;
      return Loot_Form;
   end LootFormKeys;

end Bases.UI.Loot;
