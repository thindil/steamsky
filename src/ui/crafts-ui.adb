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
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with UserInterface; use UserInterface;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Items; use Items;
with Help.UI; use Help.UI;
with Header; use Header;
with Utils.UI; use Utils.UI;
with Trades; use Trades;

package body Crafts.UI is

   RecipesMenu, ModulesMenu: Menu;
   MenuWindow, MenuWindow2: Window;
   RecipeIndex: Integer := 1;

   procedure ShowRecipeInfo is
      InfoWindow, ClearWindow, BoxWindow: Window;
      Recipe: Craft_Data;
      CurrentLine: Line_Position := 1;
      MAmount, TextLength, CargoIndex: Natural := 0;
      HaveWorkplace, IsMaterial, IsTool: Boolean := False;
      StartLine: Line_Position;
      StartColumn, EndColumn: Column_Position;
      WorkplaceName: Unbounded_String := Null_Unbounded_String;
      WindowHeight: Line_Position := 7;
      WindowWidth: Column_Position := 1;
   begin
      ClearWindow := Create((Lines - 5), (Columns / 2), 3, (Columns / 2));
      Refresh_Without_Update(ClearWindow);
      Delete(ClearWindow);
      if RecipeIndex > 0 then
         Recipe := Recipes_List(RecipeIndex);
         WindowHeight := WindowHeight + 1;
      else
         Recipe.MaterialTypes.Append
         (New_Item => Items_List(abs (RecipeIndex)).IType);
         Recipe.MaterialAmounts.Append(New_Item => 1);
         Recipe.ResultIndex := abs (RecipeIndex);
         Recipe.ResultAmount := 0;
         Recipe.Workplace := ALCHEMY_LAB;
         for ProtoRecipe of Recipes_List loop
            if ProtoRecipe.ResultIndex = Recipe.ResultIndex then
               Recipe.Skill := ProtoRecipe.Skill;
               Recipe.Time := ProtoRecipe.Difficulty * 15;
               exit;
            end if;
         end loop;
         Recipe.Difficulty := 1;
         Recipe.BaseType := 0;
         Recipe.Tool := AlchemyTools;
      end if;
      for I in
        Recipe.MaterialTypes.First_Index ..
            Recipe.MaterialTypes.Last_Index loop
         TextLength := 2;
         for J in Items_List.Iterate loop
            if Items_List(J).IType = Recipe.MaterialTypes(I) then
               if TextLength > 2 then
                  TextLength := TextLength + 3;
               end if;
               TextLength :=
                 TextLength +
                 Integer'Image(Recipe.MaterialAmounts(I))'Length +
                 1 +
                 Length(Items_List(J).Name);
               CargoIndex :=
                 FindCargo(ProtoIndex => Objects_Container.To_Index(J));
               if CargoIndex > 0 then
                  TextLength :=
                    TextLength +
                    Integer'Image(PlayerShip.Cargo(CargoIndex).Amount)'Length +
                    1;
               end if;
            end if;
         end loop;
         if TextLength > (Natural(Columns / 2) - 3) then
            WindowHeight :=
              WindowHeight +
              Line_Position(TextLength / (Natural(Columns / 2) - 3));
            WindowWidth := Columns / 2;
         elsif (TextLength + 3) > Positive(WindowWidth) then
            WindowWidth := Column_Position(TextLength) + 3;
         end if;
         TextLength := 0;
      end loop;
      if Recipe.Tool /= To_Unbounded_String("None") then
         TextLength := 6;
         for Item of Items_List loop
            if Item.IType = Recipe.Tool then
               if TextLength > 6 then
                  TextLength := TextLength + 4;
               end if;
               TextLength := TextLength + Length(Item.Name);
            end if;
         end loop;
         if TextLength > (Natural(Columns / 2) - 3) then
            WindowHeight :=
              WindowHeight +
              Line_Position(TextLength / (Natural(Columns / 2) - 3));
            WindowWidth := Columns / 2;
         elsif (TextLength + 3) > Positive(WindowWidth) then
            WindowWidth := Column_Position(TextLength) + 3;
         end if;
      end if;
      WindowHeight :=
        WindowHeight + Line_Position(Recipe.MaterialTypes.Length);
      BoxWindow := Create(WindowHeight, (Columns / 2), 3, (Columns / 2));
      InfoWindow :=
        Create(WindowHeight - 2, (Columns / 2) - 2, 4, (Columns / 2) + 1);
      if RecipeIndex > 0 then
         Add
           (Win => InfoWindow,
            Str => "Amount:" & Integer'Image(Recipe.ResultAmount));
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
         CurrentLine := CurrentLine + 1;
      end if;
      Add(Win => InfoWindow, Str => "Materials needed: ");
      for I in
        Recipe.MaterialTypes.First_Index ..
            Recipe.MaterialTypes.Last_Index loop
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 2);
         Add(Win => InfoWindow, Str => "-");
         MAmount := 0;
         for J in Items_List.Iterate loop
            IsMaterial := False;
            if RecipeIndex > 0 then
               if Items_List(J).IType = Recipe.MaterialTypes(I) then
                  IsMaterial := True;
               end if;
            else
               if Items_List(J).Name = Items_List(Recipe.ResultIndex).Name then
                  IsMaterial := True;
               end if;
            end if;
            if IsMaterial then
               if MAmount > 0 then
                  Add(Win => InfoWindow, Str => " or");
               end if;
               Get_Cursor_Position
                 (Win => InfoWindow,
                  Line => StartLine,
                  Column => StartColumn);
               Add
                 (Win => InfoWindow,
                  Str =>
                    Integer'Image(Recipe.MaterialAmounts(I)) &
                    "x" &
                    To_String(Items_List(J).Name));
               Get_Cursor_Position
                 (Win => InfoWindow,
                  Line => CurrentLine,
                  Column => EndColumn);
               CargoIndex :=
                 FindCargo(ProtoIndex => Objects_Container.To_Index(J));
               if CargoIndex > 0 then
                  TextLength :=
                    Positive'Image(PlayerShip.Cargo(CargoIndex).Amount)'Length;
                  Add
                    (Win => InfoWindow,
                     Str =>
                       "(" &
                       Positive'Image(PlayerShip.Cargo(CargoIndex).Amount)
                         (2 .. TextLength) &
                       ")");
                  Get_Cursor_Position
                    (Win => InfoWindow,
                     Line => CurrentLine,
                     Column => EndColumn);
               else
                  if StartLine = CurrentLine then
                     TextLength := Natural(EndColumn - StartColumn);
                     Change_Attributes
                       (Win => InfoWindow,
                        Line => StartLine,
                        Column => StartColumn,
                        Count => Integer(StartColumn) + TextLength,
                        Color => 3);
                  else
                     TextLength := Natural((Columns / 2) - StartColumn);
                     Change_Attributes
                       (Win => InfoWindow,
                        Line => StartLine,
                        Column => StartColumn,
                        Count => Integer(StartColumn) + TextLength,
                        Color => 3);
                     Change_Attributes
                       (Win => InfoWindow,
                        Line => CurrentLine,
                        Column => 0,
                        Count => Integer(EndColumn),
                        Color => 3);
                  end if;
                  Move_Cursor
                    (Win => InfoWindow,
                     Line => CurrentLine,
                     Column => EndColumn);
               end if;
               MAmount := MAmount + 1;
            end if;
         end loop;
         CurrentLine := CurrentLine + 1;
      end loop;
      if Recipe.Tool /= To_Unbounded_String("None") then
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
         Add(Win => InfoWindow, Str => "Tool: ");
         MAmount := 0;
         for I in Items_List.Iterate loop
            IsTool := False;
            if Items_List(I).IType = Recipe.Tool then
               IsTool := True;
            end if;
            if IsTool then
               if MAmount > 0 then
                  Add(Win => InfoWindow, Str => " or ");
               end if;
               Get_Cursor_Position
                 (Win => InfoWindow,
                  Line => StartLine,
                  Column => StartColumn);
               Add(Win => InfoWindow, Str => To_String(Items_List(I).Name));
               Get_Cursor_Position
                 (Win => InfoWindow,
                  Line => CurrentLine,
                  Column => EndColumn);
               if FindCargo(ProtoIndex => Objects_Container.To_Index(I)) =
                 0 then
                  if StartLine = CurrentLine then
                     TextLength := Natural(EndColumn - StartColumn);
                     Change_Attributes
                       (Win => InfoWindow,
                        Line => StartLine,
                        Column => StartColumn,
                        Count => Integer(StartColumn) + TextLength,
                        Color => 3);
                  else
                     TextLength := Natural((Columns / 2) - StartColumn);
                     Change_Attributes
                       (Win => InfoWindow,
                        Line => StartLine,
                        Column => StartColumn,
                        Count => Integer(StartColumn) + TextLength,
                        Color => 3);
                     Change_Attributes
                       (Win => InfoWindow,
                        Line => CurrentLine,
                        Column => 0,
                        Count => Integer(EndColumn),
                        Color => 3);
                  end if;
                  Move_Cursor
                    (Win => InfoWindow,
                     Line => CurrentLine,
                     Column => EndColumn);
               end if;
               MAmount := MAmount + 1;
            end if;
         end loop;
         CurrentLine := CurrentLine + 1;
      end if;
      Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
      Add(Win => InfoWindow, Str => "Workplace: ");
      for Module of PlayerShip.Modules loop
         if Modules_List(Module.ProtoIndex).MType = Recipe.Workplace then
            WorkplaceName := Module.Name;
            if Module.Durability > 0 then
               HaveWorkplace := True;
               exit;
            end if;
         end if;
      end loop;
      if WorkplaceName = Null_Unbounded_String then
         for Module of Modules_List loop
            if Module.MType = Recipe.Workplace then
               WorkplaceName := Module.Name;
               exit;
            end if;
         end loop;
      end if;
      if Length(WorkplaceName) + 13 > Positive(WindowWidth) then
         WindowWidth := Column_Position(Length(WorkplaceName)) + 13;
         if WindowWidth > (Columns / 2) then
            WindowWidth := Columns / 2;
         end if;
      end if;
      Add(Win => InfoWindow, Str => To_String(WorkplaceName));
      if not HaveWorkplace then
         Change_Attributes
           (Win => InfoWindow,
            Line => CurrentLine,
            Column => 11,
            Count => Length(WorkplaceName),
            Color => 3);
      end if;
      CurrentLine := CurrentLine + 1;
      Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
      Add
        (Win => InfoWindow,
         Str => "Skill: " & To_String(Skills_Names(Recipe.Skill)));
      CurrentLine := CurrentLine + 1;
      Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
      Add
        (Win => InfoWindow,
         Str => "Time needed:" & Positive'Image(Recipe.Time) & " minutes");
      Resize(BoxWindow, WindowHeight, WindowWidth);
      WindowFrame(BoxWindow, 2, "Recipe info");
      Resize(InfoWindow, WindowHeight - 2, WindowWidth - 2);
      Move_Cursor(Line => (WindowHeight + 3), Column => (Columns / 2));
      Add(Str => "Press ENTER for set manufacturing order");
      Change_Attributes
        (Line => (WindowHeight + 3),
         Column => (Columns / 2) + 6,
         Count => 5,
         Color => 1,
         Attr => BoldCharacters);
      Move_Cursor(Line => (WindowHeight + 4), Column => (Columns / 2));
      Add(Str => "Press ESCAPE to back to sky map");
      Change_Attributes
        (Line => (WindowHeight + 4),
         Column => (Columns / 2) + 6,
         Count => 6,
         Color => 1,
         Attr => BoldCharacters);
      Move_Cursor(Line => (WindowHeight + 5), Column => (Columns / 2));
      Add(Str => "Press F1 for help");
      Change_Attributes
        (Line => (WindowHeight + 5),
         Column => (Columns / 2) + 6,
         Count => 2,
         Color => 1,
         Attr => BoldCharacters);
      Refresh_Without_Update;
      Refresh_Without_Update(BoxWindow);
      Delete(BoxWindow);
      Refresh_Without_Update(InfoWindow);
      Delete(InfoWindow);
      Refresh_Without_Update(MenuWindow);
      Update_Screen;
   end ShowRecipeInfo;

   procedure ShowRecipes is
      Recipes_Items: Item_Array_Access;
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      Deconstructs: Positive_Container.Vector;
   begin
      for Item of PlayerShip.Cargo loop
         for J in Recipes_List.First_Index .. Recipes_List.Last_Index loop
            if Recipes_List(J).ResultIndex = Item.ProtoIndex then
               if Known_Recipes.Find_Index(Item => J) =
                 Positive_Container.No_Index and
                 Deconstructs.Find_Index(Item => Item.ProtoIndex) =
                   Positive_Container.No_Index then
                  Deconstructs.Append(New_Item => Item.ProtoIndex);
                  exit;
               end if;
            end if;
         end loop;
      end loop;
      Recipes_Items :=
        new Item_Array
        (1 ..
             (Integer(Known_Recipes.Length) +
              Integer(Deconstructs.Length) +
              1));
      Move_Cursor(Line => 3, Column => 2);
      for I in Known_Recipes.First_Index .. Known_Recipes.Last_Index loop
         Recipes_Items.all(I) :=
           New_Item
             (To_String
                (Items_List(Recipes_List(Known_Recipes(I)).ResultIndex).Name),
              Positive'Image(Known_Recipes(I)));
      end loop;
      for I in Deconstructs.First_Index .. Deconstructs.Last_Index loop
         Recipes_Items.all(Known_Recipes.Last_Index + I) :=
           New_Item
             ("Deconstruct " & To_String(Items_List(Deconstructs(I)).Name),
              Integer'Image(Deconstructs(I) * (-1)));
      end loop;
      Recipes_Items.all(Recipes_Items'Last) := Null_Item;
      RecipesMenu := New_Menu(Recipes_Items);
      Set_Format(RecipesMenu, Lines - 10, 1);
      Set_Options(RecipesMenu, (Show_Descriptions => False, others => True));
      Scale(RecipesMenu, MenuHeight, MenuLength);
      MenuWindow := Create(MenuHeight, MenuLength, 3, 2);
      Set_Window(RecipesMenu, MenuWindow);
      Set_Sub_Window
        (RecipesMenu,
         Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
      Post(RecipesMenu);
      for I in Known_Recipes.First_Index .. Known_Recipes.Last_Index loop
         if Known_Recipes(I) = RecipeIndex then
            Set_Current(RecipesMenu, Recipes_Items.all(I));
            exit;
         end if;
      end loop;
      ShowRecipeInfo;
   end ShowRecipes;

   function ShowRecipeMenu return GameStates is
      Modules_Items: Item_Array_Access;
      ModulesAmount: Positive := 2;
      MenuIndex: Positive := 1;
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      MType: ModuleType;
   begin
      if RecipeIndex > 0 then
         MType := Recipes_List(RecipeIndex).Workplace;
      else
         MType := ALCHEMY_LAB;
      end if;
      for Module of PlayerShip.Modules loop
         if Modules_List(Module.ProtoIndex).MType = MType then
            ModulesAmount := ModulesAmount + 1;
         end if;
      end loop;
      if ModulesAmount = 2 then
         ShowDialog("You don't have proper workplace for this recipe.");
         DrawGame(Craft_View);
         return Craft_View;
      end if;
      Modules_Items := new Item_Array(1 .. ModulesAmount);
      for I in PlayerShip.Modules.Iterate loop
         if Modules_List(PlayerShip.Modules(I).ProtoIndex).MType = MType then
            Modules_Items.all(MenuIndex) :=
              New_Item
                ("Manufacture in " & To_String(PlayerShip.Modules(I).Name),
                 Positive'Image(Modules_Container.To_Index(I)));
            MenuIndex := MenuIndex + 1;
         end if;
      end loop;
      Modules_Items.all(Modules_Items'Last - 1) := New_Item("Close", "0");
      Modules_Items.all(Modules_Items'Last) := Null_Item;
      ModulesMenu := New_Menu(Modules_Items);
      Set_Options(ModulesMenu, (Show_Descriptions => False, others => True));
      Scale(ModulesMenu, MenuHeight, MenuLength);
      MenuWindow2 :=
        Create
          (MenuHeight + 2,
           MenuLength + 2,
           ((Lines / 3) - (MenuHeight / 2)),
           ((Columns / 2) - (MenuLength / 2)));
      WindowFrame(MenuWindow2, 5, "Set recipe");
      Set_Window(ModulesMenu, MenuWindow2);
      Set_Sub_Window
        (ModulesMenu,
         Derived_Window(MenuWindow2, MenuHeight, MenuLength, 1, 1));
      Post(ModulesMenu);
      Refresh_Without_Update;
      Refresh_Without_Update(MenuWindow2);
      Update_Screen;
      return Recipe_Setting;
   end ShowRecipeMenu;

   function CraftKeys(Key: Key_Code) return GameStates is
      Result: Driver_Result;
   begin
      case Key is
         when 27 => -- Back to sky map
            RecipeIndex := 1;
            DrawGame(Sky_Map_View);
            return Sky_Map_View;
         when 10 => -- Set selected manufacturing order
            return ShowRecipeMenu;
         when 56 | KEY_UP => -- Select previous recipe
            Result := Driver(RecipesMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(RecipesMenu, M_Last_Item);
            end if;
         when 50 | KEY_DOWN => -- Select next recipe
            Result := Driver(RecipesMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(RecipesMenu, M_First_Item);
            end if;
         when Key_F1 => -- Show help
            Erase;
            ShowGameHeader(Help_Topic);
            ShowHelp(Craft_View, 5);
            return Help_Topic;
         when others =>
            Result := Driver(RecipesMenu, Key);
            if Result /= Menu_Ok then
               Result := Driver(RecipesMenu, M_Clear_Pattern);
               Result := Driver(RecipesMenu, Key);
            end if;
      end case;
      if Result = Menu_Ok then
         RecipeIndex := Integer'Value(Description(Current(RecipesMenu)));
         ShowRecipeInfo;
      end if;
      return Craft_View;
   end CraftKeys;

   function RecipeSettingKeys(Key: Key_Code) return GameStates is
      Result: Driver_Result;
      ModuleIndex: constant Natural :=
        Natural'Value(Description(Current(ModulesMenu)));
   begin
      case Key is
         when 10 => -- Set selected manufacturing order
            if ModuleIndex > 0 then
               SetRecipe(RecipeIndex, ModuleIndex);
            end if;
            DrawGame(Craft_View);
            return Craft_View;
         when 56 | KEY_UP => -- Select previous recipe
            Result := Driver(ModulesMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(ModulesMenu, M_Last_Item);
            end if;
         when 50 | KEY_DOWN => -- Select next recipe
            Result := Driver(ModulesMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(ModulesMenu, M_First_Item);
            end if;
         when 27 => -- Esc select close option, used second time, close menu
            if Name(Current(ModulesMenu)) = "Close" then
               DrawGame(Craft_View);
               return Craft_View;
            else
               Result := Driver(ModulesMenu, M_Last_Item);
            end if;
         when others =>
            Result := Driver(ModulesMenu, Key);
            if Result /= Menu_Ok then
               Result := Driver(ModulesMenu, M_Clear_Pattern);
               Result := Driver(ModulesMenu, Key);
            end if;
      end case;
      if Result = Menu_Ok then
         Refresh(MenuWindow2);
      end if;
      return Recipe_Setting;
   exception
      when An_Exception : Crafting_No_Materials =>
         ShowDialog
           ("You don't have enough materials to start manufacturing " &
            Exception_Message(An_Exception) &
            ".");
         DrawGame(Craft_View);
         return Craft_View;
      when An_Exception : Crafting_No_Tools =>
         ShowDialog
           ("You don't have proper tool to start manufacturing " &
            Exception_Message(An_Exception) &
            ".");
         DrawGame(Craft_View);
         return Craft_View;
      when Trade_No_Free_Cargo =>
         ShowDialog("You don't have that much free space in your ship cargo.");
         DrawGame(Craft_View);
         return Craft_View;
   end RecipeSettingKeys;

end Crafts.UI;
