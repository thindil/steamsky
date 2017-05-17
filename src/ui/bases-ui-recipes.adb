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

with Maps; use Maps;
with Items; use Items;
with UserInterface; use UserInterface;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Crafts; use Crafts;

package body Bases.UI.Recipes is

   procedure ShowRecipeInfo is
      RecipeIndex: Positive;
      InfoWindow: Window;
      BaseType: constant Positive :=
        Bases_Types'
          Pos
            (SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex)
               .BaseType) +
        1;
      Cost: Positive;
   begin
      for I in Recipes_List.Iterate loop
         if To_String(Items_List(Recipes_List(I).ResultIndex).Name) =
           Name(Current(TradeMenu)) then
            RecipeIndex := Recipes_Container.To_Index(I);
            exit;
         end if;
      end loop;
      InfoWindow := Create(5, (Columns / 2), 3, (Columns / 2));
      if Items_List(Recipes_List(RecipeIndex).ResultIndex).Prices(BaseType) >
        0 then
         Cost :=
           Items_List(Recipes_List(RecipeIndex).ResultIndex).Prices(BaseType) *
           Recipes_List(RecipeIndex).Difficulty *
           100;
      else
         Cost := Recipes_List(RecipeIndex).Difficulty * 100;
      end if;
      Add
        (Win => InfoWindow,
         Str =>
           "Base price:" & Positive'Image(Cost) & " " & To_String(MoneyName));
      Move_Cursor(Win => InfoWindow, Line => 2, Column => 0);
      Add(Win => InfoWindow, Str => "ENTER to buy selected recipe.");
      Change_Attributes
        (Win => InfoWindow,
         Line => 2,
         Column => 0,
         Count => 5,
         Color => 1);
      Refresh;
      Refresh(InfoWindow);
      Delete(InfoWindow);
   end ShowRecipeInfo;

   procedure ShowTradeRecipes is
      Trade_Items: Item_Array_Access;
      BaseType: constant Positive :=
        Bases_Types'
          Pos
            (SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex)
               .BaseType) +
        1;
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      MoneyIndex2, MenuAmount: Natural := 0;
      MenuIndex: Positive := 1;
   begin
      for I in Recipes_List.Iterate loop
         if Recipes_List(I).BaseType = BaseType and
           Known_Recipes.Find_Index(Item => Recipes_Container.To_Index(I)) =
             Positive_Container.No_Index then
            MenuAmount := MenuAmount + 1;
         end if;
      end loop;
      if MenuAmount = 0 then
         Move_Cursor(Line => (Lines / 3), Column => (Columns / 3));
         Add(Str => "You bought all available crafting recipes in this base.");
         Refresh;
         return;
      end if;
      Trade_Items := new Item_Array(1 .. (MenuAmount + 1));
      for I in Recipes_List.Iterate loop
         if Recipes_List(I).BaseType = BaseType and
           Known_Recipes.Find_Index(Item => Recipes_Container.To_Index(I)) =
             Positive_Container.No_Index then
            Trade_Items.all(MenuIndex) :=
              New_Item
                (To_String(Items_List(Recipes_List(I).ResultIndex).Name));
            MenuIndex := MenuIndex + 1;
         end if;
      end loop;
      Trade_Items.all(Trade_Items'Last) := Null_Item;
      TradeMenu := New_Menu(Trade_Items);
      Set_Format(TradeMenu, Lines - 10, 1);
      Set_Mark(TradeMenu, "");
      Scale(TradeMenu, MenuHeight, MenuLength);
      MenuWindow := Create(MenuHeight, MenuLength, 3, 2);
      Set_Window(TradeMenu, MenuWindow);
      Set_Sub_Window
        (TradeMenu,
         Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
      Post(TradeMenu);
      MoneyIndex2 := FindCargo(FindProtoItem(MoneyIndex));
      Move_Cursor(Line => (MenuHeight + 4), Column => 2);
      if MoneyIndex2 > 0 then
         Add
           (Str =>
              "You have" &
              Natural'Image(PlayerShip.Cargo(MoneyIndex2).Amount) &
              " " &
              To_String(MoneyName) &
              ".");
      else
         Add
           (Str =>
              "You don't have any " &
              To_String(MoneyName) &
              " to buy anything.");
      end if;
      ShowRecipeInfo;
      Refresh(MenuWindow);
   end ShowTradeRecipes;

   function TradeRecipesKeys(Key: Key_Code) return GameStates is
      Result: Menus.Driver_Result;
      RecipeIndex: Positive;
   begin
      case Key is
         when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
            CurrentMenuIndex := 1;
            DrawGame(Sky_Map_View);
            return Sky_Map_View;
         when 56 | KEY_UP => -- Select previous recipe to buy
            Result := Driver(TradeMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(TradeMenu, M_Last_Item);
            end if;
         when 50 | KEY_DOWN => -- Select next recipe to buy
            Result := Driver(TradeMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(TradeMenu, M_First_Item);
            end if;
         when 10 => -- Buy recipe
            for I in Recipes_List.Iterate loop
               if To_String(Items_List(Recipes_List(I).ResultIndex).Name) =
                 Name(Current(TradeMenu)) then
                  RecipeIndex := Recipes_Container.To_Index(I);
                  exit;
               end if;
            end loop;
            BuyRecipe(RecipeIndex);
            DrawGame(TradeRecipes_View);
         when others =>
            Result := Driver(TradeMenu, Key);
            if Result /= Menu_Ok then
               Result := Driver(TradeMenu, M_Clear_Pattern);
               Result := Driver(TradeMenu, Key);
            end if;
      end case;
      if Result = Menu_Ok then
         ShowRecipeInfo;
         Refresh(MenuWindow);
      end if;
      return TradeRecipes_View;
   end TradeRecipesKeys;

end Bases.UI.Recipes;
