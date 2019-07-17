--    Copyright 2018-2019 Bartek thindil Jasicki
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
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Label; use Gtk.Label;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.Stack; use Gtk.Stack;
with Glib; use Glib;
with Game; use Game;
with Ships; use Ships;
with Items; use Items;
with Utils.UI; use Utils.UI;
with Trades; use Trades;

package body Crafts.UI is

-- ****iv* Crafts.UI/Builder
-- SOURCE
   Builder: Gtkada_Builder;
-- ****
-- ****iv* Crafts.UI/RecipeIndex
-- SOURCE
   RecipeIndex: Unbounded_String;
-- ****

-- ****if* Crafts.UI/ShowSetRecipe
-- SOURCE
   procedure ShowSetRecipe(Object: access Gtkada_Builder_Record'Class) is
-- ****
      MaxAmount: Positive;
      AmountAdj: constant Gtk_Adjustment :=
        Gtk_Adjustment(Get_Object(Object, "amountadj"));
      MType: ModuleType;
      CmbModules: constant Gtk_Combo_Box_Text :=
        Gtk_Combo_Box_Text(Get_Object(Object, "cmbmodules"));
      LabelTimes: constant Gtk_Label :=
        Gtk_Label(Get_Object(Object, "lbltimes"));
   begin
      MaxAmount := CheckRecipe(RecipeIndex);
      Set_Value(AmountAdj, 1.0);
      Set_Upper(AmountAdj, Gdouble(MaxAmount));
      Set_Label(LabelTimes, "(max" & Positive'Image(MaxAmount) & "):");
      if Length(RecipeIndex) > 12
        and then Slice(RecipeIndex, 1, 11) = "Deconstruct" then
         MType := ALCHEMY_LAB;
      else
         MType := Recipes_List(RecipeIndex).Workplace;
      end if;
      Remove_All(CmbModules);
      for Module of PlayerShip.Modules loop
         if Modules_List(Module.ProtoIndex).MType = MType then
            Append_Text(CmbModules, To_String(Module.Name));
         end if;
      end loop;
      if Length(RecipeIndex) > 12
        and then Slice(RecipeIndex, 1, 11) = "Deconstruct" then
         Hide(Gtk_Widget(Get_Object(Object, "spincraftamount")));
         Hide(Gtk_Widget(LabelTimes));
      else
         Show_All(Gtk_Widget(Get_Object(Object, "spincraftamount")));
         Show_All(Gtk_Widget(LabelTimes));
      end if;
      Set_Active(Gtk_Combo_Box(Get_Object(Object, "cmbmodules")), 0);
   exception
      when An_Exception : Crafting_No_Materials =>
         ShowDialog
           ("You don't have enough materials to start manufacturing " &
            Exception_Message(An_Exception) & ".");
      when An_Exception : Crafting_No_Tools =>
         ShowDialog
           ("You don't have the proper tool to start manufacturing " &
            Exception_Message(An_Exception) & ".");
      when Trade_No_Free_Cargo =>
         ShowDialog
           ("You don't have that much free space in your ship's cargo.");
      when An_Exception : Crafting_No_Workshop =>
         ShowDialog
           ("You don't have proper a workplace to start manufacturing " &
            Exception_Message(An_Exception) & ".");
   end ShowSetRecipe;

-- ****if* Crafts.UI/ShowRecipeInfo
-- SOURCE
   procedure ShowRecipeInfo(Object: access Gtkada_Builder_Record'Class) is
-- ****
      RecipeInfo, WorkplaceName: Unbounded_String := Null_Unbounded_String;
      Recipe: Craft_Data;
      MAmount, CargoIndex: Natural := 0;
      HaveWorkplace, IsMaterial, HaveMaterials: Boolean := True;
      HaveTool: Boolean := False;
      TextLength: Positive;
   begin
      declare
         RecipesIter: Gtk_Tree_Iter;
         RecipesModel: Gtk_Tree_Model;
      begin
         Get_Selected
           (Gtk.Tree_View.Get_Selection
              (Gtk_Tree_View(Get_Object(Object, "treerecipes"))),
            RecipesModel, RecipesIter);
         if RecipesIter = Null_Iter then
            return;
         end if;
         RecipeIndex :=
           To_Unbounded_String(Get_String(RecipesModel, RecipesIter, 1));
      end;
      if Length(RecipeIndex) > 12
        and then Slice(RecipeIndex, 1, 11) = "Deconstruct" then
         Recipe.MaterialTypes.Append
           (New_Item =>
              Items_List(Unbounded_Slice(RecipeIndex, 13, Length(RecipeIndex)))
                .IType);
         Recipe.ResultIndex :=
           Unbounded_Slice(RecipeIndex, 13, Length(RecipeIndex));
         Recipe.MaterialAmounts.Append(New_Item => 1);
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
      else
         Recipe := Recipes_List(RecipeIndex);
         Append
           (RecipeInfo, "Amount:" & Integer'Image(Recipe.ResultAmount) & LF);
      end if;
      Append(RecipeInfo, "Materials needed: ");
      declare
         Materials: array
           (Recipe.MaterialTypes.First_Index ..
                Recipe.MaterialTypes.Last_Index) of Boolean :=
           (others => False);
      begin
         for I in
           Recipe.MaterialTypes.First_Index ..
             Recipe.MaterialTypes.Last_Index loop
            Append(RecipeInfo, LF & "-");
            MAmount := 0;
            for J in Items_List.Iterate loop
               IsMaterial := False;
               if Length(RecipeIndex) > 12
                 and then Slice(RecipeIndex, 1, 11) = "Deconstruct" then
                  if Items_List(J).Name =
                    Items_List(Recipe.ResultIndex).Name then
                     IsMaterial := True;
                  end if;
               else
                  if Items_List(J).IType = Recipe.MaterialTypes(I) then
                     IsMaterial := True;
                  end if;
               end if;
               if IsMaterial then
                  if MAmount > 0 then
                     Append(RecipeInfo, " or");
                  end if;
                  CargoIndex :=
                    FindItem(PlayerShip.Cargo, Objects_Container.Key(J));
                  if CargoIndex = 0 or
                    (CargoIndex > 0
                     and then PlayerShip.Cargo(CargoIndex).Amount <
                       Recipe.MaterialAmounts(I)) then
                     Append(RecipeInfo, "<span foreground=""red"">");
                  else
                     Materials(I) := True;
                  end if;
                  Append
                    (RecipeInfo,
                     Integer'Image(Recipe.MaterialAmounts(I)) & "x" &
                     To_String(Items_List(J).Name));
                  if CargoIndex > 0
                    and then PlayerShip.Cargo(CargoIndex).Amount >=
                      Recipe.MaterialAmounts(I) then
                     TextLength :=
                       Positive'Image(PlayerShip.Cargo(CargoIndex).Amount)'
                         Length;
                     Append
                       (RecipeInfo,
                        "(owned: " &
                        Positive'Image(PlayerShip.Cargo(CargoIndex).Amount)
                          (2 .. TextLength) &
                        ")");
                  else
                     Append(RecipeInfo, "</span>");
                  end if;
                  MAmount := MAmount + 1;
               end if;
            end loop;
         end loop;
         HaveMaterials := True;
         for I in Materials'Range loop
            if not Materials(I) then
               HaveMaterials := False;
               exit;
            end if;
         end loop;
      end;
      if Recipe.Tool /= To_Unbounded_String("None") then
         Append(RecipeInfo, LF & "Tool: ");
         MAmount := 0;
         for I in Items_List.Iterate loop
            if Items_List(I).IType = Recipe.Tool then
               if MAmount > 0 then
                  Append(RecipeInfo, " or ");
               end if;
               CargoIndex :=
                 FindItem(PlayerShip.Cargo, Objects_Container.Key(I));
               if CargoIndex = 0 then
                  Append(RecipeInfo, "<span foreground=""red"">");
               else
                  HaveTool := True;
               end if;
               Append(RecipeInfo, To_String(Items_List(I).Name));
               if CargoIndex = 0 then
                  Append(RecipeInfo, "</span>");
               end if;
               MAmount := MAmount + 1;
            end if;
         end loop;
      else
         HaveTool := True;
      end if;
      Append(RecipeInfo, LF & "Workplace: ");
      HaveWorkplace := False;
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
      if not HaveWorkplace then
         Append(RecipeInfo, "<span foreground=""red"">");
      end if;
      Append(RecipeInfo, WorkplaceName);
      if not HaveWorkplace then
         Append(RecipeInfo, "</span>");
      end if;
      Append
        (RecipeInfo,
         LF & "Skill: " & To_String(Skills_List(Recipe.Skill).Name) & "/" &
         To_String(Attributes_List(Skills_List(Recipe.Skill).Attribute).Name));
      Append
        (RecipeInfo,
         LF & "Time needed:" & Positive'Image(Recipe.Time) & " minutes");
      Set_Markup
        (Gtk_Label(Get_Object(Object, "lblrecipeinfo")),
         To_String(RecipeInfo));
      if HaveMaterials and HaveTool and HaveWorkplace then
         Show_All(Gtk_Widget(Get_Object(Object, "setcraftbox")));
         Hide(Gtk_Widget(Get_Object(Object, "lblcrafterror")));
         ShowSetRecipe(Object);
      else
         Hide(Gtk_Widget(Get_Object(Object, "setcraftbox")));
         Show_All(Gtk_Widget(Get_Object(Object, "lblcrafterror")));
         if not HaveMaterials then
            Set_Label
              (Gtk_Label(Get_Object(Object, "lblcrafterror")),
               "You can't craft this recipe because you don't have the proper materials.");
         end if;
         if not HaveTool then
            Set_Label
              (Gtk_Label(Get_Object(Object, "lblcrafterror")),
               "You can't craft this recipe because you don't have the proper tool.");
         end if;
         if not HaveWorkplace then
            Set_Label
              (Gtk_Label(Get_Object(Object, "lblcrafterror")),
               "You can't craft this recipe because you don't have the proper workshop.");
         end if;
      end if;
   end ShowRecipeInfo;

-- ****if* Crafts.UI/SetCrafting
-- SOURCE
   procedure SetCrafting(Object: access Gtkada_Builder_Record'Class) is
-- ****
      ModulesBox: constant Gtk_Combo_Box :=
        Gtk_Combo_Box(Get_Object(Object, "cmbmodules"));
      WorkshopName: constant Unbounded_String :=
        To_Unbounded_String
          (Get_String(Get_Model(ModulesBox), Get_Active_Iter(ModulesBox), 0));
      Amount: constant Natural :=
        Natural(Get_Value(Gtk_Adjustment(Get_Object(Builder, "amountadj"))));
   begin
      for I in PlayerShip.Modules.Iterate loop
         if PlayerShip.Modules(I).Name = WorkshopName then
            SetRecipe(Modules_Container.To_Index(I), Amount, RecipeIndex);
            UpdateMessages;
            exit;
         end if;
      end loop;
   end SetCrafting;

-- ****if* Crafts.UI/CreateCraftsUI
-- SOURCE
   procedure CreateCraftsUI(NewBuilder: Gtkada_Builder) is
-- ****
   begin
      Builder := NewBuilder;
      Register_Handler(Builder, "Show_Recipe_Info", ShowRecipeInfo'Access);
      Register_Handler(Builder, "Set_Crafting", SetCrafting'Access);
      On_Key_Press_Event
        (Gtk_Widget(Get_Object(Builder, "spincraftamount")),
         SelectElement'Access, Get_Object(Builder, "btnsetcrafting"));
   end CreateCraftsUI;

-- ****if* Crafts.UI/ShowCraftsUI
-- SOURCE
   procedure ShowCraftsUI is
-- ****
      Deconstructs: UnboundedString_Container.Vector;
      RecipesIter: Gtk_Tree_Iter;
      RecipesList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "recipeslist"));
      CanCraft: Boolean;
      Recipe: Craft_Data;
      CargoIndex: Natural;
      procedure CheckTool(ToolNeeded: Unbounded_String) is
      begin
         if ToolNeeded /= To_Unbounded_String("None") then
            CanCraft := False;
            for I in Items_List.Iterate loop
               if Items_List(I).IType = ToolNeeded then
                  CargoIndex :=
                    FindItem(PlayerShip.Cargo, Objects_Container.Key(I));
                  if CargoIndex > 0 then
                     CanCraft := True;
                     exit;
                  end if;
               end if;
            end loop;
         end if;
      end CheckTool;
   begin
      for Item of PlayerShip.Cargo loop
         for J in Recipes_List.Iterate loop
            if Recipes_List(J).ResultIndex = Item.ProtoIndex
              and then
              (Known_Recipes.Find_Index(Item => Recipes_Container.Key(J)) =
               Positive_Container.No_Index and
               Deconstructs.Find_Index(Item => Item.ProtoIndex) =
                 Positive_Container.No_Index) then
               Deconstructs.Append(New_Item => Item.ProtoIndex);
               exit;
            end if;
         end loop;
      end loop;
      Clear(RecipesList);
      for I in Known_Recipes.First_Index .. Known_Recipes.Last_Index loop
         Append(RecipesList, RecipesIter);
         CanCraft := False;
         Recipe := Recipes_List(Known_Recipes(I));
         for Module of PlayerShip.Modules loop
            if Modules_List(Module.ProtoIndex).MType = Recipe.Workplace
              and then Module.Durability > 0 then
               CanCraft := True;
               exit;
            end if;
         end loop;
         if CanCraft then
            CheckTool(Recipe.Tool);
         end if;
         if CanCraft then
            declare
               Materials: array
                 (Recipe.MaterialTypes.First_Index ..
                      Recipe.MaterialTypes.Last_Index) of Boolean :=
                 (others => False);
            begin
               for K in
                 Recipe.MaterialTypes.First_Index ..
                   Recipe.MaterialTypes.Last_Index loop
                  for J in Items_List.Iterate loop
                     if Items_List(J).IType = Recipe.MaterialTypes(K) then
                        CargoIndex :=
                          FindItem(PlayerShip.Cargo, Objects_Container.Key(J));
                        if CargoIndex > 0
                          and then PlayerShip.Cargo(CargoIndex).Amount >=
                            Recipe.MaterialAmounts(K) then
                           Materials(K) := True;
                        end if;
                     end if;
                  end loop;
               end loop;
               CanCraft := True;
               for I in Materials'Range loop
                  if not Materials(I) then
                     CanCraft := False;
                     exit;
                  end if;
               end loop;
            end;
         end if;
         if CanCraft then
            Set
              (RecipesList, RecipesIter, 0,
               To_String
                 (Items_List(Recipes_List(Known_Recipes(I)).ResultIndex)
                    .Name));
         else
            Set
              (RecipesList, RecipesIter, 0,
               "<span foreground=""gray"">" &
               To_String
                 (Items_List(Recipes_List(Known_Recipes(I)).ResultIndex)
                    .Name) &
               "</span>");
         end if;
         Set(RecipesList, RecipesIter, 1, To_String(Known_Recipes.Element(I)));
      end loop;
      for I in Deconstructs.First_Index .. Deconstructs.Last_Index loop
         CheckTool(AlchemyTools);
         Append(RecipesList, RecipesIter);
         if CanCraft then
            Set
              (RecipesList, RecipesIter, 0,
               "Deconstruct " & To_String(Items_List(Deconstructs(I)).Name));
         else
            Set
              (RecipesList, RecipesIter, 0,
               "<span foreground=""gray"">Deconstruct " &
               To_String(Items_List(Deconstructs(I)).Name) & "</span>");
         end if;
         Set
           (RecipesList, RecipesIter, 1,
            "Deconstruct " & To_String(Deconstructs(I)));
      end loop;
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")), "crafts");
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treerecipes")),
         Gtk_Tree_Path_New_From_String("0"), null, False);
   end ShowCraftsUI;

end Crafts.UI;
