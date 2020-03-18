--    Copyright 2018-2020 Bartek thindil Jasicki
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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Label; use Gtk.Label;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Button; use Gtk.Button;
with Gtk.Stack; use Gtk.Stack;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Gtkada.Builder; use Gtkada.Builder;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Ships; use Ships;
with Ships.Crew; use Ships.Crew;
with Items; use Items;
with Bases.Ship; use Bases.Ship;
with Bases.Trade; use Bases.Trade;
with Crafts; use Crafts;
with Utils.UI; use Utils.UI;
with Config; use Config;
with BasesTypes; use BasesTypes;

package body Bases.UI is

   -- ****it* Bases.UI/States
   -- FUNCTION
   -- Used to determine the player action in base
   -- OPTIONS
   -- RECIPES  - Player is buying crafting recipes
   -- REPAIRS  - Player is buying ship repairs
   -- HEAL     - Player is buying healing crew members
   -- CLEARING - Clearing list of available options
   -- SOURCE
   type States is (RECIPES, REPAIRS, HEAL, CLEARING);
   -- ****
   -- ****iv* Bases.UI/CurrentState
   -- FUNCTION
   -- Current UI state
   -- SOURCE
   CurrentState: States;
   -- ****

   -- ****if* Bases.UI/ObjectSelected
   -- FUNCTIONS
   -- Show information about currently selected option
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure ObjectSelected(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      Iter: Gtk_Tree_Iter;
      Model: Gtk_Tree_Model;
      Cost, Time: Natural := 0;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      MoneyIndex2: Natural;
      MinChildren: Gint;
      FormattedTime, ObjectIndex: Unbounded_String;
      ItemsModel: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Object, "itemslist"));
      procedure ShowMap is
      begin
         ShowSkyMap;
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Builder, "gamestack")), "skymap");
      end ShowMap;
      procedure FormatTime is
      begin
         if Time < 60 then
            FormattedTime :=
              To_Unbounded_String(Natural'Image(Time) & " minute");
            if Time > 1 then
               Append(FormattedTime, "s");
            end if;
         else
            FormattedTime :=
              To_Unbounded_String(Positive'Image(Time / 60) & " hour");
            if (Time / 60) > 1 then
               Append(FormattedTime, "s");
            end if;
            if (Time mod 60) > 0 then
               Append
                 (FormattedTime,
                  " and" & Positive'Image(Time mod 60) & " minute");
               if (Time mod 60) > 1 then
                  Append(FormattedTime, "s");
               end if;
            end if;
         end if;
      end FormatTime;
   begin
      if CurrentState = CLEARING then
         return;
      end if;
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treebases1"))),
         Model, Iter);
      case CurrentState is
         when RECIPES =>
            if N_Children(ItemsModel, Null_Iter) = 0 then
               ShowMap;
               return;
            end if;
         when REPAIRS =>
            if SkyBases(BaseIndex).Population < 150 then
               MinChildren := 1;
            elsif SkyBases(BaseIndex).Population > 149 and
              SkyBases(BaseIndex).Population < 300 then
               MinChildren := 2;
            else
               MinChildren := 3;
            end if;
            if N_Children(ItemsModel, Null_Iter) = MinChildren then
               ShowMap;
               return;
            end if;
         when HEAL =>
            if N_Children(ItemsModel, Null_Iter) = 1 then
               ShowMap;
               return;
            end if;
         when CLEARING =>
            null;
      end case;
      if Iter = Null_Iter then
         Hide(Gtk_Widget(Get_Object(Object, "scrollbase")));
         return;
      else
         Show_All(Gtk_Widget(Get_Object(Object, "scrollbase")));
      end if;
      ObjectIndex := To_Unbounded_String(Get_String(Model, Iter, 1));
      case CurrentState is
         when RECIPES =>
            if Get_Price
                (SkyBases(BaseIndex).BaseType,
                 Recipes_List(ObjectIndex).ResultIndex) >
              0 then
               Cost :=
                 Get_Price
                   (SkyBases(BaseIndex).BaseType,
                    Recipes_List(ObjectIndex).ResultIndex) *
                 Recipes_List(ObjectIndex).Difficulty * 10;
            else
               Cost := Recipes_List(ObjectIndex).Difficulty * 10;
            end if;
            Cost := Natural(Float(Cost) * NewGameSettings.PricesBonus);
            if Cost = 0 then
               Cost := 1;
            end if;
            CountPrice(Cost, FindMember(Talk));
            Set_Label
              (Gtk_Label(Get_Object(Object, "lblbaseinfo2")),
               "Base price:" & Positive'Image(Cost) & " " &
               To_String(MoneyName));
         when REPAIRS =>
            RepairCost(Cost, Time, Integer'Value(To_String(ObjectIndex)));
            CountPrice(Cost, FindMember(Talk));
            FormatTime;
            Set_Label
              (Gtk_Label(Get_Object(Object, "lblbaseinfo2")),
               "Repair cost:" & Natural'Image(Cost) & " " &
               To_String(MoneyName) & LF & "Repair time:" &
               To_String(FormattedTime));
         when HEAL =>
            HealCost(Cost, Time, Integer'Value(To_String(ObjectIndex)));
            FormatTime;
            Set_Label
              (Gtk_Label(Get_Object(Object, "lblbaseinfo2")),
               "Heal cost:" & Natural'Image(Cost) & " " &
               To_String(MoneyName) & LF & "Heal time:" &
               To_String(FormattedTime));
         when CLEARING =>
            null;
      end case;
      MoneyIndex2 := FindItem(PlayerShip.Cargo, MoneyIndex);
      if MoneyIndex2 > 0 then
         Set_Label
           (Gtk_Label(Get_Object(Object, "lblmoneyamount")),
            "You have" & Natural'Image(PlayerShip.Cargo(MoneyIndex2).Amount) &
            " " & To_String(MoneyName) & ".");
         if PlayerShip.Cargo(MoneyIndex2).Amount < Cost then
            Set_Sensitive
              (Gtk_Widget(Get_Object(Object, "btnacceptbase")), False);
         else
            Set_Sensitive
              (Gtk_Widget(Get_Object(Object, "btnacceptbase")), True);
         end if;
      else
         Set_Sensitive(Gtk_Widget(Get_Object(Object, "btnacceptbase")), False);
         Set_Label
           (Gtk_Label(Get_Object(Object, "lblmoneyamount")),
            "You don't have any money.");
      end if;
   end ObjectSelected;

   -- ****if* Bases.UI/AcceptAction
   -- FUNCTION
   -- Execute currently selected option
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure AcceptAction(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      Iter: Gtk_Tree_Iter;
      Model: Gtk_Tree_Model;
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treebases1"))),
         Model, Iter);
      if Iter = Null_Iter then
         return;
      end if;
      case CurrentState is
         when RECIPES =>
            BuyRecipe(To_Unbounded_String(Get_String(Model, Iter, 1)));
            ShowBuyRecipesUI;
         when REPAIRS =>
            Bases.Ship.RepairShip(Integer'Value(Get_String(Model, Iter, 1)));
            ShowRepairUI;
         when HEAL =>
            HealWounded(Natural'Value(Get_String(Model, Iter, 1)));
            ShowHealUI;
         when CLEARING =>
            null;
      end case;
   end AcceptAction;

   -- ****if* Bases.UI/VisibleRecipes
   -- FUNCTION
   -- Check if selected recipe is visible on recipes list
   -- PARAMETERS
   -- Model - Gtk_Tree_Model with recipes which will be checked
   -- Iter  - Gtk_Tree_Iter of recipe which will be checked
   -- RESULT
   -- True if recipe should be visible, otherwise false
   -- SOURCE
   function VisibleRecipes
     (Model: Gtk_Tree_Model; Iter: Gtk_Tree_Iter) return Boolean is
      -- ****
      SearchEntry: constant Gtk_GEntry :=
        Gtk_GEntry(Get_Object(Builder, "searchrecipe"));
   begin
      if Get_Text(SearchEntry) = "" then
         return True;
      end if;
      if Index
          (To_Lower(Get_String(Model, Iter, 0)),
           To_Lower(Get_Text(SearchEntry)), 1) >
        0 then
         return True;
      end if;
      return False;
   end VisibleRecipes;

   -- ****if* Bases.UI/SearchRecipes
   -- FUNCTION
   -- Search recipe by it name
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure SearchRecipes(Object: access Gtkada_Builder_Record'Class) is
   -- ****
   begin
      Refilter(Gtk_Tree_Model_Filter(Get_Object(Object, "itemsfilter")));
      if N_Children
          (Gtk_List_Store(Get_Object(Builder, "itemslist")), Null_Iter) >
        0 then
         Set_Cursor
           (Gtk_Tree_View(Get_Object(Builder, "treebases1")),
            Gtk_Tree_Path_New_From_String("0"), null, False);
      end if;
   end SearchRecipes;

   procedure CreateBasesUI is
   begin
      Register_Handler(Builder, "Object_Selected", ObjectSelected'Access);
      Register_Handler(Builder, "Accept_Action", AcceptAction'Access);
      Register_Handler(Builder, "Search_Recipe", SearchRecipes'Access);
      Set_Visible_Func
        (Gtk_Tree_Model_Filter(Get_Object(Builder, "itemsfilter")),
         VisibleRecipes'Access);
      On_Key_Press_Event
        (Gtk_Widget(Get_Object(Builder, "searchrecipe")), SelectElement'Access,
         Get_Object(Builder, "btnmenu"));
   end CreateBasesUI;

   procedure ShowBuyRecipesUI is
      RecipesIter: Gtk_Tree_Iter;
      RecipesList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "itemslist"));
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseType: constant Unbounded_String := SkyBases(BaseIndex).BaseType;
   begin
      CurrentState := CLEARING;
      Clear(RecipesList);
      CurrentState := RECIPES;
      for I in Recipes_List.Iterate loop
         if BasesTypes_List(BaseType).Recipes.Contains
             (Recipes_Container.Key(I)) and
           Known_Recipes.Find_Index(Item => Recipes_Container.Key(I)) =
             Positive_Container.No_Index and
           Recipes_List(I).Reputation <= SkyBases(BaseIndex).Reputation(1) then
            Append(RecipesList, RecipesIter);
            Set
              (RecipesList, RecipesIter, 0,
               To_String(Items_List(Recipes_List(I).ResultIndex).Name));
            Set
              (RecipesList, RecipesIter, 1,
               To_String(Recipes_Container.Key(I)));
         end if;
      end loop;
      Set_Label
        (Gtk_Button(Get_Object(Builder, "btnacceptbase")), "_Buy recipe");
      Set_Placeholder_Text
        (Gtk_GEntry(Get_Object(Builder, "searchrecipe")), "<Search recipes>");
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")), "base");
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treebases1")),
         Gtk_Tree_Path_New_From_String("0"), null, False);
      UpdateMessages;
   end ShowBuyRecipesUI;

   procedure ShowRepairUI is
      RepairsIter: Gtk_Tree_Iter;
      RepairsList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "itemslist"));
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
   begin
      CurrentState := CLEARING;
      Clear(RepairsList);
      CurrentState := REPAIRS;
      for I in PlayerShip.Modules.Iterate loop
         if PlayerShip.Modules(I).Durability <
           PlayerShip.Modules(I).MaxDurability then
            Append(RepairsList, RepairsIter);
            Set
              (RepairsList, RepairsIter, 0,
               To_String(PlayerShip.Modules(I).Name));
            Set
              (RepairsList, RepairsIter, 1,
               Integer'Image(Modules_Container.To_Index(I)));
         end if;
      end loop;
      Append(RepairsList, RepairsIter);
      Set(RepairsList, RepairsIter, 0, "Slowly repair the whole ship");
      Set(RepairsList, RepairsIter, 1, "0");
      if SkyBases(BaseIndex).Population > 149 then
         Append(RepairsList, RepairsIter);
         Set(RepairsList, RepairsIter, 0, "Repair the whole ship");
         Set(RepairsList, RepairsIter, 1, "-1");
      end if;
      if SkyBases(BaseIndex).Population > 299 then
         Append(RepairsList, RepairsIter);
         Set(RepairsList, RepairsIter, 0, "Quickly repair the whole ship");
         Set(RepairsList, RepairsIter, 1, "-2");
      end if;
      Set_Label
        (Gtk_Button(Get_Object(Builder, "btnacceptbase")), "_Buy repairs");
      Set_Placeholder_Text
        (Gtk_GEntry(Get_Object(Builder, "searchrecipe")), "<Search action>");
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")), "base");
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treebases1")),
         Gtk_Tree_Path_New_From_String("0"), null, False);
      UpdateMessages;
   end ShowRepairUI;

   procedure ShowHealUI is
      HealsIter: Gtk_Tree_Iter;
      HealsList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "itemslist"));
   begin
      CurrentState := CLEARING;
      Clear(HealsList);
      CurrentState := HEAL;
      for I in PlayerShip.Crew.Iterate loop
         if PlayerShip.Crew(I).Health < 100 then
            Append(HealsList, HealsIter);
            Set(HealsList, HealsIter, 0, To_String(PlayerShip.Crew(I).Name));
            Set
              (HealsList, HealsIter, 1,
               Integer'Image(Crew_Container.To_Index(I)));
         end if;
      end loop;
      Append(HealsList, HealsIter);
      Set(HealsList, HealsIter, 0, "Heal all wounded crew members");
      Set(HealsList, HealsIter, 1, Gint'Image(0));
      Set_Label
        (Gtk_Button(Get_Object(Builder, "btnacceptbase")), "_Buy healing");
      Set_Placeholder_Text
        (Gtk_GEntry(Get_Object(Builder, "searchrecipe")), "<Search action>");
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")), "base");
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treebases1")),
         Gtk_Tree_Path_New_From_String("0"), null, False);
      UpdateMessages;
   end ShowHealUI;

end Bases.UI;
