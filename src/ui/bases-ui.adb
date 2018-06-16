--    Copyright 2018 Bartek thindil Jasicki
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

with Gtk.Widget; use Gtk.Widget;
with Gtk.Label; use Gtk.Label;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Button; use Gtk.Button;
with Gtk.Window; use Gtk.Window;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Ships; use Ships;
with Ships.Crew; use Ships.Crew;
with Items; use Items;
with Bases.Ship; use Bases.Ship;
with Bases.Trade; use Bases.Trade;
with Crafts; use Crafts;
with Utils.UI; use Utils.UI;

package body Bases.UI is

   Builder: Gtkada_Builder;
   type States is (RECIPES, REPAIRS, HEAL, CLEARING);
   CurrentState: States;

   procedure ShowRecruitInfo(Object: access Gtkada_Builder_Record'Class) is
      RecruitIter, Iter: Gtk_Tree_Iter;
      RecruitModel: Gtk_Tree_Model;
      RecruitInfo: Unbounded_String;
      Recruit: Recruit_Data;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      List: Gtk_List_Store;
      Cost, RecruitIndex: Positive;
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treerecruits"))),
         RecruitModel,
         RecruitIter);
      if RecruitIter = Null_Iter then
         return;
      end if;
      RecruitIndex := Positive(Get_Int(RecruitModel, RecruitIter, 1));
      Recruit := SkyBases(BaseIndex).Recruits(RecruitIndex);
      if Recruit.Gender = 'M' then
         RecruitInfo := To_Unbounded_String("Gender: Male");
      else
         RecruitInfo := To_Unbounded_String("Gender: Female");
      end if;
      Set_Markup
        (Gtk_Label(Get_Object(Object, "lblrecruitinfo")),
         To_String(RecruitInfo));
      List := Gtk_List_Store(Get_Object(Object, "statslist"));
      Clear(List);
      for I in Recruit.Attributes.Iterate loop
         Append(List, Iter);
         Set
           (List,
            Iter,
            0,
            To_String(Attributes_List(Attributes_Container.To_Index(I)).Name));
         Set(List, Iter, 1, Gint(Recruit.Attributes(I)(1) * 2));
         Set
           (List,
            Iter,
            2,
            To_String
              (Attributes_List(Attributes_Container.To_Index(I)).Description));
      end loop;
      List := Gtk_List_Store(Get_Object(Object, "skillslist"));
      Clear(List);
      for Skill of Recruit.Skills loop
         Append(List, Iter);
         Set(List, Iter, 0, To_String(Skills_List(Skill(1)).Name));
         Set(List, Iter, 1, Gint(Skill(2)));
         Set
           (List,
            Iter,
            2,
            "Related statistic: " &
            To_String(Attributes_List(Skills_List(Skill(1)).Attribute).Name) &
            ". " &
            To_String(Skills_List(Skill(1)).Description));
      end loop;
      List := Gtk_List_Store(Get_Object(Object, "equipmentlist"));
      Clear(List);
      for Item of Recruit.Inventory loop
         Append(List, Iter);
         Set(List, Iter, 0, To_String(Items_List(Item).Name));
      end loop;
      RecruitInfo := To_Unbounded_String("Starting offer:");
      Append
        (RecruitInfo,
         ASCII.LF &
         "Payment:" &
         Natural'Image(Recruit.Payment) &
         " " &
         To_String(MoneyName) &
         " each day.");
      Cost := Recruit.Price;
      CountPrice(Cost, FindMember(Talk));
      Append
        (RecruitInfo,
         ASCII.LF &
         "One time fee:" &
         Positive'Image(Cost) &
         " " &
         To_String(MoneyName) &
         ".");
      Set_Label
        (Gtk_Label(Get_Object(Object, "lblpayment")),
         To_String(RecruitInfo));
   end ShowRecruitInfo;

   procedure SetActiveRow(TreeViewName, ColumnName: String) is
   begin
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, TreeViewName)),
         Gtk_Tree_Path_New_From_String("0"),
         Gtk_Tree_View_Column(Get_Object(Builder, ColumnName)),
         False);
   end SetActiveRow;

   procedure Hire(Object: access Gtkada_Builder_Record'Class) is
      RecruitIter: Gtk_Tree_Iter;
      RecruitModel: Gtk_Tree_Model;
      RecruitIndex: Positive;
      DailyPayment: constant Natural :=
        Natural
          (Get_Value(Gtk_Adjustment(Get_Object(Object, "adjdailypayment"))));
      TradePayment: constant Natural :=
        Natural
          (Get_Value(Gtk_Adjustment(Get_Object(Object, "adjtradepayment"))));
      Cost, ContractLength2: Integer;
      Recruit: Recruit_Data;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      ContractLength: constant Gint :=
        Get_Active(Gtk_Combo_Box(Get_Object(Object, "cmbcontractlength")));
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treerecruits"))),
         RecruitModel,
         RecruitIter);
      RecruitIndex :=
        Natural'Value(To_String(Get_Path(RecruitModel, RecruitIter))) + 1;
      Recruit := SkyBases(BaseIndex).Recruits(RecruitIndex);
      Cost :=
        Recruit.Price -
        ((DailyPayment - Recruit.Payment) * 50) -
        (TradePayment * 5000);
      case ContractLength is
         when 1 =>
            Cost := Cost - Integer(Float(Recruit.Price) * 0.1);
            ContractLength2 := 100;
         when 2 =>
            Cost := Cost - Integer(Float(Recruit.Price) * 0.5);
            ContractLength2 := 30;
         when 3 =>
            Cost := Cost - Integer(Float(Recruit.Price) * 0.75);
            ContractLength2 := 20;
         when 4 =>
            Cost := Cost - Integer(Float(Recruit.Price) * 0.9);
            ContractLength2 := 10;
         when others =>
            ContractLength2 := -1;
      end case;
      if Cost < 1 then
         Cost := 1;
      end if;
      Hide(Gtk_Widget(Get_Object(Object, "negotiatewindow")));
      HireRecruit
        (RecruitIndex,
         Cost,
         DailyPayment,
         TradePayment,
         ContractLength2);
      Remove(-(RecruitModel), RecruitIter);
      SetActiveRow("treerecruits", "columnname");
      ShowLastMessage(Object);
   end Hire;

   procedure ObjectSelected(Object: access Gtkada_Builder_Record'Class) is
      Iter: Gtk_Tree_Iter;
      Model: Gtk_Tree_Model;
      Cost, Time: Natural := 0;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseType: constant Positive :=
        Bases_Types'Pos(SkyBases(BaseIndex).BaseType) + 1;
      MoneyIndex2: Natural;
      ObjectIndex: Integer;
      MinChildren: Gint;
      procedure ShowMap is
      begin
         ShowSkyMap;
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Builder, "gamestack")),
            "skymap");
         Set_Deletable(Gtk_Window(Get_Object(Builder, "skymapwindow")), True);
      end ShowMap;
   begin
      if CurrentState = CLEARING then
         return;
      end if;
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treebases1"))),
         Model,
         Iter);
      case CurrentState is
         when RECIPES =>
            if N_Children(Model, Null_Iter) = 0 then
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
            if N_Children(Model, Null_Iter) = MinChildren then
               ShowMap;
               return;
            end if;
         when HEAL =>
            if N_Children(Model, Null_Iter) = 1 then
               ShowMap;
               return;
            end if;
         when CLEARING =>
            null;
      end case;
      if Iter = Null_Iter then
         return;
      end if;
      ObjectIndex := Integer(Get_Int(Model, Iter, 1));
      case CurrentState is
         when RECIPES =>
            if Items_List(Recipes_List(ObjectIndex).ResultIndex).Prices
                (BaseType) >
              0 then
               Cost :=
                 Items_List(Recipes_List(ObjectIndex).ResultIndex).Prices
                   (BaseType) *
                 Recipes_List(ObjectIndex).Difficulty *
                 100;
            else
               Cost := Recipes_List(ObjectIndex).Difficulty * 100;
            end if;
            CountPrice(Cost, FindMember(Talk));
            Set_Label
              (Gtk_Label(Get_Object(Object, "lblbaseinfo2")),
               "Base price:" &
               Positive'Image(Cost) &
               " " &
               To_String(MoneyName));
         when REPAIRS =>
            RepairCost(Cost, Time, ObjectIndex);
            CountPrice(Cost, FindMember(Talk));
            Set_Label
              (Gtk_Label(Get_Object(Object, "lblbaseinfo2")),
               "Repair cost:" &
               Natural'Image(Cost) &
               " " &
               To_String(MoneyName) &
               ASCII.LF &
               "Repair time:" &
               Natural'Image(Time) &
               " minutes");
         when HEAL =>
            HealCost(Cost, Time, ObjectIndex);
            Set_Label
              (Gtk_Label(Get_Object(Object, "lblbaseinfo2")),
               "Heal cost:" &
               Natural'Image(Cost) &
               " " &
               To_String(MoneyName) &
               ASCII.LF &
               "Heal time:" &
               Natural'Image(Time) &
               " minutes");
         when CLEARING =>
            null;
      end case;
      MoneyIndex2 := FindItem(PlayerShip.Cargo, FindProtoItem(MoneyIndex));
      if MoneyIndex2 > 0 then
         Set_Label
           (Gtk_Label(Get_Object(Object, "lblmoneyamount")),
            "You have" &
            Natural'Image(PlayerShip.Cargo(MoneyIndex2).Amount) &
            " " &
            To_String(MoneyName) &
            ".");
         if PlayerShip.Cargo(MoneyIndex2).Amount < Cost then
            Set_Sensitive
              (Gtk_Widget(Get_Object(Object, "btnacceptbase")),
               False);
         else
            Set_Sensitive
              (Gtk_Widget(Get_Object(Object, "btnacceptbase")),
               True);
         end if;
      else
         Set_Sensitive(Gtk_Widget(Get_Object(Object, "btnacceptbase")), False);
         Set_Label
           (Gtk_Label(Get_Object(Object, "lblmoneyamount")),
            "You don't have any money.");
      end if;
   end ObjectSelected;

   procedure AcceptAction(Object: access Gtkada_Builder_Record'Class) is
      Iter: Gtk_Tree_Iter;
      Model: Gtk_Tree_Model;
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treebases1"))),
         Model,
         Iter);
      if Iter = Null_Iter then
         return;
      end if;
      case CurrentState is
         when RECIPES =>
            BuyRecipe(Positive(Get_Int(Model, Iter, 1)));
            ShowBuyRecipesUI;
         when REPAIRS =>
            Bases.Ship.RepairShip(Integer(Get_Int(Model, Iter, 1)));
            ShowRepairUI;
         when HEAL =>
            HealWounded(Natural(Get_Int(Model, Iter, 1)));
            ShowHealUI;
         when CLEARING =>
            null;
      end case;
   end AcceptAction;

   procedure StartNegotiations(Object: access Gtkada_Builder_Record'Class) is
      MoneyIndex2: constant Natural :=
        FindItem(PlayerShip.Cargo, FindProtoItem(MoneyIndex));
      Cost, RecruitIndex: Positive;
      RecruitIter: Gtk_Tree_Iter;
      RecruitModel: Gtk_Tree_Model;
      Recruit: Recruit_Data;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treerecruits"))),
         RecruitModel,
         RecruitIter);
      if RecruitIter = Null_Iter then
         return;
      end if;
      RecruitIndex := Positive(Get_Int(RecruitModel, RecruitIter, 1));
      Recruit := SkyBases(BaseIndex).Recruits(RecruitIndex);
      Set_Upper
        (Gtk_Adjustment(Get_Object(Object, "adjdailypayment")),
         Gdouble(Recruit.Payment * 2));
      Set_Value
        (Gtk_Adjustment(Get_Object(Object, "adjdailypayment")),
         Gdouble(Recruit.Payment));
      Cost := Recruit.Price;
      CountPrice(Cost, FindMember(Talk));
      Set_Label
        (Gtk_Label(Get_Object(Object, "lblrecruitcost")),
         "Hire for" & Positive'Image(Cost) & " " & To_String(MoneyName));
      if MoneyIndex2 > 0 then
         Set_Label
           (Gtk_Label(Get_Object(Object, "lblrecruitmoney")),
            "You have" &
            Natural'Image(PlayerShip.Cargo(MoneyIndex2).Amount) &
            " " &
            To_String(MoneyName) &
            ".");
         if PlayerShip.Cargo(MoneyIndex2).Amount < Cost then
            Set_Sensitive
              (Gtk_Widget(Get_Object(Object, "btnhirerecruit")),
               False);
         else
            Set_Sensitive
              (Gtk_Widget(Get_Object(Object, "btnhirerecruit")),
               True);
         end if;
      else
         Set_Label
           (Gtk_Label(Get_Object(Object, "lblrecruitmoney")),
            "You don't have enough money to recruit anyone");
         Set_Sensitive
           (Gtk_Widget(Get_Object(Object, "btnhirerecruit")),
            False);
      end if;
      Show_All(Gtk_Widget(Get_Object(Object, "negotiatewindow")));
   end StartNegotiations;

   procedure NegotiateHire(Object: access Gtkada_Builder_Record'Class) is
      MoneyIndex2: constant Natural :=
        FindItem(PlayerShip.Cargo, FindProtoItem(MoneyIndex));
      RecruitIndex: Positive;
      RecruitIter: Gtk_Tree_Iter;
      RecruitModel: Gtk_Tree_Model;
      Recruit: Recruit_Data;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Cost: Integer;
      DailyPayment: constant Natural :=
        Natural
          (Get_Value(Gtk_Adjustment(Get_Object(Object, "adjdailypayment"))));
      TradePayment: constant Natural :=
        Natural
          (Get_Value(Gtk_Adjustment(Get_Object(Object, "adjtradepayment"))));
      ContractLength: constant Gint :=
        Get_Active(Gtk_Combo_Box(Get_Object(Object, "cmbcontractlength")));
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treerecruits"))),
         RecruitModel,
         RecruitIter);
      if RecruitIter = Null_Iter then
         return;
      end if;
      RecruitIndex := Positive(Get_Int(RecruitModel, RecruitIter, 1));
      Recruit := SkyBases(BaseIndex).Recruits(RecruitIndex);
      Cost :=
        Recruit.Price -
        ((DailyPayment - Recruit.Payment) * 50) -
        (TradePayment * 5000);
      case ContractLength is
         when 1 =>
            Cost := Cost - Integer(Float(Recruit.Price) * 0.1);
         when 2 =>
            Cost := Cost - Integer(Float(Recruit.Price) * 0.5);
         when 3 =>
            Cost := Cost - Integer(Float(Recruit.Price) * 0.75);
         when 4 =>
            Cost := Cost - Integer(Float(Recruit.Price) * 0.9);
         when others =>
            null;
      end case;
      if Cost < 1 then
         Cost := 1;
      end if;
      CountPrice(Cost, FindMember(Talk));
      Set_Label
        (Gtk_Label(Get_Object(Object, "lblrecruitcost")),
         "Hire for" & Positive'Image(Cost) & " " & To_String(MoneyName));
      if MoneyIndex2 > 0 then
         if PlayerShip.Cargo(MoneyIndex2).Amount < Cost then
            Set_Sensitive
              (Gtk_Widget(Get_Object(Object, "btnhirerecruit")),
               False);
         else
            Set_Sensitive
              (Gtk_Widget(Get_Object(Object, "btnhirerecruit")),
               True);
         end if;
      end if;
   end NegotiateHire;

   procedure CreateBasesUI(NewBuilder: Gtkada_Builder) is
   begin
      Builder := NewBuilder;
      Register_Handler(Builder, "Show_Recruit_Info", ShowRecruitInfo'Access);
      Register_Handler(Builder, "Hire_Recruit", Hire'Access);
      Register_Handler(Builder, "Object_Selected", ObjectSelected'Access);
      Register_Handler(Builder, "Accept_Action", AcceptAction'Access);
      Register_Handler
        (Builder,
         "Start_Negotiations",
         StartNegotiations'Access);
      Register_Handler(Builder, "Negotiate_Hire", NegotiateHire'Access);
   end CreateBasesUI;

   procedure ShowRecruitUI is
      RecruitIter: Gtk_Tree_Iter;
      RecruitList: Gtk_List_Store;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
   begin
      RecruitList := Gtk_List_Store(Get_Object(Builder, "recruitlist"));
      Clear(RecruitList);
      for I in SkyBases(BaseIndex).Recruits.Iterate loop
         Append(RecruitList, RecruitIter);
         Set
           (RecruitList,
            RecruitIter,
            0,
            To_String(SkyBases(BaseIndex).Recruits(I).Name));
         Set(RecruitList, RecruitIter, 1, Gint(Recruit_Container.To_Index(I)));
      end loop;
      Show_All(Gtk_Widget(Get_Object(Builder, "btnshowhelp")));
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")),
         "recruit");
      Set_Deletable(Gtk_Window(Get_Object(Builder, "skymapwindow")), False);
      SetActiveRow("treerecruits", "columnname");
      ShowLastMessage(Builder);
   end ShowRecruitUI;

   procedure ShowBuyRecipesUI is
      RecipesIter: Gtk_Tree_Iter;
      RecipesList: Gtk_List_Store;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseType: constant Positive :=
        Bases_Types'Pos(SkyBases(BaseIndex).BaseType) + 1;
   begin
      CurrentState := CLEARING;
      RecipesList := Gtk_List_Store(Get_Object(Builder, "itemslist"));
      Clear(RecipesList);
      CurrentState := RECIPES;
      for I in Recipes_List.Iterate loop
         if Recipes_List(I).BaseType = BaseType and
           Known_Recipes.Find_Index(Item => Recipes_Container.To_Index(I)) =
             Positive_Container.No_Index then
            Append(RecipesList, RecipesIter);
            Set
              (RecipesList,
               RecipesIter,
               0,
               To_String(Items_List(Recipes_List(I).ResultIndex).Name));
            Set
              (RecipesList,
               RecipesIter,
               1,
               Gint(Recipes_Container.To_Index(I)));
         end if;
      end loop;
      Set_Label
        (Gtk_Button(Get_Object(Builder, "btnacceptbase")),
         "_Buy recipe");
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")),
         "base");
      Set_Deletable(Gtk_Window(Get_Object(Builder, "skymapwindow")), False);
      SetActiveRow("treebases1", "columnbases");
      ShowLastMessage(Builder);
   end ShowBuyRecipesUI;

   procedure ShowRepairUI is
      RepairsIter: Gtk_Tree_Iter;
      RepairsList: Gtk_List_Store;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
   begin
      CurrentState := CLEARING;
      RepairsList := Gtk_List_Store(Get_Object(Builder, "itemslist"));
      Clear(RepairsList);
      CurrentState := REPAIRS;
      for I in PlayerShip.Modules.Iterate loop
         if PlayerShip.Modules(I).Durability <
           PlayerShip.Modules(I).MaxDurability then
            Append(RepairsList, RepairsIter);
            Set
              (RepairsList,
               RepairsIter,
               0,
               To_String(PlayerShip.Modules(I).Name));
            Set
              (RepairsList,
               RepairsIter,
               1,
               Gint(Modules_Container.To_Index(I)));
         end if;
      end loop;
      Append(RepairsList, RepairsIter);
      Set(RepairsList, RepairsIter, 0, "Slowly repair the whole ship");
      Set(RepairsList, RepairsIter, 1, 0);
      if SkyBases(BaseIndex).Population > 149 then
         Append(RepairsList, RepairsIter);
         Set(RepairsList, RepairsIter, 0, "Repair the whole ship");
         Set(RepairsList, RepairsIter, 1, -1);
      end if;
      if SkyBases(BaseIndex).Population > 299 then
         Append(RepairsList, RepairsIter);
         Set(RepairsList, RepairsIter, 0, "Quickly repair the whole ship");
         Set(RepairsList, RepairsIter, 1, -2);
      end if;
      Set_Label
        (Gtk_Button(Get_Object(Builder, "btnacceptbase")),
         "_Buy repairs");
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")),
         "base");
      Set_Deletable(Gtk_Window(Get_Object(Builder, "skymapwindow")), False);
      SetActiveRow("treebases1", "columnbases");
      ShowLastMessage(Builder);
   end ShowRepairUI;

   procedure ShowHealUI is
      HealsIter: Gtk_Tree_Iter;
      HealsList: Gtk_List_Store;
   begin
      CurrentState := CLEARING;
      HealsList := Gtk_List_Store(Get_Object(Builder, "itemslist"));
      Clear(HealsList);
      CurrentState := HEAL;
      for I in PlayerShip.Crew.Iterate loop
         if PlayerShip.Crew(I).Health < 100 then
            Append(HealsList, HealsIter);
            Set(HealsList, HealsIter, 0, To_String(PlayerShip.Crew(I).Name));
            Set(HealsList, HealsIter, 1, Gint(Crew_Container.To_Index(I)));
         end if;
      end loop;
      Append(HealsList, HealsIter);
      Set(HealsList, HealsIter, 0, "Heal all wounded crew members");
      Set(HealsList, HealsIter, 1, 0);
      Set_Label
        (Gtk_Button(Get_Object(Builder, "btnacceptbase")),
         "_Buy healing");
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")),
         "base");
      Set_Deletable(Gtk_Window(Get_Object(Builder, "skymapwindow")), False);
      SetActiveRow("treebases1", "columnbases");
      ShowLastMessage(Builder);
   end ShowHealUI;

end Bases.UI;
