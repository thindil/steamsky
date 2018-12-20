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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Label; use Gtk.Label;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Maps; use Maps;
with Ships; use Ships;
with Ships.Crew; use Ships.Crew;
with Items; use Items;
with Bases.Trade; use Bases.Trade;
with Utils.UI; use Utils.UI;
with Factions; use Factions;
with Maps.UI; use Maps.UI;

package body Bases.RecruitUI is

   Builder: Gtkada_Builder;

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
         RecruitModel, RecruitIter);
      if RecruitIter = Null_Iter then
         return;
      end if;
      RecruitIndex := Positive(Get_Int(RecruitModel, RecruitIter, 1));
      if RecruitIndex > SkyBases(BaseIndex).Recruits.Last_Index then
         return;
      end if;
      Recruit := SkyBases(BaseIndex).Recruits(RecruitIndex);
      if not Factions_List(Recruit.Faction).Flags.Contains
          (To_Unbounded_String("nogender")) then
         if Recruit.Gender = 'M' then
            RecruitInfo := To_Unbounded_String("Gender: Male");
         else
            RecruitInfo := To_Unbounded_String("Gender: Female");
         end if;
      end if;
      Append(RecruitInfo, LF & "Faction: ");
      Append(RecruitInfo, Factions_List(Recruit.Faction).Name);
      Append(RecruitInfo, LF & "Home base: ");
      Append(RecruitInfo, SkyBases(Recruit.HomeBase).Name);
      Set_Markup
        (Gtk_Label(Get_Object(Object, "lblrecruitinfo")),
         To_String(RecruitInfo));
      List := Gtk_List_Store(Get_Object(Object, "statslist"));
      Clear(List);
      for I in Recruit.Attributes.Iterate loop
         Append(List, Iter);
         Set
           (List, Iter, 0,
            To_String(Attributes_List(Attributes_Container.To_Index(I)).Name));
         Set(List, Iter, 1, Gint(Recruit.Attributes(I)(1) * 2));
         Set
           (List, Iter, 2,
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
           (List, Iter, 2,
            "Related statistic: " &
            To_String(Attributes_List(Skills_List(Skill(1)).Attribute).Name) &
            ". " & To_String(Skills_List(Skill(1)).Description));
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
         LF & "Payment:" & Natural'Image(Recruit.Payment) & " " &
         To_String(MoneyName) & " each day.");
      Cost := Recruit.Price;
      CountPrice(Cost, FindMember(Talk));
      Append
        (RecruitInfo,
         LF & "One time fee:" & Positive'Image(Cost) & " " &
         To_String(MoneyName) & ".");
      Set_Label
        (Gtk_Label(Get_Object(Object, "lblpayment")), To_String(RecruitInfo));
   end ShowRecruitInfo;

   procedure SetActiveRow(TreeViewName, ColumnName: String) is
   begin
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, TreeViewName)),
         Gtk_Tree_Path_New_From_String("0"),
         Gtk_Tree_View_Column(Get_Object(Builder, ColumnName)), False);
   end SetActiveRow;

   procedure UpdateRecruitList is
      RecruitIter: Gtk_Tree_Iter;
      RecruitList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "recruitlist"));
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
   begin
      Clear(RecruitList);
      for I in SkyBases(BaseIndex).Recruits.Iterate loop
         Append(RecruitList, RecruitIter);
         Set
           (RecruitList, RecruitIter, 0,
            To_String(SkyBases(BaseIndex).Recruits(I).Name));
         Set(RecruitList, RecruitIter, 1, Gint(Recruit_Container.To_Index(I)));
      end loop;
   end UpdateRecruitList;

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
         RecruitModel, RecruitIter);
      RecruitIndex :=
        Natural'Value(To_String(Get_Path(RecruitModel, RecruitIter))) + 1;
      Recruit := SkyBases(BaseIndex).Recruits(RecruitIndex);
      Cost :=
        Recruit.Price - ((DailyPayment - Recruit.Payment) * 50) -
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
        (RecruitIndex, Cost, DailyPayment, TradePayment, ContractLength2);
      UpdateRecruitList;
      if SkyBases(BaseIndex).Recruits.Length > 0 then
         SetActiveRow("treerecruits", "columnname");
         ShowLastMessage(Object);
      else
         ShowSkyMap;
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Object, "gamestack")), "skymap");
      end if;
   end Hire;

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
         RecruitModel, RecruitIter);
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
            "You have" & Natural'Image(PlayerShip.Cargo(MoneyIndex2).Amount) &
            " " & To_String(MoneyName) & ".");
         if PlayerShip.Cargo(MoneyIndex2).Amount < Cost then
            Set_Sensitive
              (Gtk_Widget(Get_Object(Object, "btnhirerecruit")), False);
         else
            Set_Sensitive
              (Gtk_Widget(Get_Object(Object, "btnhirerecruit")), True);
         end if;
      else
         Set_Label
           (Gtk_Label(Get_Object(Object, "lblrecruitmoney")),
            "You don't have enough money to recruit anyone");
         Set_Sensitive
           (Gtk_Widget(Get_Object(Object, "btnhirerecruit")), False);
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
         RecruitModel, RecruitIter);
      if RecruitIter = Null_Iter then
         return;
      end if;
      RecruitIndex := Positive(Get_Int(RecruitModel, RecruitIter, 1));
      Recruit := SkyBases(BaseIndex).Recruits(RecruitIndex);
      Cost :=
        Recruit.Price - ((DailyPayment - Recruit.Payment) * 50) -
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
              (Gtk_Widget(Get_Object(Object, "btnhirerecruit")), False);
         else
            Set_Sensitive
              (Gtk_Widget(Get_Object(Object, "btnhirerecruit")), True);
         end if;
      end if;
   end NegotiateHire;

   procedure CreateRecruitUI(NewBuilder: Gtkada_Builder) is
   begin
      Builder := NewBuilder;
      Register_Handler(Builder, "Show_Recruit_Info", ShowRecruitInfo'Access);
      Register_Handler(Builder, "Hire_Recruit", Hire'Access);
      Register_Handler
        (Builder, "Start_Negotiations", StartNegotiations'Access);
      Register_Handler(Builder, "Negotiate_Hire", NegotiateHire'Access);
   end CreateRecruitUI;

   procedure ShowRecruitUI is
   begin
      UpdateRecruitList;
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")), "recruit");
      SetActiveRow("treerecruits", "columnname");
      ShowLastMessage(Builder);
   end ShowRecruitUI;

end Bases.RecruitUI;
