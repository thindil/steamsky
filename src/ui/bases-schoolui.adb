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

with Gtk.Widget; use Gtk.Widget;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Label; use Gtk.Label;
with Glib; use Glib;
with Ships; use Ships;
with Bases.Trade; use Bases.Trade;
with Utils.UI; use Utils.UI;
with Trades; use Trades;
with Items; use Items;

package body Bases.SchoolUI is

   -- ****iv* Bases.SchoolUI/Builder
   -- FUNCTION
   -- Gtkada_Builder used for creating UI
   -- SOURCE
   Builder: Gtkada_Builder;
   -- ****
   -- ****iv* Bases.SchoolUI/CrewIndex
   -- FUNCTION
   -- Crew index of currently selected member
   -- SOURCE
   CrewIndex: Positive;
   -- ****

   -- ****if* Bases.SchoolUI/ShowTrainInfo
   -- FUNCTION
   -- Show information about training cost for selected crew member
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure ShowTrainInfo(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      CrewIter, SkillsIter: Gtk_Tree_Iter;
      CrewModel: Gtk_Tree_Model;
      SkillsList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Object, "skillslist1"));
      MoneyIndex2: Natural;
      Cost: Gint;
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treecrew"))),
         CrewModel, CrewIter);
      if CrewIter = Null_Iter then
         return;
      end if;
      CrewIndex := Natural'Value(To_String(Get_Path(CrewModel, CrewIter))) + 1;
      Clear(SkillsList);
      for I in Skills_List.Iterate loop
         Cost := Gint(TrainCost(CrewIndex, SkillsData_Container.To_Index(I)));
         if Cost > 0 then
            Append(SkillsList, SkillsIter);
            Set(SkillsList, SkillsIter, 0, To_String(Skills_List(I).Name));
            Set(SkillsList, SkillsIter, 1, Cost);
            Set
              (SkillsList, SkillsIter, 2,
               "Related statistic: " &
               To_String(Attributes_List(Skills_List(I).Attribute).Name) &
               ". " & To_String(Skills_List(I).Description));
         end if;
      end loop;
      MoneyIndex2 := FindItem(PlayerShip.Cargo, MoneyIndex);
      if MoneyIndex2 > 0 then
         Set_Label
           (Gtk_Label(Get_Object(Object, "lblmoney")),
            "You have" & Natural'Image(PlayerShip.Cargo(MoneyIndex2).Amount) &
            " " & To_String(MoneyName) & ".");
      else
         Set_Label
           (Gtk_Label(Get_Object(Object, "lblmoney")),
            "You don't have any " & To_String(MoneyName) &
            " to pay for learning.");
      end if;
      if N_Children(SkillsList, Null_Iter) > 0 then
         Set_Cursor
           (Gtk_Tree_View(Get_Object(Builder, "treeskills")),
            Gtk_Tree_Path_New_From_String("0"), null, False);
         Set_Sensitive(Gtk_Widget(Get_Object(Object, "btntrain")), True);
      else
         Set_Sensitive(Gtk_Widget(Get_Object(Object, "btntrain")), False);
      end if;
   end ShowTrainInfo;

   -- ****if* Bases.SchoolUI/TrainSelectedSkill
   -- FUNCTION
   -- Train selected skill
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure TrainSelectedSkill(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      SkillsIter: Gtk_Tree_Iter;
      SkillsModel: Gtk_Tree_Model;
      SkillName: Unbounded_String;
      SkillIndex: Positive;
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treeskills"))),
         SkillsModel, SkillsIter);
      if SkillsIter = Null_Iter then
         return;
      end if;
      SkillName := To_Unbounded_String(Get_String(SkillsModel, SkillsIter, 0));
      for I in Skills_List.Iterate loop
         if Skills_List(I).Name = SkillName then
            SkillIndex := SkillsData_Container.To_Index(I);
            exit;
         end if;
      end loop;
      TrainSkill(CrewIndex, SkillIndex);
      UpdateMessages;
      ShowTrainInfo(Object);
   exception
      when Trade_No_Money =>
         ShowDialog
           ("You don't have any " & To_String(MoneyName) &
            " to pay for learning.");
      when Trade_Not_Enough_Money =>
         ShowDialog
           ("You don't have enough " & To_String(MoneyName) &
            " to pay for learning this skill.");
      when Trade_Cant_Train =>
         ShowDialog("You can't train this skill any more.");
   end TrainSelectedSkill;

   procedure CreateBasesSchoolUI(NewBuilder: Gtkada_Builder) is
   begin
      Builder := NewBuilder;
      Register_Handler(Builder, "Show_Train_Info", ShowTrainInfo'Access);
      Register_Handler
        (Builder, "Train_Selected_Skill", TrainSelectedSkill'Access);
   end CreateBasesSchoolUI;

   procedure ShowSchoolUI is
      CrewIter: Gtk_Tree_Iter;
      CrewList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "crewlist"));
   begin
      Clear(CrewList);
      for Member of PlayerShip.Crew loop
         Append(CrewList, CrewIter);
         Set(CrewList, CrewIter, 0, To_String(Member.Name));
      end loop;
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")), "school");
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treecrew")),
         Gtk_Tree_Path_New_From_String("0"),
         Gtk_Tree_View_Column(Get_Object(Builder, "columnnames2")), False);
      UpdateMessages;
   end ShowSchoolUI;

end Bases.SchoolUI;
