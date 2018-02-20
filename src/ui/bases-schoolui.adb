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

with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Label; use Gtk.Label;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Window; use Gtk.Window;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Ships; use Ships;
with Bases.Trade; use Bases.Trade;
with Utils.UI; use Utils.UI;
with Trades; use Trades;

package body Bases.SchoolUI is

   Builder: Gtkada_Builder;
   CrewIndex, SkillIndex: Positive;

   function HideSchool
     (Object: access Gtkada_Builder_Record'Class) return Boolean is
   begin
      Hide(Gtk_Widget(Get_Object(Object, "schoolwindow")));
      CreateSkyMap;
      return True;
   end HideSchool;

   procedure HideLastMessage(Object: access Gtkada_Builder_Record'Class) is
   begin
      Hide(Gtk_Widget(Get_Object(Object, "infolastmessage")));
      LastMessage := Null_Unbounded_String;
   end HideLastMessage;

   procedure ShowLastMessage is
   begin
      if LastMessage = Null_Unbounded_String then
         HideLastMessage(Builder);
      else
         Set_Text
           (Gtk_Label(Get_Object(Builder, "lbllastmessage")),
            To_String(LastMessage));
         Show_All(Gtk_Widget(Get_Object(Builder, "infolastmessage")));
         LastMessage := Null_Unbounded_String;
      end if;
   end ShowLastMessage;

   procedure ShowTrainInfo(Object: access Gtkada_Builder_Record'Class) is
      CrewIter, SkillsIter: Gtk_Tree_Iter;
      CrewModel: Gtk_Tree_Model;
      SkillsList: Gtk_List_Store;
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treecrew"))),
         CrewModel,
         CrewIter);
      if CrewIter = Null_Iter then
         return;
      end if;
      CrewIndex := Natural'Value(To_String(Get_Path(CrewModel, CrewIter))) + 1;
      SkillsList := Gtk_List_Store(Get_Object(Builder, "skillslist"));
      Clear(SkillsList);
      for I in Skills_List.Iterate loop
         Append(SkillsList, SkillsIter);
         Set(SkillsList, SkillsIter, 0, To_String(Skills_List(I).Name));
         Set
           (SkillsList,
            SkillsIter,
            1,
            Gint(TrainCost(CrewIndex, SkillsData_Container.To_Index(I))));
      end loop;
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treeskills")),
         Gtk_Tree_Path_New_From_String("0"),
         Gtk_Tree_View_Column(Get_Object(Builder, "columnskill")),
         False);
   end ShowTrainInfo;

   procedure SetTrainButton(Object: access Gtkada_Builder_Record'Class) is
      SkillsIter: Gtk_Tree_Iter;
      SkillsModel: Gtk_Tree_Model;
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treeskills"))),
         SkillsModel,
         SkillsIter);
      if SkillsIter = Null_Iter then
         return;
      end if;
      SkillIndex :=
        Natural'Value(To_String(Get_Path(SkillsModel, SkillsIter))) + 1;
      if Get_Int(SkillsModel, SkillsIter, 1) = 0 then
         Set_Sensitive(Gtk_Widget(Get_Object(Object, "btntrain")), False);
      else
         Set_Sensitive(Gtk_Widget(Get_Object(Object, "btntrain")), True);
      end if;
   end SetTrainButton;

   procedure TrainSelectedSkill(Object: access Gtkada_Builder_Record'Class) is
      ParentWindow: constant Gtk_Window :=
        Gtk_Window(Get_Object(Object, "schoolwindow"));
   begin
      TrainSkill(CrewIndex, SkillIndex);
      ShowLastMessage;
      ShowTrainInfo(Object);
      SetTrainButton(Object);
   exception
      when Trade_No_Money =>
         ShowDialog
           ("You don't have any " &
            To_String(MoneyName) &
            " to pay for learning.",
            ParentWindow);
      when Trade_Not_Enough_Money =>
         ShowDialog
           ("You don't have enough " &
            To_String(MoneyName) &
            " to pay for learning this skill.",
            ParentWindow);
      when Trade_Cant_Train =>
         ShowDialog("You can't train this skill any more.", ParentWindow);
   end TrainSelectedSkill;

   procedure CreateBasesSchoolUI is
      Error: aliased GError;
   begin
      if Builder /= null then
         return;
      end if;
      Gtk_New(Builder);
      if Add_From_File
          (Builder,
           To_String(DataDirectory) &
           "ui" &
           Dir_Separator &
           "bases-school.glade",
           Error'Access) =
        Guint(0) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      Register_Handler(Builder, "Hide_School", HideSchool'Access);
      Register_Handler(Builder, "Hide_Last_Message", HideLastMessage'Access);
      Register_Handler(Builder, "Show_Train_Info", ShowTrainInfo'Access);
      Register_Handler(Builder, "Set_Train_Button", SetTrainButton'Access);
      Register_Handler
        (Builder,
         "Train_Selected_Skill",
         TrainSelectedSkill'Access);
      Do_Connect(Builder);
   end CreateBasesSchoolUI;

   procedure ShowSchoolUI is
      CrewIter: Gtk_Tree_Iter;
      CrewList: Gtk_List_Store;
   begin
      CrewList := Gtk_List_Store(Get_Object(Builder, "crewlist"));
      Clear(CrewList);
      for Member of PlayerShip.Crew loop
         Append(CrewList, CrewIter);
         Set(CrewList, CrewIter, 0, To_String(Member.Name));
      end loop;
      Show_All(Gtk_Widget(Get_Object(Builder, "schoolwindow")));
      ShowLastMessage;
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treecrew")),
         Gtk_Tree_Path_New_From_String("0"),
         Gtk_Tree_View_Column(Get_Object(Builder, "columnname")),
         False);
   end ShowSchoolUI;

end Bases.SchoolUI;
