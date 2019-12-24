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

with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Bin; use Gtk.Bin;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Store; use Gtk.Tree_Store;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Glib.Object; use Glib.Object;
with Gdk.Types; use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Game; use Game;
with Goals; use Goals;
with MainMenu; use MainMenu;
with Statistics.UI; use Statistics.UI;
with Utils; use Utils;

package body Goals.UI is

   -- ****iv* Goals.UI/Builder
   -- FUNCTION
   -- Gtkada_Builder used for creating UI
   -- SOURCE
   Builder: Gtkada_Builder;
   -- ****

   -- ****iv* Goals.UI/FromMainMenu
   -- FUNCTION
   -- If true, UI was called from main menu. Default true
   -- SOURCE
   FromMainMenu: Boolean := True;
   -- ****

   -- ****if* Goals.UI/HideGoals
   -- FUNCTION
   -- Hide goals UI instead of destroy it
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   function HideGoals
     (User_Data: access Gtkada_Builder_Record'Class) return Boolean is
   -- ****
   begin
      return Hide_On_Delete(Gtk_Widget(Get_Object(User_Data, "goalswindow")));
   end HideGoals;

   procedure ShowGoalsMenu(InMainMenu: Boolean := True) is
   begin
      FromMainMenu := InMainMenu;
      Show_All(Gtk_Widget(Get_Object(Builder, "goalswindow")));
   end ShowGoalsMenu;

   -- ****if* Goals.UI/GoalSelected
   -- FUNCTION
   -- Enable or disable "Select goal" button
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure GoalSelected(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      Iter: Gtk_Tree_Iter;
      GoalsView: constant Gtk_Tree_View :=
        Gtk_Tree_View(Get_Object(Object, "treegoals"));
      GoalsModel: Gtk_Tree_Model;
      Button: constant Gtk_Widget :=
        Get_Child
          (Gtk_Box
             (Get_Child
                (Gtk_Box
                   (Gtk.Bin.Get_Child
                      (Gtk_Bin(Get_Object(Builder, "goalswindow")))),
                 1)),
           0);
   begin
      Get_Selected(Get_Selection(GoalsView), GoalsModel, Iter);
      if Get_String(GoalsModel, Iter, 0) /= "Random" and
        Get_Int(GoalsModel, Iter, 1) = 0 then
         Set_Sensitive(Button, False);
      else
         Set_Sensitive(Button, True);
      end if;
   end GoalSelected;

   -- ****if* Goals.UI/SelectGoal
   -- FUNCTION
   -- Set currently selected goal as a current game goal
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked. Unused
   -- SOURCE
   procedure SelectGoal(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
      Iter: Gtk_Tree_Iter;
      GoalsView: constant Gtk_Tree_View :=
        Gtk_Tree_View(Get_Object(Builder, "treegoals"));
      GoalsModel: Gtk_Tree_Model;
   begin
      Get_Selected(Get_Selection(GoalsView), GoalsModel, Iter);
      if Get_String(GoalsModel, Iter, 0) /= "Random" and
        Get_Int(GoalsModel, Iter, 1) = 0 then
         if Row_Expanded(GoalsView, Get_Path(GoalsModel, Iter)) then
            if not Collapse_Row(GoalsView, Get_Path(GoalsModel, Iter)) then
               return;
            end if;
         else
            if not Expand_Row(GoalsView, Get_Path(GoalsModel, Iter), True) then
               return;
            end if;
         end if;
         return;
      end if;
      if Get_String(GoalsModel, Iter, 0) = "Random" then
         ClearCurrentGoal;
         if FromMainMenu then
            UpdateGoalButton("Random");
         else
            CurrentGoal :=
              Goals_List
                (GetRandom(Goals_List.First_Index, Goals_List.Last_Index));
            UpdateGoalsButton(GoalText(0));
         end if;
      else
         CurrentGoal := Goals_List(Positive(Get_Int(GoalsModel, Iter, 1)));
         if FromMainMenu then
            UpdateGoalButton(GoalText(0));
         else
            UpdateGoalsButton(GoalText(0));
         end if;
      end if;
      Hide(Gtk_Widget(Get_Object(Builder, "goalswindow")));
   end SelectGoal;

   -- ****if* Goals.UI/CloseGoals
   -- FUNCTION
   -- Hide goals UI
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked. Unused
   -- SOURCE
   procedure CloseGoals(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      Hide(Gtk_Widget(Get_Object(Builder, "goalswindow")));
   end CloseGoals;

   procedure SelectGoalTemp(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
   begin
      SelectGoal(null);
   end SelectGoalTemp;

   procedure CreateGoalsMenu is
      Error: aliased GError;
      GoalsList: Gtk_Tree_Store;
      CategoryIter: Gtk_Tree_Iter;
      Accelerators: constant Gtk_Accel_Group := Gtk_Accel_Group_New;
      procedure AddGoals(CategoryName: String; GType: GoalTypes) is
         GoalsIter: Gtk_Tree_Iter;
      begin
         Append(GoalsList, CategoryIter, Null_Iter);
         Set(GoalsList, CategoryIter, 0, CategoryName);
         Set(GoalsList, CategoryIter, 1, 0);
         for I in Goals_List.Iterate loop
            if Goals_List(I).GType = GType then
               Append(GoalsList, GoalsIter, CategoryIter);
               Set
                 (GoalsList, GoalsIter, 0,
                  GoalText(Goals_Container.To_Index(I)));
               Set(GoalsList, GoalsIter, 1, Gint(Goals_Container.To_Index(I)));
            end if;
         end loop;
         if not Has_Child(GoalsList, CategoryIter) then
            Remove(GoalsList, CategoryIter);
            CategoryIter := Nth_Child(GoalsList, Null_Iter, 0);
         end if;
      end AddGoals;
      procedure AddButton
        (Text: String; Subprogram: Cb_Gtk_Button_Void;
         Key: Gdk_Key_Type := 0) is
         Button: constant Gtk_Button := Gtk_Button_New_With_Mnemonic(Text);
      begin
         On_Clicked(Button, Subprogram);
         if Key /= 0 then
            Add_Accelerator
              (Button, "clicked", Accelerators, Key, 0, Accel_Visible);
         end if;
         Pack_Start
           (Gtk_Box
              (Get_Child
                 (Gtk_Box
                    (Gtk.Bin.Get_Child
                       (Gtk_Bin(Get_Object(Builder, "goalswindow")))),
                  1)),
            Button, False);
      end AddButton;
   begin
      if Builder /= null then
         return;
      end if;
      Gtk_New(Builder);
      if Add_From_File
          (Builder,
           To_String(DataDirectory) & "ui" & Dir_Separator & "goals.glade",
           Error'Access) =
        Guint(0) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      Register_Handler(Builder, "Hide_Goals", HideGoals'Access);
      Register_Handler(Builder, "Goal_Selected", GoalSelected'Access);
      Register_Handler(Builder, "Select_Goal", SelectGoalTemp'Access);
      Do_Connect(Builder);
      GoalsList := Gtk_Tree_Store(Get_Object(Builder, "goalslist"));
      Append(GoalsList, CategoryIter, Null_Iter);
      Set(GoalsList, CategoryIter, 0, "Random");
      Set(GoalsList, CategoryIter, 1, 0);
      AddGoals("Gain max reputation in bases", REPUTATION);
      AddGoals("Destroy enemy ships", DESTROY);
      AddGoals("Discover map", DISCOVER);
      AddGoals("Visit bases", VISIT);
      AddGoals("Craft items", CRAFT);
      AddGoals("Finish missions", MISSION);
      AddGoals("Kill enemies in melee combat", KILL);
      AddButton("_Select goal", SelectGoal'Access);
      AddButton("Close [Escape]", CloseGoals'Access, GDK_Escape);
      Add_Accel_Group
        (Gtk_Window(Get_Object(Builder, "goalswindow")), Accelerators);
   end CreateGoalsMenu;

end Goals.UI;
