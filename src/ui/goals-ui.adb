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

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Bin; use Gtk.Bin;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Cell_Area_Box; use Gtk.Cell_Area_Box;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Store; use Gtk.Tree_Store;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Game; use Game;
with Goals; use Goals;
with MainMenu; use MainMenu;
with Statistics.UI; use Statistics.UI;
with Utils; use Utils;

package body Goals.UI is

   -- ****iv* Goals.UI/FromMainMenu
   -- FUNCTION
   -- If true, UI was called from main menu. Default true
   -- SOURCE
   FromMainMenu: Boolean := True;
   -- ****

   -- ****iv* Goals.UI/GoalsWindow
   -- FUNCTION
   -- Main Gtk_Window for selecting a goal.
   -- SOURCE
   GoalsWindow: Gtk_Window;
   -- ****

   -- ****if* Goals.UI/HideGoals
   -- FUNCTION
   -- Hide goals UI instead of destroy it
   -- PARAMETERS
   -- Self  - Gtk_Widget which triggered this event (GoalsWindow)
   -- Event - Gdk_Event structure with data about the event. Unused.
   -- SOURCE
   function HideGoals
     (Self: access Gtk_Widget_Record'Class; Event: Gdk_Event) return Boolean is
      pragma Unreferenced(Event);
      -- ****
   begin
      return Hide_On_Delete(Self);
   end HideGoals;

   procedure ShowGoalsMenu(InMainMenu: Boolean := True) is
   begin
      FromMainMenu := InMainMenu;
      Show_All(GoalsWindow);
   end ShowGoalsMenu;

   -- ****if* Goals.UI/GoalSelected
   -- FUNCTION
   -- Enable or disable "Select goal" button
   -- PARAMETERS
   -- Self - Gtk_Tree_View with goals
   -- SOURCE
   procedure GoalSelected(Self: access Gtk_Tree_View_Record'Class) is
      -- ****
      Iter: Gtk_Tree_Iter;
      GoalsModel: Gtk_Tree_Model;
      Button: constant Gtk_Widget :=
        Get_Child
          (Gtk_Box
             (Get_Child(Gtk_Box(Gtk.Bin.Get_Child(Gtk_Bin(GoalsWindow))), 1)),
           0);
   begin
      Get_Selected(Get_Selection(Self), GoalsModel, Iter);
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
        Gtk_Tree_View
          (Get_Child
             (Gtk_Bin
                (Get_Child
                   (Gtk_Box(Gtk.Bin.Get_Child(Gtk_Bin(GoalsWindow))), 0))));
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
      Hide(GoalsWindow);
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
      Hide(GoalsWindow);
   end CloseGoals;

   -- ****if* Goals.UI/SelectGoalView
   -- FUNCTION
   -- Set currently selected goal as a current game goal after selecting it
   -- in Goals Tree_View.
   -- PARAMETERS
   -- Self   - Gtk_Tree_View with list of goals. Unused.
   -- Path   - Gtk_Tree_Path to row which was clicked. Unused.
   -- Column - Gtk_Tree_View_Column which was clicked. Unused.
   -- SOURCE
   procedure SelectGoalView
     (Self: access Gtk_Tree_View_Record'Class; Path: Gtk_Tree_Path;
      Column: not null access Gtk_Tree_View_Column_Record'Class) is
      pragma Unreferenced(Self, Path, Column);
      -- ****
   begin
      SelectGoal(null);
   end SelectGoalView;

   procedure CreateGoalsMenu is
      GoalsList: constant Gtk_Tree_Store :=
        Gtk_Tree_Store_Newv((GType_String, GType_Uint));
      CategoryIter: Gtk_Tree_Iter;
      Accelerators: constant Gtk_Accel_Group := Gtk_Accel_Group_New;
      ButtonBox, MainBox: constant Gtk_Vbox := Gtk_Vbox_New;
      GoalsView: constant Gtk_Tree_View := Gtk_Tree_View_New;
      Column: Gtk_Tree_View_Column;
      Area: Gtk_Cell_Area_Box;
      Renderer: Gtk_Cell_Renderer_Text;
      GoalsScroll: constant Gtk_Scrolled_Window := Gtk_Scrolled_Window_New;
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
         Pack_Start(ButtonBox, Button, False);
      end AddButton;
   begin
      if GoalsWindow /= null then
         return;
      end if;
      Set_Policy(GoalsScroll, Policy_Never, Policy_Automatic);
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
      Set_Model(GoalsView, +(GoalsList));
      Gtk.Cell_Renderer_Text.Gtk_New(Renderer);
      Area := Gtk_Cell_Area_Box_New;
      Pack_Start(Area, Renderer, True);
      Add_Attribute(Area, Renderer, "text", 0);
      Column := Gtk_Tree_View_Column_New_With_Area(Area);
      if Append_Column(GoalsView, Column) /= 1 then
         raise Program_Error with "Can't add column to goals view";
      end if;
      On_Row_Activated(GoalsView, SelectGoalView'Access);
      On_Cursor_Changed(GoalsView, GoalSelected'Access);
      Add(GoalsScroll, GoalsView);
      Pack_Start(MainBox, GoalsScroll);
      AddButton("_Select goal", SelectGoal'Access);
      AddButton("Close [Escape]", CloseGoals'Access, GDK_Escape);
      Pack_Start(MainBox, ButtonBox, False);
      GoalsWindow := Gtk_Window_New;
      On_Delete_Event(Gtk_Widget(GoalsWindow), HideGoals'Access);
      Set_Default_Size(GoalsWindow, -1, 600);
      Set_Position(GoalsWindow, Win_Pos_Center);
      Set_Title(GoalsWindow, "Steam Sky - Select Goal");
      if not Set_Icon_From_File
          (GoalsWindow,
           To_String(DataDirectory) & Dir_Separator & "ui" & Dir_Separator &
           "images" & Dir_Separator & "icon.png") then
         raise Program_Error with "Can't set icon for the goals window";
      end if;
      Add(GoalsWindow, MainBox);
      Add_Accel_Group(GoalsWindow, Accelerators);
   end CreateGoalsMenu;

end Goals.UI;
