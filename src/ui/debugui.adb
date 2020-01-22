--    Copyright 2019-2020 Bartek thindil Jasicki
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
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Bin; use Gtk.Bin;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Cell_Area_Box; use Gtk.Cell_Area_Box;
with Gtk.Cell_Renderer_Spin; use Gtk.Cell_Renderer_Spin;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.Entry_Completion; use Gtk.Entry_Completion;
with Gtk.Enums; use Gtk.Enums;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Grid; use Gtk.Grid;
with Gtk.Label; use Gtk.Label;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Spin_Button; use Gtk.Spin_Button;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Stack_Switcher; use Gtk.Stack_Switcher;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Widget; use Gtk.Widget;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Glib.Object; use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Bases; use Bases;
with Crew; use Crew;
with Events; use Events;
with Factions; use Factions;
with Game; use Game;
with Game.SaveLoad; use Game.SaveLoad;
with Items; use Items;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with ShipModules; use ShipModules;
with BasesTypes; use BasesTypes;

package body DebugUI is

   -- ****iv* DebugUI/Builder
   -- FUNCTION
   -- Gtkada_Builder used for creating UI
   -- SOURCE
   Builder: Gtkada_Builder;
   -- ****

   -- ****iv* DebugUI/Setting
   -- FUNCTION
   -- If true, UI is currently in setting state
   -- SOURCE
   Setting: Boolean;
   -- ****

   -- ****iv* DebugUI/Stack
   -- FUNCTION
   -- Gtk_Stack with most of UI
   -- SOURCE
   Stack: Gtk_Stack;
   -- ****

   -- ****if* DebugUI/MoveShip
   -- FUNCTION
   -- Move the player ship to selected location on the map
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked
   -- SOURCE
   procedure MoveShip(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      PlayerShip.SkyX :=
        Integer(Get_Value(Gtk_Adjustment(Get_Object(Builder, "adjshipx"))));
      PlayerShip.SkyY :=
        Integer(Get_Value(Gtk_Adjustment(Get_Object(Builder, "adjshipy"))));
      ShowSkyMap;
   end MoveShip;

   -- ****if* DebugUI/UpdateShip
   -- FUNCTION
   -- Update the player ship data
   -- PARAMETERS
   -- Self - Gtk_Widget which was mapped
   -- SOURCE
   procedure UpdateShip(Self: access Gtk_Widget_Record'Class) is
      -- ****
      CrewList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "crewlist"));
      CrewIter: Gtk_Tree_Iter;
      ComboBox: constant Gtk_Combo_Box_Text :=
        Gtk_Combo_Box_Text(Get_Child_At(Gtk_Grid(Self), 1, 1));
   begin
      Setting := True;
      Clear(CrewList);
      CrewIter := Get_Iter_First(CrewList);
      for I in PlayerShip.Crew.Iterate loop
         Append(CrewList, CrewIter);
         Set(CrewList, CrewIter, 0, To_String(PlayerShip.Crew(I).Name));
         Set
           (CrewList, CrewIter, 1, Positive'Image(Crew_Container.To_Index(I)));
      end loop;
      Remove_All(ComboBox);
      for I in PlayerShip.Modules.Iterate loop
         Append
           (ComboBox, Positive'Image(Modules_Container.To_Index(I)),
            To_String(PlayerShip.Modules(I).Name));
      end loop;
      Setting := False;
      Set_Active(ComboBox, 0);
      Set_Value
        (Gtk_Adjustment(Get_Object(Builder, "adjshipx")),
         Gdouble(PlayerShip.SkyX));
      Set_Value
        (Gtk_Adjustment(Get_Object(Builder, "adjshipy")),
         Gdouble(PlayerShip.SkyY));
   end UpdateShip;

   -- ****if* DebugUI/UpdateCrew
   -- FUNCTION
   -- Update the player ship crew
   -- PARAMETERS
   -- Self - Gtk_Widget which was mapped
   -- SOURCE
   procedure UpdateCrew(Self: access Gtk_Widget_Record'Class) is
      -- ****
      ComboBox: constant Gtk_Combo_Box :=
        Gtk_Combo_Box
          (Get_Child(Gtk_Box(Get_Child(Gtk_Box(Get_Parent(Self)), 0)), 1));
   begin
      Set_Active(ComboBox, 0);
   end UpdateCrew;

   -- ****if* DebugUI/SetMemberStats
   -- FUNCTION
   -- Show statistics, skills, etc for selected the player ship crew member
   -- PARAMETERS
   -- Self - Gtk_Combo_Box with crew members names
   -- SOURCE
   procedure SetMemberStats(Self: access Gtk_Combo_Box_Record'Class) is
      -- ****
      Member: Member_Data;
      Iter: Gtk_Tree_Iter;
      List: Gtk_List_Store := Gtk_List_Store(Get_Object(Builder, "statslist"));
      KnowSkills: Positive_Container.Vector;
      CrewBox: constant Gtk_Box := Gtk_Box(Get_Child_By_Name(Stack, "page1"));
      AddSkillBox: constant Gtk_Box :=
        Gtk_Box
          (Get_Child
             (Gtk_Box(Get_Child(Gtk_Box(Get_Child(CrewBox, 1)), 2)), 1));
      ComboBox: constant Gtk_Combo_Box_Text :=
        Gtk_Combo_Box_Text(Get_Child(AddSkillBox, 1));
   begin
      if Setting then
         return;
      end if;
      Member := PlayerShip.Crew(Positive'Value(Get_Active_Id(Self)));
      Set_Value
        (Gtk_Adjustment(Get_Object(Builder, "adjhealth")),
         Gdouble(Member.Health));
      Set_Value
        (Gtk_Adjustment(Get_Object(Builder, "adjthirst")),
         Gdouble(Member.Thirst));
      Set_Value
        (Gtk_Adjustment(Get_Object(Builder, "adjhunger")),
         Gdouble(Member.Hunger));
      Set_Value
        (Gtk_Adjustment(Get_Object(Builder, "adjtired")),
         Gdouble(Member.Tired));
      Set_Value
        (Gtk_Adjustment(Get_Object(Builder, "adjmorale")),
         Gdouble(Member.Morale(1)));
      Set_Value
        (Gtk_Adjustment(Get_Object(Builder, "adjloyalty")),
         Gdouble(Member.Loyalty));
      Clear(List);
      for I in Member.Attributes.Iterate loop
         Append(List, Iter);
         Set
           (List, Iter, 0,
            To_String(Attributes_List(Attributes_Container.To_Index(I)).Name));
         Set(List, Iter, 1, Gint(Attributes_Container.To_Index(I)));
         Set(List, Iter, 2, Gint(Member.Attributes(I)(1)));
      end loop;
      List := Gtk_List_Store(Get_Object(Builder, "skillslist"));
      Clear(List);
      for I in Member.Skills.Iterate loop
         Append(List, Iter);
         Set(List, Iter, 0, To_String(Skills_List(Member.Skills(I)(1)).Name));
         Set(List, Iter, 1, Gint(Skills_Container.To_Index(I)));
         Set(List, Iter, 2, Gint(Member.Skills(I)(2)));
         KnowSkills.Append(Member.Skills(I)(1));
      end loop;
      Remove_All(ComboBox);
      for I in Skills_List.Iterate loop
         if not KnowSkills.Contains(SkillsData_Container.To_Index(I)) then
            Append
              (ComboBox,
               Integer'Image(SkillsData_Container.To_Index(I) * (-1)),
               To_String(Skills_List(I).Name));
         end if;
      end loop;
      if N_Children(Get_Model(ComboBox)) > 0 then
         Show_All(AddSkillBox);
         Set_Active(ComboBox, 0);
      else
         Hide(AddSkillBox);
      end if;
   end SetMemberStats;

   -- ****if* DebugUI/UpdateAttribute
   -- FUNCTION
   -- Update the attribute of selected the player ship crew member
   -- PARAMETERS
   -- Model - Gtk_Tree_Model with attributes values
   -- Path  - Gtk_Tree_Path in Model to selected attribute (unused)
   -- Iter  - Gtk_Tree_Iter in Model to selected attribute
   -- RESULT
   -- This function always returns false
   -- SOURCE
   function UpdateAttribute
     (Model: Gtk_Tree_Model; Path: Gtk_Tree_Path; Iter: Gtk_Tree_Iter)
      return Boolean is
      pragma Unreferenced(Path);
      -- ****
      CrewBox: constant Gtk_Box := Gtk_Box(Get_Child_By_Name(Stack, "page1"));
      MemberIndex: constant Positive :=
        Positive'Value
          (Get_Active_Id
             (Gtk_Combo_Box(Get_Child(Gtk_Box(Get_Child(CrewBox, 0)), 1))));
   begin
      PlayerShip.Crew(MemberIndex).Attributes
        (Positive(Get_Int(Model, Iter, 1)))
        (1) :=
        Positive(Get_Int(Model, Iter, 2));
      return False;
   end UpdateAttribute;

   -- ****if* DebugUI/UpdateSkill
   -- FUNCTION
   -- Update the skill of selected player ship crew member
   -- PARAMETERS
   -- Model - Gtk_Tree_Model with skills values
   -- Path  - Gtk_Tree_Path in Model to selected skill (unused)
   -- Iter  - Gtk_Tree_Iter in Model to selected skill
   -- RESULT
   -- This function always returns false
   -- SOURCE
   function UpdateSkill
     (Model: Gtk_Tree_Model; Path: Gtk_Tree_Path; Iter: Gtk_Tree_Iter)
      return Boolean is
      pragma Unreferenced(Path);
      -- ****
      CrewBox: constant Gtk_Box := Gtk_Box(Get_Child_By_Name(Stack, "page1"));
      MemberIndex: constant Positive :=
        Positive'Value
          (Get_Active_Id
             (Gtk_Combo_Box(Get_Child(Gtk_Box(Get_Child(CrewBox, 0)), 1))));
      SkillIndex: constant Integer := Integer(Get_Int(Model, Iter, 1));
   begin
      if SkillIndex > 0 then
         PlayerShip.Crew(MemberIndex).Skills(SkillIndex)(2) :=
           Positive(Get_Int(Model, Iter, 2));
      else
         PlayerShip.Crew(MemberIndex).Skills.Append
           ((abs (SkillIndex), Positive(Get_Int(Model, Iter, 2)), 0));
         Set
           (-(Model), Iter, 1,
            Gint(PlayerShip.Crew(MemberIndex).Skills.Last_Index));
      end if;
      return False;
   end UpdateSkill;

   -- ****if* DebugUI/UpdateMember
   -- FUNCTION
   -- Update selected the player ship crew member statistics, skill, etc
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked
   -- SOURCE
   procedure UpdateMember(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
      CrewBox: constant Gtk_Box := Gtk_Box(Get_Child_By_Name(Stack, "page1"));
      MemberIndex: constant Positive :=
        Positive'Value
          (Get_Active_Id
             (Gtk_Combo_Box(Get_Child(Gtk_Box(Get_Child(CrewBox, 0)), 1))));
   begin
      PlayerShip.Crew(MemberIndex).Health :=
        Natural(Get_Value(Gtk_Adjustment(Get_Object(Builder, "adjhealth"))));
      PlayerShip.Crew(MemberIndex).Thirst :=
        Natural(Get_Value(Gtk_Adjustment(Get_Object(Builder, "adjthirst"))));
      PlayerShip.Crew(MemberIndex).Hunger :=
        Natural(Get_Value(Gtk_Adjustment(Get_Object(Builder, "adjhunger"))));
      PlayerShip.Crew(MemberIndex).Tired :=
        Natural(Get_Value(Gtk_Adjustment(Get_Object(Builder, "adjtired"))));
      PlayerShip.Crew(MemberIndex).Morale(1) :=
        Natural(Get_Value(Gtk_Adjustment(Get_Object(Builder, "adjmorale"))));
      PlayerShip.Crew(MemberIndex).Loyalty :=
        Natural(Get_Value(Gtk_Adjustment(Get_Object(Builder, "adjloyalty"))));
      Foreach
        (Gtk_List_Store(Get_Object(Builder, "statslist")),
         UpdateAttribute'Access);
      Foreach
        (Gtk_List_Store(Get_Object(Builder, "skillslist")),
         UpdateSkill'Access);
   end UpdateMember;

   -- ****if* DebugUI/UpdateCargoInfo
   -- FUNCTION
   -- Update the player ship cargo list
   -- PARAMETERS
   -- Self - Gtk_Widget which will be mapped
   -- SOURCE
   procedure UpdateCargoInfo(Self: access Gtk_Widget_Record'Class) is
      -- ****
      ComboBox: constant Gtk_Combo_Box_Text :=
        Gtk_Combo_Box_Text(Get_Child_At(Gtk_Grid(Self), 1, 1));
   begin
      Setting := True;
      Remove_All(ComboBox);
      for I in PlayerShip.Cargo.Iterate loop
         Append
           (ComboBox, Positive'Image(Inventory_Container.To_Index(I)),
            GetItemName(PlayerShip.Cargo(I), False));
      end loop;
      Setting := False;
      Set_Active(ComboBox, 0);
   end UpdateCargoInfo;

   -- ****if* DebugUI/ShowBaseInfo
   -- FUNCTION
   -- Update information about selected base
   -- PARAMETERS
   -- Self - Gtk_GEntry in which button Enter was pressed
   -- SOURCE
   procedure ShowBaseInfo(Self: access Gtk_Entry_Record'Class) is
      -- ****
      BaseGrid: constant Gtk_Grid := Gtk_Grid(Get_Parent(Self));
      BaseName: constant Unbounded_String :=
        To_Unbounded_String(Get_Text(Self));
   begin
      for I in SkyBases'Range loop
         if SkyBases(I).Name = BaseName then
            Set_Active
              (Gtk_Combo_Box_Text(Get_Child_At(BaseGrid, 1, 3)),
               Bases_Size'Pos(SkyBases(I).Size));
            if not Set_Active_Id
                (Gtk_Combo_Box_Text(Get_Child_At(BaseGrid, 1, 2)),
                 To_String(SkyBases(I).Owner)) then
               raise Program_Error with "Can't set active Id for base owner";
            end if;
            if not Set_Active_Id
                (Gtk_Combo_Box_Text(Get_Child_At(BaseGrid, 1, 1)),
                 To_String(SkyBases(I).BaseType)) then
               raise Program_Error with "Can't set active Id for base type";
            end if;
            Set_Value
              (Gtk_Adjustment(Get_Object(Builder, "adjpopulation")),
               Gdouble(SkyBases(I).Population));
            Set_Value
              (Gtk_Adjustment(Get_Object(Builder, "adjreputation")),
               Gdouble(SkyBases(I).Reputation(1)));
            if SkyBases(I).Cargo.Length > 0 then
               Set_Value
                 (Gtk_Adjustment(Get_Object(Builder, "adjbasemoney")),
                  Gdouble(SkyBases(I).Cargo(1).Amount));
            else
               Set_Value
                 (Gtk_Adjustment(Get_Object(Builder, "adjbasemoney")), 0.0);
            end if;
            exit;
         end if;
      end loop;
   end ShowBaseInfo;

   -- ****if* DebugUI/ResetWorldUI
   -- FUNCTION
   -- Update world editing data
   -- SOURCE
   procedure ResetWorldUI is
      -- ****
      EventsCombo: constant Gtk_Widget :=
        Get_Child
          (Gtk_Box(Get_Child(Gtk_Box(Get_Child_By_Name(Stack, "page4")), 0)),
           2);
      EventGrid: constant Gtk_Grid :=
        Gtk_Grid
          (Get_Child
             (Gtk_Box
                (Get_Child(Gtk_Box(Get_Child_By_Name(Stack, "page4")), 1)),
              0));
   begin
      Set_Text
        (Gtk_GEntry
           (Get_Child_At
              (Gtk_Grid
                 (Get_Child
                    (Gtk_Box
                       (Get_Child
                          (Gtk_Box(Get_Child_By_Name(Stack, "page4")), 0)),
                     0)),
               1, 0)),
         "");
      Set_Value(Gtk_Adjustment(Get_Object(Builder, "adjnpcshipx")), 1.0);
      Set_Value(Gtk_Adjustment(Get_Object(Builder, "adjnpcshipy")), 1.0);
      Set_Text(Gtk_GEntry(Get_Child_At(EventGrid, 1, 0)), "");
      Set_Text(Gtk_GEntry(Get_Child_At(EventGrid, 1, 2)), "");
      if Events_List.Length = 0 then
         Hide(EventsCombo);
         Hide
           (Get_Child
              (Gtk_Box
                 (Get_Child(Gtk_Box(Get_Child_By_Name(Stack, "page4")), 0)),
               3));
      else
         Show_All(EventsCombo);
         Show_All
           (Get_Child
              (Gtk_Box
                 (Get_Child(Gtk_Box(Get_Child_By_Name(Stack, "page4")), 0)),
               3));
         Remove_All(Gtk_Combo_Box_Text(EventsCombo));
         for I in Events_List.Iterate loop
            case Events_List(I).EType is
               when EnemyShip =>
                  Append
                    (Gtk_Combo_Box_Text(EventsCombo),
                     Positive'Image(Events_Container.To_Index(I)),
                     "Enemy ship: " &
                     To_String
                       (ProtoShips_List(Events_List(I).ShipIndex).Name));
               when AttackOnBase =>
                  Append
                    (Gtk_Combo_Box_Text(EventsCombo),
                     Positive'Image(Events_Container.To_Index(I)),
                     "Attack on base: " &
                     To_String
                       (ProtoShips_List(Events_List(I).ShipIndex).Name));
               when Disease =>
                  Append
                    (Gtk_Combo_Box_Text(EventsCombo),
                     Positive'Image(Events_Container.To_Index(I)),
                     "Disease in base: " &
                     To_String
                       (SkyBases
                          (SkyMap(Events_List(I).SkyX, Events_List(I).SkyY)
                             .BaseIndex)
                          .Name));
               when DoublePrice =>
                  Append
                    (Gtk_Combo_Box_Text(EventsCombo),
                     Positive'Image(Events_Container.To_Index(I)),
                     "Double price in base: " &
                     To_String
                       (SkyBases
                          (SkyMap(Events_List(I).SkyX, Events_List(I).SkyY)
                             .BaseIndex)
                          .Name));
               when FullDocks =>
                  Append
                    (Gtk_Combo_Box_Text(EventsCombo),
                     Positive'Image(Events_Container.To_Index(I)),
                     "Full docks in base: " &
                     To_String
                       (SkyBases
                          (SkyMap(Events_List(I).SkyX, Events_List(I).SkyY)
                             .BaseIndex)
                          .Name));
               when EnemyPatrol =>
                  Append
                    (Gtk_Combo_Box_Text(EventsCombo),
                     Positive'Image(Events_Container.To_Index(I)),
                     "Enemy patrol: " &
                     To_String
                       (ProtoShips_List(Events_List(I).ShipIndex).Name));
               when Trader =>
                  Append
                    (Gtk_Combo_Box_Text(EventsCombo),
                     Positive'Image(Events_Container.To_Index(I)),
                     "Trader: " &
                     To_String
                       (ProtoShips_List(Events_List(I).ShipIndex).Name));
               when FriendlyShip =>
                  Append
                    (Gtk_Combo_Box_Text(EventsCombo),
                     Positive'Image(Events_Container.To_Index(I)),
                     "Friendly ship: " &
                     To_String
                       (ProtoShips_List(Events_List(I).ShipIndex).Name));
               when others =>
                  null;
            end case;
         end loop;
         Set_Active(Gtk_Combo_Box_Text(EventsCombo), 0);
      end if;
   end ResetWorldUI;

   -- ****if* DebugUI/RefreshUI
   -- FUNCTION
   -- Refresh whole debug UI
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure RefreshUI(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
      -- ****
   begin
      UpdateShip(Get_Child(Gtk_Box(Get_Child_By_Name(Stack, "page0")), 0));
      UpdateCrew(Get_Child(Gtk_Box(Get_Child_By_Name(Stack, "page1")), 1));
      UpdateCargoInfo(Get_Child_By_Name(Stack, "page2"));
      ShowBaseInfo
        (Gtk_GEntry
           (Get_Child_At
              (Gtk_Grid
                 (Get_Child(Gtk_Box(Get_Child_By_Name(Stack, "page3")), 0)),
               1, 0)));
      ResetWorldUI;
   end RefreshUI;

   -- ****if* DebugUI/ChangeStatLevel
   -- FUNCTION
   -- Change level of selected attribute of the player ship crew member
   -- PARAMETERS
   -- Self     - Cell in which text will be changed (unused)
   -- Path     - Text with path to the selected cell
   -- New_Text - New text which will be set for selected cell
   -- SOURCE
   procedure ChangeStatLevel
     (Self: access Gtk_Cell_Renderer_Text_Record'Class; Path: UTF8_String;
      New_Text: UTF8_String) is
      pragma Unreferenced(Self);
      -- ****
      StatsList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "statslist"));
      NewValue: Gint;
   begin
      NewValue := Gint'Value(New_Text);
      if NewValue < 1 then
         NewValue := 1;
      elsif NewValue > 50 then
         NewValue := 50;
      end if;
      Set(StatsList, Get_Iter_From_String(StatsList, Path), 2, NewValue);
   exception
      when Constraint_Error =>
         null;
   end ChangeStatLevel;

   -- ****if* DebugUI/ChangeSkillLevel
   -- FUNCTION
   -- Change level of selected skill of the player ship crew member
   -- PARAMETERS
   -- Self     - Cell in which text will be changed (unused)
   -- Path     - Text with path to the selected cell
   -- New_Text - New text which will be set for selected cell
   -- SOURCE
   procedure ChangeSkillLevel
     (Self: access Gtk_Cell_Renderer_Text_Record'Class; Path: UTF8_String;
      New_Text: UTF8_String) is
      pragma Unreferenced(Self);
      -- ****
      SkillsList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "skillslist"));
      NewValue: Gint;
   begin
      NewValue := Gint'Value(New_Text);
      if NewValue < 1 then
         NewValue := 1;
      elsif NewValue > 100 then
         NewValue := 100;
      end if;
      Set(SkillsList, Get_Iter_From_String(SkillsList, Path), 2, NewValue);
   exception
      when Constraint_Error =>
         null;
   end ChangeSkillLevel;

   -- ****if* DebugUI/AddSkill
   -- FUNCTION
   -- Add new skill to selected the player ship crew member
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked
   -- SOURCE
   procedure AddSkill(Self: access Gtk_Button_Record'Class) is
      -- ****
      SkillsIter: Gtk_Tree_Iter;
      SkillsList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "skillslist"));
      SkillBox: constant Gtk_Box := Gtk_Box(Get_Parent(Self));
      ComboBox: constant Gtk_Combo_Box_Text :=
        Gtk_Combo_Box_Text(Get_Child(SkillBox, 1));
   begin
      Append(SkillsList, SkillsIter);
      Set(SkillsList, SkillsIter, 0, Get_Active_Text(ComboBox));
      Set(SkillsList, SkillsIter, 1, Gint'Value(Get_Active_Id(ComboBox)));
      Set(SkillsList, SkillsIter, 2, 1);
      Remove(ComboBox, Get_Active(ComboBox));
      if N_Children(Get_Model(ComboBox)) > 0 then
         Set_Active(ComboBox, 0);
      else
         Hide(SkillBox);
      end if;
   end AddSkill;

   -- ****if* DebugUI/SetCargoAmount
   -- FUNCTION
   -- Set cargo amount value for selected item in the player ship cargo
   -- PARAMETERS
   -- Self - Gtk_Combo_Box which value was changed.
   -- SOURCE
   procedure SetCargoAmount(Self: access Gtk_Combo_Box_Record'Class) is
      -- ****
      Item: InventoryData;
   begin
      if Setting then
         return;
      end if;
      Item :=
        PlayerShip.Cargo
          (Positive'Value(Get_Active_Id(Gtk_Combo_Box_Text(Self))));
      Set_Value
        (Gtk_Adjustment(Get_Object(Builder, "adjupdatecargo")),
         Gdouble(Item.Amount));
   end SetCargoAmount;

   -- ****if* DebugUI/AddCargo
   -- FUNCTION
   -- Add new item to the player ship cargo
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked.
   -- SOURCE
   procedure AddCargo(Self: access Gtk_Button_Record'Class) is
      -- ****
      ItemIndex: Unbounded_String := Null_Unbounded_String;
      ItemName: constant Unbounded_String :=
        To_Unbounded_String
          (Get_Text
             (Gtk_GEntry(Get_Child_At(Gtk_Grid(Get_Parent(Self)), 1, 0))));
   begin
      for I in Items_List.Iterate loop
         if Items_List(I).Name = ItemName then
            ItemIndex := Objects_Container.Key(I);
            exit;
         end if;
      end loop;
      if ItemIndex = Null_Unbounded_String then
         return;
      end if;
      UpdateCargo
        (PlayerShip, ItemIndex,
         Positive
           (Get_Value(Gtk_Adjustment(Get_Object(Builder, "adjaddcargo")))));
      UpdateCargoInfo(Get_Parent(Self));
   end AddCargo;

   -- ****if* DebugUI/UpdateShipCargo
   -- FUNCTION
   -- Updated existing item in the player ship cargo
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked.
   -- SOURCE
   procedure UpdateShipCargo(Self: access Gtk_Button_Record'Class) is
      -- ****
      CargoIndex: constant Positive :=
        Positive'Value
          (Get_Active_Id
             (Gtk_Combo_Box_Text
                (Get_Child_At(Gtk_Grid(Get_Parent(Self)), 1, 1))));
   begin
      UpdateCargo
        (Ship => PlayerShip,
         Amount =>
           (Integer
              (Get_Value
                 (Gtk_Adjustment(Get_Object(Builder, "adjupdatecargo")))) -
            PlayerShip.Cargo(CargoIndex).Amount),
         CargoIndex => CargoIndex);
   end UpdateShipCargo;

   -- ****if* DebugUI/UpdateBase
   -- FUNCTION
   -- Update selected base with new data
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked.
   -- SOURCE
   procedure UpdateBase(Self: access Gtk_Button_Record'Class) is
      -- ****
      BaseGrid: constant Gtk_Grid :=
        Gtk_Grid(Get_Child(Gtk_Box(Get_Parent(Self)), 0));
      BaseName: constant Unbounded_String :=
        To_Unbounded_String
          (Get_Text(Gtk_GEntry(Get_Child_At(BaseGrid, 1, 0))));
   begin
      for SkyBase of SkyBases loop
         if SkyBase.Name = BaseName then
            SkyBase.BaseType :=
              To_Unbounded_String
                (Get_Active_Id
                   (Gtk_Combo_Box_Text(Get_Child_At(BaseGrid, 1, 1))));
            SkyBase.Owner :=
              To_Unbounded_String
                (Get_Active_Id
                   (Gtk_Combo_Box_Text(Get_Child_At(BaseGrid, 1, 2))));
            SkyBase.Size :=
              Bases_Size'Val
                (Integer
                   (Get_Active
                      (Gtk_Combo_Box_Text(Get_Child_At(BaseGrid, 1, 3)))));
            if SkyBase.Cargo.Length > 0 then
               SkyBase.Cargo(1).Amount :=
                 Natural
                   (Get_Value
                      (Gtk_Adjustment(Get_Object(Builder, "adjbasemoney"))));
            else
               SkyBase.Cargo.Append
                 (New_Item =>
                    (ProtoIndex => MoneyIndex,
                     Amount =>
                       Natural
                         (Get_Value
                            (Gtk_Adjustment
                               (Get_Object(Builder, "adjbasemoney")))),
                     Durability => 100, Price => 0));
            end if;
            SkyBase.Population :=
              Natural
                (Get_Value
                   (Gtk_Adjustment(Get_Object(Builder, "adjpopulation"))));
            SkyBase.Reputation(1) :=
              Integer
                (Get_Value
                   (Gtk_Adjustment(Get_Object(Builder, "adjreputation"))));
         end if;
      end loop;
   end UpdateBase;

   -- ****if* DebugUI/AddShip
   -- FUNCTION
   -- Add new NPC ship on map
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked.
   -- SOURCE
   procedure AddShip(Self: access Gtk_Button_Record'Class) is
      -- ****
      ShipName: constant Unbounded_String :=
        To_Unbounded_String
          (Get_Text
             (Gtk_GEntry
                (Get_Child_At
                   (Gtk_Grid(Get_Child(Gtk_Box(Get_Parent(Self)), 0)), 1,
                    0))));
      NpcShipX: constant Positive :=
        Positive
          (Get_Value(Gtk_Adjustment(Get_Object(Builder, "adjnpcshipx"))));
      NpcShipY: constant Positive :=
        Positive
          (Get_Value(Gtk_Adjustment(Get_Object(Builder, "adjnpcshipy"))));
   begin
      for I in ProtoShips_List.Iterate loop
         if ProtoShips_List(I).Name = ShipName then
            if Traders.Contains(ProtoShips_Container.Key(I)) then
               Events_List.Append
                 (New_Item =>
                    (Trader, NpcShipX, NpcShipY,
                     Positive
                       (Get_Value
                          (Gtk_Adjustment(Get_Object(Builder, "adjminutes")))),
                     ProtoShips_Container.Key(I)));
            elsif FriendlyShips.Contains(ProtoShips_Container.Key(I)) then
               Events_List.Append
                 (New_Item =>
                    (FriendlyShip, NpcShipX, NpcShipY,
                     Positive
                       (Get_Value
                          (Gtk_Adjustment(Get_Object(Builder, "adjminutes")))),
                     ProtoShips_Container.Key(I)));
            else
               Events_List.Append
                 (New_Item =>
                    (EnemyShip, NpcShipX, NpcShipY,
                     Positive
                       (Get_Value
                          (Gtk_Adjustment(Get_Object(Builder, "adjminutes")))),
                     ProtoShips_Container.Key(I)));
            end if;
            SkyMap(NpcShipX, NpcShipY).EventIndex := Events_List.Last_Index;
            ResetWorldUI;
            return;
         end if;
      end loop;
   end AddShip;

   -- ****if* DebugUI/ShowItemEvent
   -- FUNCTION
   -- Show or hide info about item used by selected event
   -- PARAMETERS
   -- Self - Gtk_Combo_Box with selected event
   -- SOURCE
   procedure ShowItemEvent(Self: access Gtk_Combo_Box_Record'Class) is
   -- ****
   begin
      if Get_Active(Self) = 1 then
         Show_All(Get_Child_At(Gtk_Grid(Get_Parent(Self)), 0, 2));
         Show_All(Get_Child_At(Gtk_Grid(Get_Parent(Self)), 1, 2));
      else
         Hide(Get_Child_At(Gtk_Grid(Get_Parent(Self)), 0, 2));
         Hide(Get_Child_At(Gtk_Grid(Get_Parent(Self)), 1, 2));
      end if;
   end ShowItemEvent;

   -- ****if* DebugUI/AddEvent
   -- FUNCTION
   -- Add new event to the game
   -- PARAMETERS
   -- Self - Gtk_Button clicked.
   -- SOURCE
   procedure AddEvent(Self: access Gtk_Button_Record'Class) is
      -- ****
      ParentGrid: constant Gtk_Grid :=
        Gtk_Grid(Get_Child(Gtk_Box(Get_Parent(Self)), 0));
      BaseName: constant Unbounded_String :=
        To_Unbounded_String
          (Get_Text(Gtk_GEntry(Get_Child_At(ParentGrid, 1, 0))));
      ItemName: constant Unbounded_String :=
        To_Unbounded_String
          (Get_Text(Gtk_GEntry(Get_Child_At(ParentGrid, 1, 2))));
      EventAdded: Boolean := True;
   begin
      for Base of SkyBases loop
         if Base.Name = BaseName then
            case Get_Active
              (Gtk_Combo_Box_Text(Get_Child_At(ParentGrid, 1, 1))) is
               when 0 =>
                  Events_List.Append
                    (New_Item =>
                       (Disease, Base.SkyX, Base.SkyY,
                        Positive
                          (Get_Value
                             (Gtk_Adjustment
                                (Get_Object(Builder, "adjminutesbase")))),
                        1));
               when 1 =>
                  EventAdded := False;
                  for I in Items_List.Iterate loop
                     if Items_List(I).Name = ItemName then
                        Events_List.Append
                          (New_Item =>
                             (DoublePrice, Base.SkyX, Base.SkyY,
                              Positive
                                (Get_Value
                                   (Gtk_Adjustment
                                      (Get_Object
                                         (Builder, "adjminutesbase")))),
                              Objects_Container.Key(I)));
                        EventAdded := True;
                        exit;
                     end if;
                  end loop;
               when 2 =>
                  Events_List.Append
                    (New_Item =>
                       (FullDocks, Base.SkyX, Base.SkyY,
                        Positive
                          (Get_Value
                             (Gtk_Adjustment
                                (Get_Object(Builder, "adjminutesbase")))),
                        1));
               when others =>
                  null;
            end case;
            if EventAdded then
               SkyMap(Base.SkyX, Base.SkyY).EventIndex :=
                 Events_List.Last_Index;
               ResetWorldUI;
            end if;
            exit;
         end if;
      end loop;
   end AddEvent;

   -- ****if* DebugUI/DeleteEvent
   -- FUNCTION
   -- Delete selected event from the game
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked.
   -- SOURCE
   procedure DeleteEvent(Self: access Gtk_Button_Record'Class) is
   -- ****
   begin
      DeleteEvent
        (Positive'Value
           (Get_Active_Id
              (Gtk_Combo_Box_Text(Get_Child(Gtk_Box(Get_Parent(Self)), 2)))));
      ResetWorldUI;
   end DeleteEvent;

   -- ****if* DebugUI/Save_Game
   -- FUNCTION
   -- Save current state of the game to disk
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked. Unused
   -- SOURCE
   procedure Save_Game(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      SaveGame(True);
   end Save_Game;

   -- ****if* DebugUI/SetModuleStats
   -- FUNCTION
   -- Set information about selected the player ship module
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure SetModuleStats(Self: access Gtk_Combo_Box_Record'Class) is
      -- ****
      Module: ModuleData;
      ShipGrid: constant Gtk_Grid := Gtk_Grid(Get_Parent(Self));
   begin
      if Setting then
         return;
      end if;
      Module := PlayerShip.Modules(Positive'Value(Get_Active_Id(Self)));
      Set_Text
        (Gtk_GEntry(Get_Child_At(ShipGrid, 1, 2)),
         To_String(Modules_List(Module.ProtoIndex).Name));
      Set_Value
        (Gtk_Adjustment(Get_Object(Builder, "adjmoduleweight")),
         Gdouble(Module.Weight));
      Set_Value
        (Gtk_Adjustment(Get_Object(Builder, "adjmoduledur")),
         Gdouble(Module.Durability));
      Set_Value
        (Gtk_Adjustment(Get_Object(Builder, "adjmodulemaxdur")),
         Gdouble(Module.MaxDurability));
      Set_Value
        (Gtk_Adjustment(Get_Object(Builder, "adjmoduleupgrade")),
         Gdouble(Module.UpgradeProgress));
   end SetModuleStats;

   -- ****if* DebugUI/UpdateModule
   -- FUNCTION
   -- Update selected the player ship module with new data
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked.
   -- SOURCE
   procedure UpdateModule(Self: access Gtk_Button_Record'Class) is
      -- ****
      ShipGrid: constant Gtk_Grid :=
        Gtk_Grid(Get_Child(Gtk_Box(Get_Parent(Self)), 0));
      ModuleIndex: constant Positive :=
        Positive'Value
          (Get_Active_Id(Gtk_Combo_Box(Get_Child_At(ShipGrid, 1, 1))));
      ProtoName: constant Unbounded_String :=
        To_Unbounded_String
          (Get_Text(Gtk_GEntry(Get_Child_At(ShipGrid, 1, 2))));
   begin
      for I in Modules_List.Iterate loop
         if Modules_List(I).Name = ProtoName then
            PlayerShip.Modules(ModuleIndex).ProtoIndex :=
              BaseModules_Container.Key(I);
            exit;
         end if;
      end loop;
      PlayerShip.Modules(ModuleIndex).Weight :=
        Natural
          (Get_Value(Gtk_Adjustment(Get_Object(Builder, "adjmoduleweight"))));
      PlayerShip.Modules(ModuleIndex).Durability :=
        Natural
          (Get_Value(Gtk_Adjustment(Get_Object(Builder, "adjmoduledur"))));
      PlayerShip.Modules(ModuleIndex).MaxDurability :=
        Natural
          (Get_Value(Gtk_Adjustment(Get_Object(Builder, "adjmodulemaxdur"))));
      PlayerShip.Modules(ModuleIndex).UpgradeProgress :=
        Natural
          (Get_Value(Gtk_Adjustment(Get_Object(Builder, "adjmoduleupgrade"))));
   end UpdateModule;

   -- ****if* DebugUI/ShowBasesTypes
   -- FUNCTION
   -- Update bases types list after selecting faction
   -- PARAMETERS
   -- Self - Gtk_Combo_Box in which value was changed
   -- SOURCE
   procedure ShowBasesTypes(Self: access Gtk_Combo_Box_Record'Class) is
      -- ****
      ComboBox: constant Gtk_Combo_Box_Text :=
        Gtk_Combo_Box_Text(Get_Child_At(Gtk_Grid(Get_Parent(Self)), 1, 1));
      FactionIndex: constant Unbounded_String :=
        To_Unbounded_String(Get_Active_Id(Self));
   begin
      Remove_All(ComboBox);
      for I in BasesTypes_List.Iterate loop
         if Factions_List(FactionIndex).BasesTypes.Contains
             (BasesTypes_Container.Key(I)) then
            Append
              (ComboBox, To_String(BasesTypes_Container.Key(I)),
               To_String(BasesTypes_List(I).Name));
         end if;
      end loop;
      Set_Active(ComboBox, 0);
   end ShowBasesTypes;

   procedure CreateDebugUI is
      Error: aliased GError;
      WorldBox: constant Gtk_Hbox := Gtk_Hbox_New;
      Label: Gtk_Label;
      ComboBox: Gtk_Combo_Box_Text;
      SpinButton: Gtk_Spin_Button;
      Button: Gtk_Button;
   begin
      if Builder /= null then
         return;
      end if;
      Gtk_New(Builder);
      if Add_From_File
          (Builder,
           To_String(DataDirectory) & "ui" & Dir_Separator & "debug.glade",
           Error'Access) =
        Guint(0) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      Register_Handler(Builder, "Refresh_UI", RefreshUI'Access);
      Do_Connect(Builder);
      declare
         List: Gtk_List_Store :=
           Gtk_List_Store(Get_Object(Builder, "itemslist"));
         Iter: Gtk_Tree_Iter;
      begin
         for Item of Items_List loop
            Append(List, Iter);
            Set(List, Iter, 0, To_String(Item.Name));
         end loop;
         List := Gtk_List_Store(Get_Object(Builder, "baseslist"));
         for I in SkyBases'Range loop
            Append(List, Iter);
            Set(List, Iter, 0, To_String(SkyBases(I).Name));
         end loop;
         List := Gtk_List_Store(Get_Object(Builder, "shipslist"));
         for Ship of ProtoShips_List loop
            Append(List, Iter);
            Set(List, Iter, 0, To_String(Ship.Name));
         end loop;
         List := Gtk_List_Store(Get_Object(Builder, "moduleslist"));
         for Module of Modules_List loop
            Append(List, Iter);
            Set(List, Iter, 0, To_String(Module.Name));
         end loop;
      end;
      Button := Gtk_Button_New_With_Mnemonic("_Save game");
      On_Clicked(Button, Save_Game'Access);
      Pack_Start(Gtk_Box(Get_Object(Builder, "switchbox")), Button, False);
      Stack := Gtk_Stack_New;
      Set_Stack(Gtk_Stack_Switcher(Get_Object(Builder, "stackswitch")), Stack);
      Pack_Start
        (Gtk_Box(Get_Child(Gtk_Bin(Get_Object(Builder, "debugwindow")))),
         Stack);
      declare
         ShipGrid: constant Gtk_Grid := Gtk_Grid_New;
         Labels: constant array(1 .. 6) of Unbounded_String :=
           (To_Unbounded_String("Module"), To_Unbounded_String("Prototype"),
            To_Unbounded_String("Weight"), To_Unbounded_String("Durability"),
            To_Unbounded_String("Max Durability"),
            To_Unbounded_String("Upgrade Progress"));
         ShipEntry: constant Gtk_Entry := Gtk_Entry_New;
         MoveBox: constant Gtk_Hbox := Gtk_Hbox_New;
         ShipBox: constant Gtk_Vbox := Gtk_Vbox_New;
      begin
         On_Map(ShipGrid, UpdateShip'Access);
         Button := Gtk_Button_New_With_Mnemonic("_Move ship");
         Set_Tooltip_Text
           (Button, "Move player ship to selected location on map");
         On_Clicked(Button, MoveShip'Access);
         Attach(ShipGrid, Button, 0, 0);
         Pack_Start(MoveBox, Gtk_Label_New("X:"), False);
         SpinButton :=
           Gtk_Spin_Button_New
             (Gtk_Adjustment(Get_Object(Builder, "adjshipx")), 0.0);
         Set_Tooltip_Text
           (SpinButton,
            "New X coordinate on map for the player ship. Value between 1 and 1024.");
         Pack_Start(MoveBox, SpinButton, False);
         Pack_Start(MoveBox, Gtk_Label_New("Y:"), False);
         SpinButton :=
           Gtk_Spin_Button_New
             (Gtk_Adjustment(Get_Object(Builder, "adjshipy")), 0.0);
         Set_Tooltip_Text
           (SpinButton,
            "New Y coordinate on map for the player ship. Value between 1 and 1024.");
         Pack_Start(MoveBox, SpinButton, False);
         Attach(ShipGrid, MoveBox, 1, 0);
         for I in Labels'Range loop
            Attach(ShipGrid, Gtk_Label_New(To_String(Labels(I))), 0, Gint(I));
         end loop;
         ComboBox := Gtk_Combo_Box_Text_New;
         Set_Tooltip_Text(ComboBox, "Select module to edit.");
         On_Changed(Gtk_Combo_Box(ComboBox), SetModuleStats'Access);
         Attach(ShipGrid, ComboBox, 1, 1);
         Set_Completion
           (ShipEntry,
            Gtk_Entry_Completion(Get_Object(Builder, "modulescompletion")));
         Set_Tooltip_Text
           (ShipEntry,
            "Select prototype module for this module. Start typying to select name of proto module. WARNING: If you select other type of module than previous (for example from Cabin to Workshop) the game can crash.");
         Attach(ShipGrid, ShipEntry, 1, 2);
         SpinButton :=
           Gtk_Spin_Button_New
             (Gtk_Adjustment(Get_Object(Builder, "adjmoduleweight")), 0.0);
         Set_Tooltip_Text(SpinButton, "Set new weight of the module.");
         Attach(ShipGrid, SpinButton, 1, 3);
         SpinButton :=
           Gtk_Spin_Button_New
             (Gtk_Adjustment(Get_Object(Builder, "adjmoduledur")), 0.0);
         Set_Tooltip_Text(SpinButton, "Set new durability of the module.");
         Attach(ShipGrid, SpinButton, 1, 4);
         SpinButton :=
           Gtk_Spin_Button_New
             (Gtk_Adjustment(Get_Object(Builder, "adjmodulemaxdur")), 0.0);
         Set_Tooltip_Text
           (SpinButton, "Set new maximum durablity for the module.");
         Attach(ShipGrid, SpinButton, 1, 5);
         SpinButton :=
           Gtk_Spin_Button_New
             (Gtk_Adjustment(Get_Object(Builder, "adjmoduleupgrade")), 0.0);
         Set_Tooltip_Text
           (SpinButton,
            "Set upgrade progress for the module. No effect unless any ugrade is set for selected module.");
         Attach(ShipGrid, SpinButton, 1, 6);
         Pack_Start(ShipBox, ShipGrid, False);
         Button := Gtk_Button_New_With_Mnemonic("_Change");
         Set_Tooltip_Text(Button, "Commit changes to the ship module.");
         On_Clicked(Button, UpdateModule'Access);
         Pack_Start(ShipBox, Button, False);
         Add_Titled(Stack, ShipBox, "page0", "Ship");
      end;
      declare
         CrewBox: constant Gtk_Vbox := Gtk_Vbox_New;
         SkillsBox: constant Gtk_Vbox := Gtk_Vbox_New;
         HBox: Gtk_Hbox;
         Scrolled: Gtk_Scrolled_Window;
         View: Gtk_Tree_View;
         Column: Gtk_Tree_View_Column;
         Area: Gtk_Cell_Area_Box := Gtk_Cell_Area_Box_New;
         Renderer: Gtk_Cell_Renderer_Text;
         RendererSpin: Gtk_Cell_Renderer_Spin;
         MemberGrid: constant Gtk_Grid := Gtk_Grid_New;
         Labels: constant array(0 .. 5) of Unbounded_String :=
           (To_Unbounded_String("Health"), To_Unbounded_String("Thirst"),
            To_Unbounded_String("Hunger"), To_Unbounded_String("Tired"),
            To_Unbounded_String("Morale"), To_Unbounded_String("Loyalty"));
         LowerLabel: Unbounded_String;
         MemberBox: constant Gtk_Hbox := Gtk_Hbox_New;
         MemberComboBox: constant Gtk_Combo_Box :=
           Gtk_Combo_Box_New_With_Model
             (+(Gtk_List_Store(Get_Object(Builder, "crewlist"))));
      begin
         HBox := Gtk_Hbox_New;
         Label := Gtk_Label_New("Member");
         Pack_Start(HBox, Label, False);
         Renderer := Gtk_Cell_Renderer_Text_New;
         Pack_Start(MemberComboBox, Renderer, True);
         Add_Attribute(MemberComboBox, Renderer, "text", 0);
         Set_Id_Column(MemberComboBox, 1);
         On_Changed(MemberComboBox, SetMemberStats'Access);
         Pack_Start(HBox, MemberComboBox, False);
         Pack_Start(CrewBox, HBox, False);
         for I in Labels'Range loop
            Attach
              (MemberGrid, Gtk_Label_New(To_String(Labels(I))), 0, Gint(I));
            LowerLabel :=
              To_Unbounded_String
                (To_Lower(Element(Labels(I), 1)) &
                 Slice(Labels(I), 2, Length(Labels(I))));
            SpinButton :=
              Gtk_Spin_Button_New
                (Gtk_Adjustment
                   (Get_Object(Builder, "adj" & To_String(LowerLabel))),
                 0.0);
            Set_Tooltip_Text
              (SpinButton,
               "Level of " & To_String(LowerLabel) &
               " of crew member. Value between 0 and 100.");
            Attach(MemberGrid, SpinButton, 1, Gint(I));
         end loop;
         On_Map(MemberBox, UpdateCrew'Access);
         Pack_Start(MemberBox, MemberGrid, False);
         Scrolled := Gtk_Scrolled_Window_New;
         View :=
           Gtk_Tree_View_New_With_Model
             (+(Gtk_List_Store(Get_Object(Builder, "statslist"))));
         Set_Tooltip_Text
           (View,
            "To change level of selected attribute, double left click on level column. Values between 1 and 50.");
         Renderer := Gtk_Cell_Renderer_Text_New;
         Pack_Start(Area, Renderer, True);
         Add_Attribute(Area, Renderer, "text", 0);
         Column := Gtk_Tree_View_Column_New_With_Area(Area);
         Set_Title(Column, "Name");
         Set_Clickable(Column, True);
         Set_Sort_Indicator(Column, True);
         Set_Sort_Column_Id(Column, 0);
         if Append_Column(View, Column) /= 1 then
            raise Program_Error
              with "Can't add column name to member attributes view.";
         end if;
         Area := Gtk_Cell_Area_Box_New;
         RendererSpin := Gtk_Cell_Renderer_Spin_New;
         Set_Property
           (RendererSpin, Gtk.Cell_Renderer_Spin.Adjustment_Property,
            Get_Object(Builder, "adjstats"));
         Set_Property
           (RendererSpin, Gtk.Cell_Renderer_Text.Editable_Property, True);
         Pack_Start(Area, RendererSpin, True);
         Add_Attribute(Area, RendererSpin, "text", 2);
         Column := Gtk_Tree_View_Column_New_With_Area(Area);
         Set_Title(Column, "Level");
         Set_Clickable(Column, True);
         Set_Sort_Indicator(Column, True);
         Set_Sort_Column_Id(Column, 2);
         On_Edited(RendererSpin, ChangeStatLevel'Access);
         if Append_Column(View, Column) /= 2 then
            raise Program_Error
              with "Can't add column level to member stats view.";
         end if;
         Set_Policy(Scrolled, Policy_Never, Policy_Automatic);
         Add(Scrolled, View);
         Pack_Start(MemberBox, Scrolled, False);
         Scrolled := Gtk_Scrolled_Window_New;
         View :=
           Gtk_Tree_View_New_With_Model
             (+(Gtk_List_Store(Get_Object(Builder, "skillslist"))));
         Set_Tooltip_Text
           (View,
            "To change level of selected skill, double left click on level column. Values between 1 and 100.");
         Area := Gtk_Cell_Area_Box_New;
         Pack_Start(Area, Renderer, True);
         Add_Attribute(Area, Renderer, "text", 0);
         Column := Gtk_Tree_View_Column_New_With_Area(Area);
         Set_Title(Column, "Name");
         Set_Clickable(Column, True);
         Set_Sort_Indicator(Column, True);
         Set_Sort_Column_Id(Column, 0);
         if Append_Column(View, Column) /= 1 then
            raise Program_Error
              with "Can't add column name to member skills view.";
         end if;
         Area := Gtk_Cell_Area_Box_New;
         Set_Property
           (RendererSpin, Gtk.Cell_Renderer_Spin.Adjustment_Property,
            Get_Object(Builder, "adjskills"));
         Set_Property
           (RendererSpin, Gtk.Cell_Renderer_Text.Editable_Property, True);
         Pack_Start(Area, RendererSpin, True);
         Add_Attribute(Area, RendererSpin, "text", 2);
         Column := Gtk_Tree_View_Column_New_With_Area(Area);
         Set_Title(Column, "Level");
         Set_Clickable(Column, True);
         Set_Sort_Indicator(Column, True);
         Set_Sort_Column_Id(Column, 2);
         On_Edited(RendererSpin, ChangeSkillLevel'Access);
         if Append_Column(View, Column) /= 2 then
            raise Program_Error
              with "Can't add column level to member skills view.";
         end if;
         Set_Policy(Scrolled, Policy_Never, Policy_Automatic);
         Add(Scrolled, View);
         Pack_Start(SkillsBox, Scrolled);
         HBox := Gtk_Hbox_New;
         Button := Gtk_Button_New_With_Mnemonic("_Add");
         Set_Tooltip_Text(Button, "Add new skill to selected crew member");
         On_Clicked(Button, AddSkill'Access);
         Pack_Start(HBox, Button, False);
         ComboBox := Gtk_Combo_Box_Text_New;
         Set_Tooltip_Text(ComboBox, "Select skill to add.");
         Pack_Start(HBox, ComboBox);
         Pack_Start(SkillsBox, HBox, False);
         Pack_Start(MemberBox, SkillsBox);
         Pack_Start(CrewBox, MemberBox, False);
         Button := Gtk_Button_New_With_Mnemonic("_Change");
         Set_Tooltip_Text(Button, "Commit changes to the crew member.");
         On_Clicked(Button, UpdateMember'Access);
         Pack_Start(CrewBox, Button, False);
         Add_Titled(Stack, CrewBox, "page1", "Crew");
      end;
      declare
         CargoGrid: constant Gtk_Grid := Gtk_Grid_New;
         CargoEntry: constant Gtk_Entry := Gtk_Entry_New;
      begin
         On_Map(CargoGrid, UpdateCargoInfo'Access);
         Button := Gtk_Button_New_With_Mnemonic("_Add");
         Set_Tooltip_Text
           (Button, "Add selected item to the player ship cargo.");
         On_Clicked(Button, AddCargo'Access);
         Attach(CargoGrid, Button, 0, 0);
         Set_Completion
           (CargoEntry,
            Gtk_Entry_Completion(Get_Object(Builder, "cargocompletion")));
         Set_Tooltip_Text
           (CargoEntry,
            "Start typing here a name of the item to select it from the list.");
         Attach(CargoGrid, CargoEntry, 1, 0);
         Label := Gtk_Label_New("Amount:");
         Attach(CargoGrid, Label, 2, 0);
         SpinButton :=
           Gtk_Spin_Button_New
             (Gtk_Adjustment(Get_Object(Builder, "adjaddcargo")), 0.0);
         Set_Tooltip_Text
           (SpinButton,
            "Add that amount of selected item to the player ship cargo.");
         Attach(CargoGrid, SpinButton, 3, 0);
         Button := Gtk_Button_New_With_Mnemonic("_Update");
         Set_Tooltip_Text
           (Button,
            "Update amount of selected item in the player ship cargo.");
         On_Clicked(Button, UpdateShipCargo'Access);
         Attach(CargoGrid, Button, 0, 1);
         ComboBox := Gtk_Combo_Box_Text_New;
         Set_Tooltip_Text(ComboBox, "Select item from the player ship cargo.");
         On_Changed(Gtk_Combo_Box(ComboBox), SetCargoAmount'Access);
         Attach(CargoGrid, ComboBox, 1, 1);
         Label := Gtk_Label_New("Amount:");
         Attach(CargoGrid, Label, 2, 1);
         SpinButton :=
           Gtk_Spin_Button_New
             (Gtk_Adjustment(Get_Object(Builder, "adjupdatecargo")), 0.0);
         Set_Tooltip_Text
           (SpinButton,
            "New amount of selected item in the player ship cargo.");
         Attach(CargoGrid, SpinButton, 3, 1);
         Add_Titled(Stack, CargoGrid, "page2", "Cargo");
      end;
      declare
         BaseButton: constant Gtk_Button :=
           Gtk_Button_New_With_Mnemonic("_Update base");
         BaseGrid: constant Gtk_Grid := Gtk_Grid_New;
         BaseEntry: constant Gtk_Entry := Gtk_Entry_New;
         BaseBox: constant Gtk_Vbox := Gtk_Vbox_New;
      begin
         Label := Gtk_Label_New("Base:");
         Attach(BaseGrid, Label, 0, 0);
         Set_Completion
           (BaseEntry,
            Gtk_Entry_Completion(Get_Object(Builder, "basescompletion1")));
         Set_Tooltip_Text
           (BaseEntry,
            "Start typing here a name of the base to select it from the list. Base data will show after pressing key Enter.");
         On_Activate(BaseEntry, ShowBaseInfo'Access);
         Attach(BaseGrid, BaseEntry, 1, 0);
         Label := Gtk_Label_New("Type:");
         Attach(BaseGrid, Label, 0, 1);
         ComboBox := Gtk_Combo_Box_Text_New;
         Set_Tooltip_Text(ComboBox, "Type of selected base");
         Attach(BaseGrid, ComboBox, 1, 1);
         Label := Gtk_Label_New("Owner:");
         Attach(BaseGrid, Label, 0, 2);
         ComboBox := Gtk_Combo_Box_Text_New;
         for I in Factions_List.Iterate loop
            Append
              (ComboBox, To_String(Factions_Container.Key(I)),
               To_String(Factions_List(I).Name));
         end loop;
         Set_Tooltip_Text(ComboBox, "Owner of selected base");
         On_Changed(ComboBox, ShowBasesTypes'Access);
         Attach(BaseGrid, ComboBox, 1, 2);
         Label := Gtk_Label_New("Size:");
         Attach(BaseGrid, Label, 0, 3);
         ComboBox := Gtk_Combo_Box_Text_New;
         for I in Bases_Size loop
            Append_Text
              (ComboBox,
               Bases_Size'Image(I)(1) &
               To_Lower(Bases_Size'Image(I)(2 .. Bases_Size'Image(I)'Last)));
         end loop;
         Set_Tooltip_Text(ComboBox, "Size of selected base");
         Attach(BaseGrid, ComboBox, 1, 3);
         Label := Gtk_Label_New("Population:");
         Attach(BaseGrid, Label, 0, 4);
         SpinButton :=
           Gtk_Spin_Button_New
             (Gtk_Adjustment(Get_Object(Builder, "adjpopulation")), 0.0);
         Set_Tooltip_Text
           (SpinButton, "Amount of people living in selected base.");
         Attach(BaseGrid, SpinButton, 1, 4);
         Label := Gtk_Label_New("Reputation:");
         Attach(BaseGrid, Label, 0, 5);
         SpinButton :=
           Gtk_Spin_Button_New
             (Gtk_Adjustment(Get_Object(Builder, "adjreputation")), 0.0);
         Set_Tooltip_Text
           (SpinButton,
            "Player reputation in selected base. Value between -100 and 100.");
         Attach(BaseGrid, SpinButton, 1, 5);
         Label := Gtk_Label_New("Money:");
         Attach(BaseGrid, Label, 0, 6);
         SpinButton :=
           Gtk_Spin_Button_New
             (Gtk_Adjustment(Get_Object(Builder, "adjbasemoney")), 0.0);
         Set_Tooltip_Text
           (SpinButton, "Amount of money owned by selected base.");
         Attach(BaseGrid, SpinButton, 1, 6);
         Pack_Start(BaseBox, BaseGrid, False);
         Set_Halign(BaseButton, Align_Start);
         On_Clicked(BaseButton, UpdateBase'Access);
         Set_Tooltip_Text(BaseButton, "Update selected base.");
         Pack_Start(BaseBox, BaseButton, False);
         Add_Titled(Stack, BaseBox, "page3", "Bases");
      end;
      declare
         EventGrid: constant Gtk_Grid := Gtk_Grid_New;
         EventButton: constant Gtk_Button :=
           Gtk_Button_New_With_Label("Add ship");
         ShipsEntry: constant Gtk_Entry := Gtk_Entry_New;
         EventsComboBox: constant Gtk_Combo_Box_Text := Gtk_Combo_Box_Text_New;
         DeleteEventButton: constant Gtk_Button :=
           Gtk_Button_New_With_Label("Delete event");
         EventBox: constant Gtk_Vbox := Gtk_Vbox_New;
      begin
         Label := Gtk_Label_New("Ship:");
         Attach(EventGrid, Label, 0, 0);
         Set_Completion
           (ShipsEntry,
            Gtk_Entry_Completion(Get_Object(Builder, "shipscompletion")));
         Set_Tooltip_Text
           (ShipsEntry,
            "Start typing the name of a ship here to select it from the list.");
         Attach(EventGrid, ShipsEntry, 1, 0);
         Label := Gtk_Label_New("X:");
         Attach(EventGrid, Label, 0, 1);
         SpinButton :=
           Gtk_Spin_Button_New
             (Gtk_Adjustment(Get_Object(Builder, "adjnpcshipx")), 0.0);
         Set_Tooltip_Text
           (SpinButton,
            "X coordinates on the map where new NPC ship will be added.");
         Attach(EventGrid, SpinButton, 1, 1);
         Label := Gtk_Label_New("Y:");
         Attach(EventGrid, Label, 0, 2);
         SpinButton :=
           Gtk_Spin_Button_New
             (Gtk_Adjustment(Get_Object(Builder, "adjnpcshipy")), 0.0);
         Set_Tooltip_Text
           (SpinButton,
            "Y coordinates on the map where new NPC ship will be added.");
         Attach(EventGrid, SpinButton, 1, 2);
         Label := Gtk_Label_New("Duration:");
         Attach(EventGrid, Label, 0, 3);
         SpinButton :=
           Gtk_Spin_Button_New
             (Gtk_Adjustment(Get_Object(Builder, "adjminutes")), 0.0);
         Set_Tooltip_Text
           (SpinButton,
            "Amount of minutes, how long selected ship will be available on the map.");
         Attach(EventGrid, SpinButton, 1, 3);
         Pack_Start(EventBox, EventGrid, False);
         Set_Tooltip_Text(EventButton, "Add selected ship to the map.");
         Set_Halign(EventButton, Align_Start);
         On_Clicked(EventButton, AddShip'Access);
         Pack_Start(EventBox, EventButton, False);
         Set_Tooltip_Text(EventsComboBox, "Select event to delete.");
         Set_Halign(EventsComboBox, Align_Start);
         Set_Halign(DeleteEventButton, Align_Start);
         On_Clicked(DeleteEventButton, DeleteEvent'Access);
         Set_Tooltip_Text(DeleteEventButton, "Delete selected event.");
         Pack_Start(EventBox, EventsComboBox, False);
         Pack_Start(EventBox, DeleteEventButton, False);
         Pack_Start(WorldBox, EventBox, False);
      end;
      declare
         EventGrid: constant Gtk_Grid := Gtk_Grid_New;
         EventButton: constant Gtk_Button :=
           Gtk_Button_New_With_Label("Add event");
         Label: Gtk_Label;
         EventEntry: Gtk_Entry;
         EventComboBox: constant Gtk_Combo_Box_Text := Gtk_Combo_Box_Text_New;
         EventBox: constant Gtk_Vbox := Gtk_Vbox_New;
         MinutesEntry: constant Gtk_Spin_Button :=
           Gtk_Spin_Button_New
             (Gtk_Adjustment(Get_Object(Builder, "adjminutesbase")), 0.0);
      begin
         Label := Gtk_Label_New("Base:");
         Attach(EventGrid, Label, 0, 0);
         EventEntry := Gtk_Entry_New;
         Set_Completion
           (EventEntry,
            Gtk_Entry_Completion(Get_Object(Builder, "basescompletion")));
         Set_Tooltip_Text
           (EventEntry,
            "Start typing the name of a base here to select it from the list.");
         Attach(EventGrid, EventEntry, 1, 0);
         Label := Gtk_Label_New("Event:");
         Attach(EventGrid, Label, 0, 1);
         Append(EventComboBox, "3", "Disease");
         Append(EventComboBox, "4", "Double price");
         Append(EventComboBox, "6", "Full docks");
         Set_Active(EventComboBox, 0);
         On_Changed(EventComboBox, ShowItemEvent'Access);
         Set_Tooltip_Text
           (EventComboBox, "Select event to add it to the selected base.");
         Attach(EventGrid, EventComboBox, 1, 1);
         Label := Gtk_Label_New("Item:");
         Attach(EventGrid, Label, 0, 2);
         EventEntry := Gtk_Entry_New;
         Set_Completion
           (EventEntry,
            Gtk_Entry_Completion(Get_Object(Builder, "tradecompletion")));
         Set_Tooltip_Text
           (EventEntry,
            "Start typing the name of an item here to select it from the list.");
         Attach(EventGrid, EventEntry, 1, 2);
         Label := Gtk_Label_New("Duration:");
         Attach(EventGrid, Label, 0, 3);
         Set_Tooltip_Text
           (MinutesEntry,
            "Amount of minutes, how long the selected will be available on the map.");
         Attach(EventGrid, MinutesEntry, 1, 3);
         Pack_Start(EventBox, EventGrid, False);
         On_Clicked(EventButton, AddEvent'Access);
         Set_Halign(EventButton, Align_Start);
         Set_Tooltip_Text(EventButton, "Add the selected event to the map.");
         Pack_Start(EventBox, EventButton, False);
         Pack_Start(WorldBox, EventBox, False);
      end;
      Add_Titled(Stack, WorldBox, "page4", "World");
   end CreateDebugUI;

   procedure ShowDebugUI is
   begin
      RefreshUI(Builder);
      Show_All(Gtk_Widget(Get_Object(Builder, "debugwindow")));
      ShowItemEvent
        (Gtk_Combo_Box
           (Get_Child_At
              (Gtk_Grid
                 (Get_Child
                    (Gtk_Box
                       (Get_Child
                          (Gtk_Box(Get_Child_By_Name(Stack, "page4")), 1)),
                     0)),
               1, 1)));
      ResetWorldUI;
   end ShowDebugUI;

end DebugUI;
