--    Copyright 2019 Bartek thindil Jasicki
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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Game; use Game;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Maps.UI; use Maps.UI;
with Crew; use Crew;
with Items; use Items;

package body DebugUI is

   Builder: Gtkada_Builder;
   Setting: Boolean;

   procedure MoveShip(Object: access Gtkada_Builder_Record'Class) is
   begin
      PlayerShip.SkyX :=
        Integer(Get_Value(Gtk_Adjustment(Get_Object(Object, "adjshipx"))));
      PlayerShip.SkyY :=
        Integer(Get_Value(Gtk_Adjustment(Get_Object(Object, "adjshipy"))));
      ShowSkyMap;
   end MoveShip;

   procedure UpdateShip(Object: access Gtkada_Builder_Record'Class) is
   begin
      Set_Value
        (Gtk_Adjustment(Get_Object(Object, "adjshipx")),
         Gdouble(PlayerShip.SkyX));
      Set_Value
        (Gtk_Adjustment(Get_Object(Object, "adjshipy")),
         Gdouble(PlayerShip.SkyY));
   end UpdateShip;

   procedure UpdateCrew(Object: access Gtkada_Builder_Record'Class) is
      ComboBox: constant Gtk_Combo_Box_Text :=
        Gtk_Combo_Box_Text(Get_Object(Object, "cmbmember"));
   begin
      Setting := True;
      Remove_All(ComboBox);
      for I in PlayerShip.Crew.Iterate loop
         Append
           (ComboBox, Positive'Image(Crew_Container.To_Index(I)),
            To_String(PlayerShip.Crew(I).Name));
      end loop;
      Setting := False;
      Set_Active(ComboBox, 0);
   end UpdateCrew;

   procedure SetMemberStats(Object: access Gtkada_Builder_Record'Class) is
      Member: Member_Data;
      Iter: Gtk_Tree_Iter;
      List: Gtk_List_Store := Gtk_List_Store(Get_Object(Builder, "statslist"));
      KnowSkills: Positive_Container.Vector;
      ComboBox: constant Gtk_Combo_Box_Text :=
        Gtk_Combo_Box_Text(Get_Object(Object, "cmbskill"));
   begin
      if Setting then
         return;
      end if;
      Member :=
        PlayerShip.Crew
          (Positive'Value
             (Get_Active_Id
                (Gtk_Combo_Box_Text(Get_Object(Object, "cmbmember")))));
      Set_Value
        (Gtk_Adjustment(Get_Object(Object, "adjhealth")),
         Gdouble(Member.Health));
      Set_Value
        (Gtk_Adjustment(Get_Object(Object, "adjthirst")),
         Gdouble(Member.Thirst));
      Set_Value
        (Gtk_Adjustment(Get_Object(Object, "adjhunger")),
         Gdouble(Member.Hunger));
      Set_Value
        (Gtk_Adjustment(Get_Object(Object, "adjtired")),
         Gdouble(Member.Tired));
      Set_Value
        (Gtk_Adjustment(Get_Object(Object, "adjmorale")),
         Gdouble(Member.Morale(1)));
      Set_Value
        (Gtk_Adjustment(Get_Object(Object, "adjloyalty")),
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
         Show_All(Gtk_Widget(Get_Object(Object, "addskillbox")));
         Set_Active(ComboBox, 0);
      else
         Hide(Gtk_Widget(Get_Object(Object, "addskillbox")));
      end if;
   end SetMemberStats;

   function UpdateAttribute(Model: Gtk_Tree_Model; Path: Gtk_Tree_Path;
      Iter: Gtk_Tree_Iter) return Boolean is
      pragma Unreferenced(Path);
      MemberIndex: constant Positive :=
        Positive'Value
          (Get_Active_Id
             (Gtk_Combo_Box_Text(Get_Object(Builder, "cmbmember"))));
   begin
      PlayerShip.Crew(MemberIndex).Attributes
        (Positive(Get_Int(Model, Iter, 1)))
        (1) :=
        Positive(Get_Int(Model, Iter, 2));
      return False;
   end UpdateAttribute;

   function UpdateSkill(Model: Gtk_Tree_Model; Path: Gtk_Tree_Path;
      Iter: Gtk_Tree_Iter) return Boolean is
      pragma Unreferenced(Path);
      MemberIndex: constant Positive :=
        Positive'Value
          (Get_Active_Id
             (Gtk_Combo_Box_Text(Get_Object(Builder, "cmbmember"))));
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

   procedure UpdateMember(Object: access Gtkada_Builder_Record'Class) is
      MemberIndex: constant Positive :=
        Positive'Value
          (Get_Active_Id(Gtk_Combo_Box_Text(Get_Object(Object, "cmbmember"))));
   begin
      PlayerShip.Crew(MemberIndex).Health :=
        Natural(Get_Value(Gtk_Adjustment(Get_Object(Object, "adjhealth"))));
      PlayerShip.Crew(MemberIndex).Thirst :=
        Natural(Get_Value(Gtk_Adjustment(Get_Object(Object, "adjthirst"))));
      PlayerShip.Crew(MemberIndex).Hunger :=
        Natural(Get_Value(Gtk_Adjustment(Get_Object(Object, "adjhunger"))));
      PlayerShip.Crew(MemberIndex).Tired :=
        Natural(Get_Value(Gtk_Adjustment(Get_Object(Object, "adjtired"))));
      PlayerShip.Crew(MemberIndex).Morale(1) :=
        Natural(Get_Value(Gtk_Adjustment(Get_Object(Object, "adjmorale"))));
      PlayerShip.Crew(MemberIndex).Loyalty :=
        Natural(Get_Value(Gtk_Adjustment(Get_Object(Object, "adjloyalty"))));
      Foreach
        (Gtk_List_Store(Get_Object(Object, "statslist")),
         UpdateAttribute'Access);
      Foreach
        (Gtk_List_Store(Get_Object(Object, "skillslist")), UpdateSkill'Access);
   end UpdateMember;

   procedure UpdateCargoInfo(Object: access Gtkada_Builder_Record'Class) is
      ComboBox: Gtk_Combo_Box_Text :=
        Gtk_Combo_Box_Text(Get_Object(Object, "cmbcargoadd"));
   begin
      Setting := True;
      Remove_All(ComboBox);
      for I in Items_List.Iterate loop
         Append
           (ComboBox, To_String(Objects_Container.Key(I)),
            To_String(Items_List(I).Name));
      end loop;
      Set_Active(ComboBox, 0);
      ComboBox := Gtk_Combo_Box_Text(Get_Object(Object, "cmbcargoupdate"));
      Remove_All(ComboBox);
      for I in PlayerShip.Cargo.Iterate loop
         Append
           (ComboBox, Positive'Image(Inventory_Container.To_Index(I)),
            GetItemName(PlayerShip.Cargo(I), False));
      end loop;
      Setting := False;
      Set_Active(ComboBox, 0);
   end UpdateCargoInfo;

   procedure RefreshUI(Object: access Gtkada_Builder_Record'Class) is
   begin
      UpdateShip(Object);
      UpdateCrew(Object);
      UpdateCargoInfo(Object);
   end RefreshUI;

   procedure ChangeStatLevel(Self: access Gtk_Cell_Renderer_Text_Record'Class;
      Path: UTF8_String; New_Text: UTF8_String) is
      pragma Unreferenced(Self);
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

   procedure ChangeSkillLevel(Self: access Gtk_Cell_Renderer_Text_Record'Class;
      Path: UTF8_String; New_Text: UTF8_String) is
      pragma Unreferenced(Self);
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

   procedure AddSkill(Object: access Gtkada_Builder_Record'Class) is
      SkillsIter: Gtk_Tree_Iter;
      SkillsList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "skillslist"));
      ComboBox: constant Gtk_Combo_Box_Text :=
        Gtk_Combo_Box_Text(Get_Object(Object, "cmbskill"));
   begin
      Append(SkillsList, SkillsIter);
      Set(SkillsList, SkillsIter, 0, Get_Active_Text(ComboBox));
      Set(SkillsList, SkillsIter, 1, Gint'Value(Get_Active_Id(ComboBox)));
      Set(SkillsList, SkillsIter, 2, 1);
      Remove(ComboBox, Get_Active(ComboBox));
      if N_Children(Get_Model(ComboBox)) > 0 then
         Set_Active(ComboBox, 0);
      else
         Hide(Gtk_Widget(Get_Object(Object, "addskillbox")));
      end if;
   end AddSkill;

   procedure SetCargoAmount(Object: access Gtkada_Builder_Record'Class) is
      Item: InventoryData;
   begin
      if Setting then
         return;
      end if;
      Item :=
        PlayerShip.Cargo
          (Positive'Value
             (Get_Active_Id
                (Gtk_Combo_Box_Text(Get_Object(Object, "cmbcargoupdate")))));
      Set_Value
        (Gtk_Adjustment(Get_Object(Object, "adjupdatecargo")),
         Gdouble(Item.Amount));
   end SetCargoAmount;

   procedure AddCargo(Object: access Gtkada_Builder_Record'Class) is
   begin
      UpdateCargo
        (PlayerShip,
         To_Unbounded_String
           (Get_Active_Id
              (Gtk_Combo_Box_Text(Get_Object(Object, "cmbcargoadd")))),
         Positive
           (Get_Value(Gtk_Adjustment(Get_Object(Object, "adjaddcargo")))));
      UpdateCargoInfo(Object);
   end AddCargo;

   procedure CreateDebugUI is
      Error: aliased GError;
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
      Register_Handler(Builder, "Move_Ship", MoveShip'Access);
      Register_Handler(Builder, "Update_Ship", UpdateShip'Access);
      Register_Handler(Builder, "Refresh_UI", RefreshUI'Access);
      Register_Handler(Builder, "Update_Crew", UpdateCrew'Access);
      Register_Handler(Builder, "Set_Member_Stats", SetMemberStats'Access);
      Register_Handler(Builder, "Update_Member", UpdateMember'Access);
      Register_Handler(Builder, "Add_Skill", AddSkill'Access);
      Register_Handler(Builder, "Update_Cargo_Info", UpdateCargoInfo'Access);
      Register_Handler(Builder, "Set_Cargo_Amount", SetCargoAmount'Access);
      Register_Handler(Builder, "Add_Cargo", AddCargo'Access);
      Do_Connect(Builder);
      On_Edited
        (Gtk_Cell_Renderer_Text(Get_Object(Builder, "renderstat")),
         ChangeStatLevel'Access);
      On_Edited
        (Gtk_Cell_Renderer_Text(Get_Object(Builder, "renderskill")),
         ChangeSkillLevel'Access);
   end CreateDebugUI;

   procedure ShowDebugUI is
   begin
      RefreshUI(Builder);
      Show_All(Gtk_Widget(Get_Object(Builder, "debugwindow")));
   end ShowDebugUI;

end DebugUI;
