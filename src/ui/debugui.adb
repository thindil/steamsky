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
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Widget; use Gtk.Widget;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
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

   -- ****if* DebugUI/MoveShip
   -- FUNCTION
   -- Move the player ship to selected location on the map
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure MoveShip(Object: access Gtkada_Builder_Record'Class) is
   -- ****
   begin
      PlayerShip.SkyX :=
        Integer(Get_Value(Gtk_Adjustment(Get_Object(Object, "adjshipx"))));
      PlayerShip.SkyY :=
        Integer(Get_Value(Gtk_Adjustment(Get_Object(Object, "adjshipy"))));
      ShowSkyMap;
   end MoveShip;

   -- ****if* DebugUI/UpdateShip
   -- FUNCTION
   -- Update the player ship data
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure UpdateShip(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      CrewList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Object, "crewlist"));
      CrewIter: Gtk_Tree_Iter;
      ComboBox: constant Gtk_Combo_Box_Text :=
        Gtk_Combo_Box_Text(Get_Object(Object, "cmbmodules"));
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
        (Gtk_Adjustment(Get_Object(Object, "adjshipx")),
         Gdouble(PlayerShip.SkyX));
      Set_Value
        (Gtk_Adjustment(Get_Object(Object, "adjshipy")),
         Gdouble(PlayerShip.SkyY));
   end UpdateShip;

   -- ****if* DebugUI/UpdateCrew
   -- FUNCTION
   -- Update the player ship crew
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure UpdateCrew(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      ComboBox: constant Gtk_Combo_Box :=
        Gtk_Combo_Box(Get_Object(Object, "cmbmember"));
   begin
      Set_Active(ComboBox, 0);
   end UpdateCrew;

   -- ****if* DebugUI/SetMemberStats
   -- FUNCTION
   -- Show statistics, skills, etc for selected the player ship crew member
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure SetMemberStats(Object: access Gtkada_Builder_Record'Class) is
      -- ****
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
             (Get_Active_Id(Gtk_Combo_Box(Get_Object(Object, "cmbmember")))));
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
      MemberIndex: constant Positive :=
        Positive'Value
          (Get_Active_Id(Gtk_Combo_Box(Get_Object(Builder, "cmbmember"))));
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
      MemberIndex: constant Positive :=
        Positive'Value
          (Get_Active_Id(Gtk_Combo_Box(Get_Object(Builder, "cmbmember"))));
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
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure UpdateMember(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      MemberIndex: constant Positive :=
        Positive'Value
          (Get_Active_Id(Gtk_Combo_Box(Get_Object(Object, "cmbmember"))));
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

   -- ****if* DebugUI/UpdateCargoInfo
   -- FUNCTION
   -- Update the player ship cargo list
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure UpdateCargoInfo(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      ComboBox: constant Gtk_Combo_Box_Text :=
        Gtk_Combo_Box_Text(Get_Object(Object, "cmbcargoupdate"));
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
   -- Updated information about selected base
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure ShowBaseInfo(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      BaseName: constant Unbounded_String :=
        To_Unbounded_String
          (Get_Text(Gtk_GEntry(Get_Object(Object, "edtbase"))));
   begin
      for I in SkyBases'Range loop
         if SkyBases(I).Name = BaseName then
            Set_Active
              (Gtk_Combo_Box_Text(Get_Object(Object, "cmbbasetype")),
               Gint(BaseTypeIndex(SkyBases(I).BaseType) - 1));
            Set_Active
              (Gtk_Combo_Box_Text(Get_Object(Object, "cmbbasesize")),
               Bases_Size'Pos(SkyBases(I).Size));
            if not Set_Active_Id
                (Gtk_Combo_Box_Text(Get_Object(Object, "cmbbaseowner")),
                 To_String(SkyBases(I).Owner)) then
               return;
            end if;
            Set_Value
              (Gtk_Adjustment(Get_Object(Object, "adjpopulation")),
               Gdouble(SkyBases(I).Population));
            Set_Value
              (Gtk_Adjustment(Get_Object(Object, "adjreputation")),
               Gdouble(SkyBases(I).Reputation(1)));
            Set_Value
              (Gtk_Adjustment(Get_Object(Object, "adjbasemoney")),
               Gdouble(SkyBases(I).Cargo(1).Amount));
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
        Gtk_Widget(Get_Object(Builder, "cmbevents"));
   begin
      Set_Text(Gtk_GEntry(Get_Object(Builder, "edtship")), "");
      Set_Value(Gtk_Adjustment(Get_Object(Builder, "adjnpcshipx")), 1.0);
      Set_Value(Gtk_Adjustment(Get_Object(Builder, "adjnpcshipy")), 1.0);
      Set_Text(Gtk_GEntry(Get_Object(Builder, "edteventbase")), "");
      Set_Text(Gtk_GEntry(Get_Object(Builder, "edteventitem")), "");
      if Events_List.Length = 0 then
         Hide(EventsCombo);
         Hide(Gtk_Widget(Get_Object(Builder, "btndeleteevent")));
      else
         Show_All(EventsCombo);
         Show_All(Gtk_Widget(Get_Object(Builder, "btndeleteevent")));
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
   -- ****
   begin
      UpdateShip(Object);
      UpdateCrew(Object);
      UpdateCargoInfo(Object);
      ShowBaseInfo(Object);
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
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure AddSkill(Object: access Gtkada_Builder_Record'Class) is
      -- ****
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

   -- ****if* DebugUI/SetCargoAmount
   -- FUNCTION
   -- Set cargo amount value for selected item in the player ship cargo
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure SetCargoAmount(Object: access Gtkada_Builder_Record'Class) is
      -- ****
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

   -- ****if* DebugUI/AddCargo
   -- FUNCTION
   -- Add new item to the player ship cargo
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure AddCargo(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      ItemIndex: Unbounded_String := Null_Unbounded_String;
      ItemName: constant Unbounded_String :=
        To_Unbounded_String
          (Get_Text(Gtk_GEntry(Get_Object(Object, "edtcargoadd"))));
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
           (Get_Value(Gtk_Adjustment(Get_Object(Object, "adjaddcargo")))));
      UpdateCargoInfo(Object);
   end AddCargo;

   -- ****if* DebugUI/UpdateShipCargo
   -- FUNCTION
   -- Updated existing item in the player ship cargo
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure UpdateShipCargo(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      CargoIndex: constant Positive :=
        Positive'Value
          (Get_Active_Id
             (Gtk_Combo_Box_Text(Get_Object(Object, "cmbcargoupdate"))));
   begin
      UpdateCargo
        (Ship => PlayerShip,
         Amount =>
           (Integer
              (Get_Value
                 (Gtk_Adjustment(Get_Object(Object, "adjupdatecargo")))) -
            PlayerShip.Cargo(CargoIndex).Amount),
         CargoIndex => CargoIndex);
   end UpdateShipCargo;

   -- ****if* DebugUI/UpdateBase
   -- FUNCTION
   -- Update selected base with new data
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure UpdateBase(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      BaseName: constant Unbounded_String :=
        To_Unbounded_String
          (Get_Text(Gtk_GEntry(Get_Object(Object, "edtbase"))));
   begin
      for SkyBase of SkyBases loop
         if SkyBase.Name = BaseName then
            SkyBase.BaseType :=
              To_Unbounded_String
                (Get_Active_Text
                   (Gtk_Combo_Box_Text(Get_Object(Object, "cmbbasetype"))));
            SkyBase.Owner :=
              To_Unbounded_String
                (Get_Active_Id
                   (Gtk_Combo_Box_Text(Get_Object(Object, "cmbbaseowner"))));
            SkyBase.Size :=
              Bases_Size'Val
                (Integer
                   (Get_Active
                      (Gtk_Combo_Box_Text
                         (Get_Object(Object, "cmbbasesize")))));
            SkyBase.Cargo(1).Amount :=
              Natural
                (Get_Value
                   (Gtk_Adjustment(Get_Object(Object, "adjbasemoney"))));
            SkyBase.Population :=
              Natural
                (Get_Value
                   (Gtk_Adjustment(Get_Object(Object, "adjpopulation"))));
            SkyBase.Reputation(1) :=
              Natural
                (Get_Value
                   (Gtk_Adjustment(Get_Object(Object, "adjreputation"))));
         end if;
      end loop;
   end UpdateBase;

   -- ****if* DebugUI/AddShip
   -- FUNCTION
   -- Add new NPC ship on map
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure AddShip(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      ShipName: constant Unbounded_String :=
        To_Unbounded_String
          (Get_Text(Gtk_GEntry(Get_Object(Object, "edtship"))));
      NpcShipX: constant Positive :=
        Positive(Get_Value(Gtk_Adjustment(Get_Object(Object, "adjnpcshipx"))));
      NpcShipY: constant Positive :=
        Positive(Get_Value(Gtk_Adjustment(Get_Object(Object, "adjnpcshipy"))));
   begin
      for I in ProtoShips_List.Iterate loop
         if ProtoShips_List(I).Name = ShipName then
            if Traders.Contains(ProtoShips_Container.Key(I)) then
               Events_List.Append
                 (New_Item =>
                    (Trader, NpcShipX, NpcShipY,
                     Positive
                       (Get_Value
                          (Gtk_Adjustment(Get_Object(Object, "adjminutes")))),
                     ProtoShips_Container.Key(I)));
            elsif FriendlyShips.Contains(ProtoShips_Container.Key(I)) then
               Events_List.Append
                 (New_Item =>
                    (FriendlyShip, NpcShipX, NpcShipY,
                     Positive
                       (Get_Value
                          (Gtk_Adjustment(Get_Object(Object, "adjminutes")))),
                     ProtoShips_Container.Key(I)));
            else
               Events_List.Append
                 (New_Item =>
                    (EnemyShip, NpcShipX, NpcShipY,
                     Positive
                       (Get_Value
                          (Gtk_Adjustment(Get_Object(Object, "adjminutes")))),
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
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure ShowItemEvent(Object: access Gtkada_Builder_Record'Class) is
   -- ****
   begin
      if Get_Active(Gtk_Combo_Box_Text(Get_Object(Object, "cmbbaseevent"))) =
        1 then
         Show_All(Gtk_Widget(Get_Object(Object, "lbleventitem")));
         Show_All(Gtk_Widget(Get_Object(Object, "edteventitem")));
      else
         Hide(Gtk_Widget(Get_Object(Object, "lbleventitem")));
         Hide(Gtk_Widget(Get_Object(Object, "edteventitem")));
      end if;
   end ShowItemEvent;

   -- ****if* DebugUI/AddEvent
   -- FUNCTION
   -- Add new event to the game
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure AddEvent(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      BaseName: constant Unbounded_String :=
        To_Unbounded_String
          (Get_Text(Gtk_GEntry(Get_Object(Object, "edteventbase"))));
      ItemName: constant Unbounded_String :=
        To_Unbounded_String
          (Get_Text(Gtk_GEntry(Get_Object(Object, "edteventitem"))));
      EventAdded: Boolean := True;
   begin
      for Base of SkyBases loop
         if Base.Name = BaseName then
            case Get_Active
              (Gtk_Combo_Box_Text(Get_Object(Object, "cmbbaseevent"))) is
               when 0 =>
                  Events_List.Append
                    (New_Item =>
                       (Disease, Base.SkyX, Base.SkyY,
                        Positive
                          (Get_Value
                             (Gtk_Adjustment
                                (Get_Object(Object, "adjminutesbase")))),
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
                                      (Get_Object(Object, "adjminutesbase")))),
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
                                (Get_Object(Object, "adjminutesbase")))),
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
   -- SOURCE
   procedure DeleteEvent(Object: access Gtkada_Builder_Record'Class) is
   -- ****
   begin
      Events_List.Delete
        (Index =>
           Positive'Value
             (Get_Active_Id
                (Gtk_Combo_Box_Text(Get_Object(Object, "cmbevents")))));
      ResetWorldUI;
   end DeleteEvent;

   -- ****if* DebugUI/Save_Game
   -- FUNCTION
   -- Save current state of the game to disk
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI (unused)
   -- SOURCE
   procedure Save_Game(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
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
   procedure SetModuleStats(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      Module: ModuleData;
   begin
      if Setting then
         return;
      end if;
      Module :=
        PlayerShip.Modules
          (Positive'Value
             (Get_Active_Id(Gtk_Combo_Box(Get_Object(Object, "cmbmodules")))));
      Set_Text
        (Gtk_GEntry(Get_Object(Object, "edtprototype")),
         To_String(Modules_List(Module.ProtoIndex).Name));
      Set_Value
        (Gtk_Adjustment(Get_Object(Object, "adjmoduleweight")),
         Gdouble(Module.Weight));
      Set_Value
        (Gtk_Adjustment(Get_Object(Object, "adjmoduledur")),
         Gdouble(Module.Durability));
      Set_Value
        (Gtk_Adjustment(Get_Object(Object, "adjmodulemaxdur")),
         Gdouble(Module.MaxDurability));
      Set_Value
        (Gtk_Adjustment(Get_Object(Object, "adjmoduleupgrade")),
         Gdouble(Module.UpgradeProgress));
   end SetModuleStats;

   -- ****if* DebugUI/UpdateModule
   -- FUNCTION
   -- Update selected the player ship module with new data
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure UpdateModule(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      ModuleIndex: constant Positive :=
        Positive'Value
          (Get_Active_Id(Gtk_Combo_Box(Get_Object(Object, "cmbmodules"))));
      ProtoName: constant Unbounded_String :=
        To_Unbounded_String
          (Get_Text(Gtk_GEntry(Get_Object(Object, "edtprototype"))));
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
          (Get_Value(Gtk_Adjustment(Get_Object(Object, "adjmoduleweight"))));
      PlayerShip.Modules(ModuleIndex).Durability :=
        Natural(Get_Value(Gtk_Adjustment(Get_Object(Object, "adjmoduledur"))));
      PlayerShip.Modules(ModuleIndex).MaxDurability :=
        Natural
          (Get_Value(Gtk_Adjustment(Get_Object(Object, "adjmodulemaxdur"))));
      PlayerShip.Modules(ModuleIndex).UpgradeProgress :=
        Natural
          (Get_Value(Gtk_Adjustment(Get_Object(Object, "adjmoduleupgrade"))));
   end UpdateModule;

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
      Register_Handler(Builder, "Update_Cargo", UpdateShipCargo'Access);
      Register_Handler(Builder, "Show_Base_Info", ShowBaseInfo'Access);
      Register_Handler(Builder, "Update_Base", UpdateBase'Access);
      Register_Handler(Builder, "Add_Ship", AddShip'Access);
      Register_Handler(Builder, "Show_Item_Event", ShowItemEvent'Access);
      Register_Handler(Builder, "Add_Event", AddEvent'Access);
      Register_Handler(Builder, "Delete_Event", DeleteEvent'Access);
      Register_Handler(Builder, "Save_Game", Save_Game'Access);
      Register_Handler(Builder, "Set_Module_Stats", SetModuleStats'Access);
      Register_Handler(Builder, "Update_Module", UpdateModule'Access);
      Do_Connect(Builder);
      On_Edited
        (Gtk_Cell_Renderer_Text(Get_Object(Builder, "renderstat")),
         ChangeStatLevel'Access);
      On_Edited
        (Gtk_Cell_Renderer_Text(Get_Object(Builder, "renderskill")),
         ChangeSkillLevel'Access);
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
      declare
         ComboBox: Gtk_Combo_Box_Text;
      begin
         ComboBox := Gtk_Combo_Box_Text(Get_Object(Builder, "cmbbasetype"));
         for I in BasesTypes_List.Iterate loop
            Append_Text(ComboBox, To_String(BasesTypes_Container.Key(I)));
         end loop;
         ComboBox := Gtk_Combo_Box_Text(Get_Object(Builder, "cmbbaseowner"));
         for I in Factions_List.Iterate loop
            Append
              (ComboBox, To_String(Factions_Container.Key(I)),
               To_String(Factions_List(I).Name));
         end loop;
         ComboBox := Gtk_Combo_Box_Text(Get_Object(Builder, "cmbbasesize"));
         for I in Bases_Size loop
            Append_Text
              (ComboBox,
               Bases_Size'Image(I)(1) &
               To_Lower(Bases_Size'Image(I)(2 .. Bases_Size'Image(I)'Last)));
         end loop;
      end;
   end CreateDebugUI;

   procedure ShowDebugUI is
   begin
      RefreshUI(Builder);
      Show_All(Gtk_Widget(Get_Object(Builder, "debugwindow")));
      ShowItemEvent(Builder);
      ResetWorldUI;
   end ShowDebugUI;

end DebugUI;
