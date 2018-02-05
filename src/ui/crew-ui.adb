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
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Label; use Gtk.Label;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Gdk.RGBA; use Gdk.RGBA;
with Game; use Game;
with Maps.UI; use Maps.UI;
with Combat.UI; use Combat.UI;
with Ships; use Ships;
with ShipModules; use ShipModules;
with Help.UI; use Help.UI;

package body Crew.UI is

   Builder: Gtkada_Builder;
   GameState: GameStates;

   function HideCrewInfo
     (Object: access Gtkada_Builder_Record'Class) return Boolean is
   begin
      Hide(Gtk_Widget(Get_Object(Object, "crewwindow")));
      case GameState is
         when SkyMap_View =>
            CreateSkyMap;
         when Combat_View =>
            ShowCombatUI;
      end case;
      return True;
   end HideCrewInfo;

   procedure ShowMemberInfo(Object: access Gtkada_Builder_Record'Class) is
      CrewIter: Gtk_Tree_Iter;
      CrewModel: Gtk_Tree_Model;
      MemberIndex: Positive;
      Member: Member_Data;
      MemberInfo: Unbounded_String;
      TiredPoints: Integer;
      Iter: Gtk_Tree_Iter;
      List: Gtk_List_Store;
      OrdersForAll: Boolean := False;
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treecrew"))),
         CrewModel,
         CrewIter);
      if CrewIter = Null_Iter then
         return;
      end if;
      MemberIndex :=
        Natural'Value(To_String(Get_Path(CrewModel, CrewIter))) + 1;
      Member := PlayerShip.Crew(MemberIndex);
      if Member.Gender = 'M' then
         MemberInfo := To_Unbounded_String("Gender: Male");
      else
         MemberInfo := To_Unbounded_String("Gender: Female");
      end if;
      if Member.Skills.Length = 0 then
         Hide(Gtk_Widget(Get_Object(Object, "treestats")));
         Hide(Gtk_Widget(Get_Object(Object, "scrollskills")));
         Hide(Gtk_Widget(Get_Object(Object, "lblorder")));
         Hide(Gtk_Widget(Get_Object(Object, "btnorders")));
         Hide(Gtk_Widget(Get_Object(Object, "btninventory")));
         Append(MemberInfo, ASCII.LF & "Passenger");
      else
         Show_All(Gtk_Widget(Get_Object(Object, "treestats")));
         Show_All(Gtk_Widget(Get_Object(Object, "scrollskills")));
         Show_All(Gtk_Widget(Get_Object(Object, "lblorder")));
         Show_All(Gtk_Widget(Get_Object(Object, "btnorders")));
         Show_All(Gtk_Widget(Get_Object(Object, "btninventory")));
      end if;
      for Module of PlayerShip.Modules loop
         if Module.Durability < Module.MaxDurability then
            OrdersForAll := True;
            exit;
         end if;
         if Module.Durability > 0 and
           Modules_List(Module.ProtoIndex).MType = CABIN and
           Module.Data(1) < Module.Data(2) then
            OrdersForAll := True;
            exit;
         end if;
      end loop;
      if not OrdersForAll then
         Hide(Gtk_Widget(Get_Object(Object, "btnordersall")));
      else
         Show_All(Gtk_Widget(Get_Object(Object, "btnordersall")));
      end if;
      if Member.Health < 100 and Member.Health > 80 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""green"">Slightly wounded</span>");
      elsif Member.Health < 81 and Member.Health > 50 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""yellow"">Wounded</span>");
      elsif Member.Health < 51 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""red"">Heavily Wounded</span>");
      end if;
      TiredPoints := Member.Tired - Member.Attributes(ConditionIndex)(1);
      if TiredPoints > 20 and TiredPoints < 41 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""green"">Bit tired</span>");
      elsif TiredPoints > 40 and TiredPoints < 81 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""yellow"">Tired</span>");
      elsif TiredPoints > 80 and TiredPoints < 100 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""red"">Very tired</span>");
      elsif TiredPoints = 100 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""blue"">Unconscious</span>");
      end if;
      if Member.Thirst > 20 and Member.Thirst < 41 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""green"">Bit thirsty</span>");
      elsif Member.Thirst > 40 and Member.Thirst < 81 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""yellow"">Thirsty</span>");
      elsif Member.Thirst > 80 and Member.Thirst < 100 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""red"">Very thirsty</span>");
      elsif Member.Thirst = 100 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""blue"">Dehydrated</span>");
      end if;
      if Member.Hunger > 20 and Member.Hunger < 41 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""green"">Bit hungry</span>");
      elsif Member.Hunger > 40 and Member.Hunger < 81 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""yellow"">Hungry</span>");
      elsif Member.Hunger > 80 and Member.Hunger < 100 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""red"">Very hungry</span>");
      elsif Member.Hunger = 100 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""blue"">Starving</span>");
      end if;
      Set_Markup
        (Gtk_Label(Get_Object(Object, "lblinfo")),
         To_String(MemberInfo));
      if Member.Skills.Length > 0 then
         List := Gtk_List_Store(Get_Object(Builder, "statslist"));
         Clear(List);
         for I in Member.Attributes.Iterate loop
            Append(List, Iter);
            Set
              (List,
               Iter,
               0,
               To_String(Attributes_Names(Attributes_Container.To_Index(I))));
            Set(List, Iter, 1, Gint(Member.Attributes(I)(1) * 2));
         end loop;
         List := Gtk_List_Store(Get_Object(Builder, "skillslist"));
         Clear(List);
         for Skill of Member.Skills loop
            Append(List, Iter);
            Set(List, Iter, 0, To_String(Skills_List(Skill(1)).Name));
            Set(List, Iter, 1, Gint(Skill(2)));
         end loop;
         case Member.Order is
            when Pilot =>
               MemberInfo := To_Unbounded_String("Piloting");
            when Engineer =>
               MemberInfo := To_Unbounded_String("Engineering");
            when Gunner =>
               MemberInfo := To_Unbounded_String("Gunner");
            when Rest =>
               MemberInfo := To_Unbounded_String("On break");
            when Repair =>
               MemberInfo := To_Unbounded_String("Repair ship");
            when Craft =>
               MemberInfo := To_Unbounded_String("Manufacturing");
            when Upgrading =>
               MemberInfo := To_Unbounded_String("Upgrading module");
            when Talk =>
               MemberInfo := To_Unbounded_String("Talking in bases");
            when Heal =>
               MemberInfo := To_Unbounded_String("Healing wounded");
            when Clean =>
               MemberInfo := To_Unbounded_String("Cleans ship");
            when Boarding =>
               MemberInfo := To_Unbounded_String("Boarding");
            when Defend =>
               MemberInfo := To_Unbounded_String("Defends ship");
         end case;
         Set_Text
           (Gtk_Label(Get_Object(Object, "lblorder")),
            "Order: " & To_String(MemberInfo));
      end if;
   end ShowMemberInfo;

   procedure ShowHelp(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
   begin
      ShowHelpUI(7);
   end ShowHelp;

   procedure CreateCrewUI is
      Error: aliased GError;
   begin
      if Builder /= null then
         return;
      end if;
      Gtk_New(Builder);
      if Add_From_File
          (Builder,
           To_String(DataDirectory) & "ui" & Dir_Separator & "crew.glade",
           Error'Access) =
        Guint(0) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      Override_Background_Color
        (Gtk_Widget(Get_Object(Builder, "lblinfo")),
         0,
         Black_RGBA);
      Override_Color
        (Gtk_Widget(Get_Object(Builder, "lblinfo")),
         0,
         White_RGBA);
      Register_Handler(Builder, "Hide_Crew_Info", HideCrewInfo'Access);
      Register_Handler(Builder, "Show_Member_Info", ShowMemberInfo'Access);
      Register_Handler(Builder, "Show_Help", ShowHelp'Access);
      Do_Connect(Builder);
   end CreateCrewUI;

   procedure ShowCrewUI(OldState: GameStates) is
      CrewIter: Gtk_Tree_Iter;
      CrewList: Gtk_List_Store;
   begin
      CrewList := Gtk_List_Store(Get_Object(Builder, "crewlist"));
      Clear(CrewList);
      for Member of PlayerShip.Crew loop
         Append(CrewList, CrewIter);
         Set(CrewList, CrewIter, 0, To_String(Member.Name));
      end loop;
      GameState := OldState;
      Show_All(Gtk_Widget(Get_Object(Builder, "crewwindow")));
   end ShowCrewUI;

end Crew.UI;
