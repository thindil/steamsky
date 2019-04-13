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
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Game; use Game;
with Ships; use Ships;
with Maps.UI; use Maps.UI;
with Crew; use Crew;

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
   end SetMemberStats;

   procedure UpdateMember(Object: access Gtkada_Builder_Record'Class) is
      MemberIndex: constant Positive := Positive'Value
             (Get_Active_Id
                (Gtk_Combo_Box_Text(Get_Object(Object, "cmbmember"))));
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
   end UpdateMember;

   procedure RefreshUI(Object: access Gtkada_Builder_Record'Class) is
   begin
      UpdateShip(Object);
      UpdateCrew(Object);
   end RefreshUI;

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
      Do_Connect(Builder);
   end CreateDebugUI;

   procedure ShowDebugUI is
   begin
      RefreshUI(Builder);
      Show_All(Gtk_Widget(Get_Object(Builder, "debugwindow")));
   end ShowDebugUI;

end DebugUI;
