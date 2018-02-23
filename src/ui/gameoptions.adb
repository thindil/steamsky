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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Switch; use Gtk.Switch;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Adjustment; use Gtk.Adjustment;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Glib.Object; use Glib.Object;
with Game; use Game;
with Maps.UI; use Maps.UI;
with Config; use Config;
with Ships; use Ships;

package body GameOptions is

   Builder: Gtkada_Builder;

   function HideOptions
     (Object: access Gtkada_Builder_Record'Class) return Boolean is
   begin
      GameSettings.AutoRest :=
        Get_State(Gtk_Switch(Get_Object(Object, "switchautorest")));
      GameSettings.UndockSpeed :=
        ShipSpeed'Val
          (Get_Active(Gtk_Combo_Box(Get_Object(Object, "cmbspeed"))) + 1);
      GameSettings.AutoCenter :=
        Get_State(Gtk_Switch(Get_Object(Object, "switchautocenter")));
      GameSettings.AutoReturn :=
        Get_State(Gtk_Switch(Get_Object(Object, "switchautoreturn")));
      GameSettings.AutoFinish :=
        Get_State(Gtk_Switch(Get_Object(Object, "switchautofinish")));
      GameSettings.LowFuel :=
        Positive(Get_Value(Gtk_Adjustment(Get_Object(Object, "adjfuel"))));
      GameSettings.LowDrinks :=
        Positive(Get_Value(Gtk_Adjustment(Get_Object(Object, "adjdrinks"))));
      GameSettings.LowFood :=
        Positive(Get_Value(Gtk_Adjustment(Get_Object(Object, "adjfood"))));
      GameSettings.AutoMoveStop :=
        AutoMoveBreak'Val
          (Get_Active(Gtk_Combo_Box(Get_Object(Object, "cmbautomovestop"))));
      SaveConfig;
      Hide(Gtk_Widget(Get_Object(Object, "optionswindow")));
      CreateSkyMap;
      return True;
   end HideOptions;

   procedure CreateGameOptions is
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
           "gameoptions.glade",
           Error'Access) =
        Guint(0) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      Register_Handler(Builder, "Hide_Options", HideOptions'Access);
      Do_Connect(Builder);
   end CreateGameOptions;

   procedure ShowGameOptions is
   begin
      Set_State
        (Gtk_Switch(Get_Object(Builder, "switchautorest")),
         GameSettings.AutoRest);
      Set_Active
        (Gtk_Combo_Box(Get_Object(Builder, "cmbspeed")),
         (ShipSpeed'Pos(GameSettings.UndockSpeed) - 1));
      Set_State
        (Gtk_Switch(Get_Object(Builder, "switchautocenter")),
         GameSettings.AutoCenter);
      Set_State
        (Gtk_Switch(Get_Object(Builder, "switchautoreturn")),
         GameSettings.AutoReturn);
      Set_State
        (Gtk_Switch(Get_Object(Builder, "switchautofinish")),
         GameSettings.AutoFinish);
      Set_Value
        (Gtk_Adjustment(Get_Object(Builder, "adjfuel")),
         Gdouble(GameSettings.LowFuel));
      Set_Value
        (Gtk_Adjustment(Get_Object(Builder, "adjdrinks")),
         Gdouble(GameSettings.LowDrinks));
      Set_Value
        (Gtk_Adjustment(Get_Object(Builder, "adjfood")),
         Gdouble(GameSettings.LowFood));
      Set_Active
        (Gtk_Combo_Box(Get_Object(Builder, "cmbautomovestop")),
         (AutoMoveBreak'Pos(GameSettings.AutoMoveStop)));
      Show_All(Gtk_Widget(Get_Object(Builder, "optionswindow")));
   end ShowGameOptions;

end GameOptions;
