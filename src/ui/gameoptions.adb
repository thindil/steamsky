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
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Accel_Map; use Gtk.Accel_Map;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Glib.Object; use Glib.Object;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
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
      Save(To_String(SaveDirectory) & "keys.cfg");
      Hide(Gtk_Widget(Get_Object(Object, "optionswindow")));
      CreateSkyMap;
      return True;
   end HideOptions;

   function SetAccelerator
     (Self: access Gtk_Widget_Record'Class;
      Event: Gdk.Event.Gdk_Event_Key) return Boolean is
      KeyMods: constant Gdk_Modifier_Type :=
        Event.State and Get_Default_Mod_Mask;
      Changed: Boolean;
   begin
      if Self = Gtk_Widget(Get_Object(Builder, "edtupleft")) then
         Changed :=
           Change_Entry
             ("<skymapwindow>/btnupleft",
              Event.Keyval,
              KeyMods,
              True);
      elsif Self = Gtk_Widget(Get_Object(Builder, "edtup")) then
         Changed :=
           Change_Entry("<skymapwindow>/btnup", Event.Keyval, KeyMods, True);
      elsif Self = Gtk_Widget(Get_Object(Builder, "edtupright")) then
         Changed :=
           Change_Entry
             ("<skymapwindow>/btnupright",
              Event.Keyval,
              KeyMods,
              True);
      elsif Self = Gtk_Widget(Get_Object(Builder, "edtleft")) then
         Changed :=
           Change_Entry("<skymapwindow>/btnleft", Event.Keyval, KeyMods, True);
      elsif Self = Gtk_Widget(Get_Object(Builder, "edtmovewait")) then
         Changed :=
           Change_Entry
             ("<skymapwindow>/btnmovewait",
              Event.Keyval,
              KeyMods,
              True);
      elsif Self = Gtk_Widget(Get_Object(Builder, "edtright")) then
         Changed :=
           Change_Entry
             ("<skymapwindow>/btnright",
              Event.Keyval,
              KeyMods,
              True);
      elsif Self = Gtk_Widget(Get_Object(Builder, "edtdownleft")) then
         Changed :=
           Change_Entry
             ("<skymapwindow>/btnbottomleft",
              Event.Keyval,
              KeyMods,
              True);
      elsif Self = Gtk_Widget(Get_Object(Builder, "edtdown")) then
         Changed :=
           Change_Entry
             ("<skymapwindow>/btnbottom",
              Event.Keyval,
              KeyMods,
              True);
      elsif Self = Gtk_Widget(Get_Object(Builder, "edtdownright")) then
         Changed :=
           Change_Entry
             ("<skymapwindow>/btnbottomright",
              Event.Keyval,
              KeyMods,
              True);
      elsif Self = Gtk_Widget(Get_Object(Builder, "edtmoveto")) then
         Changed :=
           Change_Entry
             ("<skymapwindow>/btnmoveto",
              Event.Keyval,
              KeyMods,
              True);
      end if;
      if Changed then
         Set_Text
           (Gtk_Entry(Self),
            Accelerator_Get_Label(Event.Keyval, KeyMods));
      end if;
      return False;
   end SetAccelerator;

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
      On_Key_Press_Event
        (Gtk_Widget(Get_Object(Builder, "edtupleft")),
         SetAccelerator'Access);
      On_Key_Press_Event
        (Gtk_Widget(Get_Object(Builder, "edtup")),
         SetAccelerator'Access);
      On_Key_Press_Event
        (Gtk_Widget(Get_Object(Builder, "edtupright")),
         SetAccelerator'Access);
      On_Key_Press_Event
        (Gtk_Widget(Get_Object(Builder, "edtleft")),
         SetAccelerator'Access);
      On_Key_Press_Event
        (Gtk_Widget(Get_Object(Builder, "edtmovewait")),
         SetAccelerator'Access);
      On_Key_Press_Event
        (Gtk_Widget(Get_Object(Builder, "edtright")),
         SetAccelerator'Access);
      On_Key_Press_Event
        (Gtk_Widget(Get_Object(Builder, "edtdownleft")),
         SetAccelerator'Access);
      On_Key_Press_Event
        (Gtk_Widget(Get_Object(Builder, "edtdown")),
         SetAccelerator'Access);
      On_Key_Press_Event
        (Gtk_Widget(Get_Object(Builder, "edtdownright")),
         SetAccelerator'Access);
      On_Key_Press_Event
        (Gtk_Widget(Get_Object(Builder, "edtmoveto")),
         SetAccelerator'Access);
   end CreateGameOptions;

   procedure ShowGameOptions is
      Key: Gtk_Accel_Key;
      Found: Boolean;
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
      Lookup_Entry("<skymapwindow>/btnupleft", Key, Found);
      Set_Text
        (Gtk_Entry(Get_Object(Builder, "edtupleft")),
         Accelerator_Get_Label(Key.Accel_Key, Key.Accel_Mods));
      Lookup_Entry("<skymapwindow>/btnup", Key, Found);
      Set_Text
        (Gtk_Entry(Get_Object(Builder, "edtup")),
         Accelerator_Get_Label(Key.Accel_Key, Key.Accel_Mods));
      Lookup_Entry("<skymapwindow>/btnupright", Key, Found);
      Set_Text
        (Gtk_Entry(Get_Object(Builder, "edtupright")),
         Accelerator_Get_Label(Key.Accel_Key, Key.Accel_Mods));
      Lookup_Entry("<skymapwindow>/btnleft", Key, Found);
      Set_Text
        (Gtk_Entry(Get_Object(Builder, "edtleft")),
         Accelerator_Get_Label(Key.Accel_Key, Key.Accel_Mods));
      Lookup_Entry("<skymapwindow>/btnmovewait", Key, Found);
      Set_Text
        (Gtk_Entry(Get_Object(Builder, "edtmovewait")),
         Accelerator_Get_Label(Key.Accel_Key, Key.Accel_Mods));
      Lookup_Entry("<skymapwindow>/btnright", Key, Found);
      Set_Text
        (Gtk_Entry(Get_Object(Builder, "edtright")),
         Accelerator_Get_Label(Key.Accel_Key, Key.Accel_Mods));
      Lookup_Entry("<skymapwindow>/btnbottomleft", Key, Found);
      Set_Text
        (Gtk_Entry(Get_Object(Builder, "edtdownleft")),
         Accelerator_Get_Label(Key.Accel_Key, Key.Accel_Mods));
      Lookup_Entry("<skymapwindow>/btnbottom", Key, Found);
      Set_Text
        (Gtk_Entry(Get_Object(Builder, "edtdown")),
         Accelerator_Get_Label(Key.Accel_Key, Key.Accel_Mods));
      Lookup_Entry("<skymapwindow>/btnbottomright", Key, Found);
      Set_Text
        (Gtk_Entry(Get_Object(Builder, "edtdownright")),
         Accelerator_Get_Label(Key.Accel_Key, Key.Accel_Mods));
      Lookup_Entry("<skymapwindow>/btnmoveto", Key, Found);
      Set_Text
        (Gtk_Entry(Get_Object(Builder, "edtmoveto")),
         Accelerator_Get_Label(Key.Accel_Key, Key.Accel_Mods));
      Show_All(Gtk_Widget(Get_Object(Builder, "optionswindow")));
   end ShowGameOptions;

end GameOptions;
