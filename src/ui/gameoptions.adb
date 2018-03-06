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
with Gtk.Window; use Gtk.Window;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Glib.Object; use Glib.Object;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Game; use Game;
with Maps.UI; use Maps.UI;
with Config; use Config;
with Ships; use Ships;
with Utils.UI; use Utils.UI;

package body GameOptions is

   Builder: Gtkada_Builder;
   AccelNames: constant array(Positive range <>) of Unbounded_String :=
     (To_Unbounded_String("<skymapwindow>/btnupleft"),
      To_Unbounded_String("<skymapwindow>/btnup"),
      To_Unbounded_String("<skymapwindow>/btnupright"),
      To_Unbounded_String("<skymapwindow>/btnleft"),
      To_Unbounded_String("<skymapwindow>/btnmovewait"),
      To_Unbounded_String("<skymapwindow>/btnright"),
      To_Unbounded_String("<skymapwindow>/btnbottomleft"),
      To_Unbounded_String("<skymapwindow>/btnbottom"),
      To_Unbounded_String("<skymapwindow>/btnbottomright"),
      To_Unbounded_String("<skymapwindow>/btnmoveto"),
      To_Unbounded_String("<skymapwindow>/Menu/ShipInfo"),
      To_Unbounded_String("<skymapwindow>/Menu/ShipCargoInfo"),
      To_Unbounded_String("<skymapwindow>/Menu/CrewInfo"),
      To_Unbounded_String("<skymapwindow>/Menu/ShipOrders"),
      To_Unbounded_String("<skymapwindow>/Menu/CraftInfo"),
      To_Unbounded_String("<skymapwindow>/Menu/MessagesInfo"),
      To_Unbounded_String("<skymapwindow>/Menu/BasesInfo"),
      To_Unbounded_String("<skymapwindow>/Menu/EventsInfo"),
      To_Unbounded_String("<skymapwindow>/Menu/MissionsInfo"),
      To_Unbounded_String("<skymapwindow>/Menu/MoveMap"),
      To_Unbounded_String("<skymapwindow>/Menu/GameStats"),
      To_Unbounded_String("<skymapwindow>/Menu/Help"),
      To_Unbounded_String("<skymapwindow>/Menu/GameOptions"),
      To_Unbounded_String("<skymapwindow>/Menu/QuitGame"),
      To_Unbounded_String("<skymapwindow>/Menu/ResignFromGame"),
      To_Unbounded_String("<skymapwindow>/Menu"),
      To_Unbounded_String("<skymapwindow>/Menu/WaitOrders"));
   EditNames: constant array(Positive range <>) of Unbounded_String :=
     (To_Unbounded_String("edtupleft"),
      To_Unbounded_String("edtup"),
      To_Unbounded_String("edtupright"),
      To_Unbounded_String("edtleft"),
      To_Unbounded_String("edtmovewait"),
      To_Unbounded_String("edtright"),
      To_Unbounded_String("edtdownleft"),
      To_Unbounded_String("edtdown"),
      To_Unbounded_String("edtdownright"),
      To_Unbounded_String("edtmoveto"),
      To_Unbounded_String("edtshipinfo"),
      To_Unbounded_String("edtcargo"),
      To_Unbounded_String("edtcrew"),
      To_Unbounded_String("edtorders"),
      To_Unbounded_String("edtcrafts"),
      To_Unbounded_String("edtmessages"),
      To_Unbounded_String("edtbases"),
      To_Unbounded_String("edtevents"),
      To_Unbounded_String("edtmissions"),
      To_Unbounded_String("edtmap"),
      To_Unbounded_String("edtgamestats"),
      To_Unbounded_String("edthelp"),
      To_Unbounded_String("edtgameoptions"),
      To_Unbounded_String("edtquit"),
      To_Unbounded_String("edtresign"),
      To_Unbounded_String("edtmenu"),
      To_Unbounded_String("edtwaitorders"));

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
      Changed, Found: Boolean := False;
      Key: Gtk_Accel_Key;
   begin
      for I in AccelNames'Range loop
         Lookup_Entry(To_String(AccelNames(I)), Key, Found);
         if Key.Accel_Key = Event.Keyval and Key.Accel_Mods = KeyMods then
            ShowDialog
              ("You can't set this key because it is set for other action. Please choice another key.",
               Gtk_Window(Get_Object(Builder, "optionswindow")));
            return False;
         end if;
      end loop;
      for I in EditNames'Range loop
         if Self =
           Gtk_Widget(Get_Object(Builder, To_String(EditNames(I)))) then
            Changed :=
              Change_Entry
                (To_String(AccelNames(I)),
                 Event.Keyval,
                 KeyMods,
                 True);
            exit;
         end if;
      end loop;
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
      for I in EditNames'Range loop
         On_Key_Press_Event
           (Gtk_Widget(Get_Object(Builder, To_String(EditNames(I)))),
            SetAccelerator'Access);
      end loop;
      On_Key_Release_Event
        (Gtk_Widget(Get_Object(Builder, "optionswindow")),
         CloseWindow'Access);
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
      for I in EditNames'Range loop
         Lookup_Entry(To_String(AccelNames(I)), Key, Found);
         Set_Text
           (Gtk_Entry(Get_Object(Builder, To_String(EditNames(I)))),
            Accelerator_Get_Label(Key.Accel_Key, Key.Accel_Mods));
      end loop;
      Show_All(Gtk_Widget(Get_Object(Builder, "optionswindow")));
   end ShowGameOptions;

end GameOptions;
