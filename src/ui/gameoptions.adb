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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Switch; use Gtk.Switch;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Accel_Map; use Gtk.Accel_Map;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Settings; use Gtk.Settings;
with Gtk.Label; use Gtk.Label;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Game; use Game;
with Maps.UI; use Maps.UI;
with Config; use Config;
with Ships; use Ships;
with Utils.UI; use Utils.UI;
with Messages; use Messages;
with Themes; use Themes;

package body GameOptions is

   Builder: Gtkada_Builder;
   type Accel_Data is record
      AccelName: Unbounded_String;
      EntryName: Unbounded_String;
   end record;
   Accels: constant array(Positive range <>) of Accel_Data :=
     (1 =>
        (To_Unbounded_String("<skymapwindow>/btnupleft"),
         To_Unbounded_String("edtupleft")),
      2 =>
        (To_Unbounded_String("<skymapwindow>/btnup"),
         To_Unbounded_String("edtup")),
      3 =>
        (To_Unbounded_String("<skymapwindow>/btnupright"),
         To_Unbounded_String("edtupright")),
      4 =>
        (To_Unbounded_String("<skymapwindow>/btnleft"),
         To_Unbounded_String("edtleft")),
      5 =>
        (To_Unbounded_String("<skymapwindow>/btnmovewait"),
         To_Unbounded_String("edtmovewait")),
      6 =>
        (To_Unbounded_String("<skymapwindow>/btnright"),
         To_Unbounded_String("edtright")),
      7 =>
        (To_Unbounded_String("<skymapwindow>/btnbottomleft"),
         To_Unbounded_String("edtdownleft")),
      8 =>
        (To_Unbounded_String("<skymapwindow>/btnbottom"),
         To_Unbounded_String("edtdown")),
      9 =>
        (To_Unbounded_String("<skymapwindow>/btnbottomright"),
         To_Unbounded_String("edtdownright")),
      10 =>
        (To_Unbounded_String("<skymapwindow>/btnmoveto"),
         To_Unbounded_String("edtmoveto")),
      11 =>
        (To_Unbounded_String("<skymapwindow>/Menu/ShipInfo"),
         To_Unbounded_String("edtshipinfo")),
      12 =>
        (To_Unbounded_String("<skymapwindow>/Menu/ShipCargoInfo"),
         To_Unbounded_String("edtcargo")),
      13 =>
        (To_Unbounded_String("<skymapwindow>/Menu/CrewInfo"),
         To_Unbounded_String("edtcrew")),
      14 =>
        (To_Unbounded_String("<skymapwindow>/Menu/ShipOrders"),
         To_Unbounded_String("edtorders")),
      15 =>
        (To_Unbounded_String("<skymapwindow>/Menu/CraftInfo"),
         To_Unbounded_String("edtcrafts")),
      16 =>
        (To_Unbounded_String("<skymapwindow>/Menu/MessagesInfo"),
         To_Unbounded_String("edtmessages")),
      17 =>
        (To_Unbounded_String("<skymapwindow>/Menu/BasesInfo"),
         To_Unbounded_String("edtbases")),
      18 =>
        (To_Unbounded_String("<skymapwindow>/Menu/EventsInfo"),
         To_Unbounded_String("edtevents")),
      19 =>
        (To_Unbounded_String("<skymapwindow>/Menu/MissionsInfo"),
         To_Unbounded_String("edtmissions")),
      20 =>
        (To_Unbounded_String("<skymapwindow>/Menu/MoveMap"),
         To_Unbounded_String("edtmap")),
      21 =>
        (To_Unbounded_String("<skymapwindow>/Menu/GameStats"),
         To_Unbounded_String("edtgamestats")),
      22 =>
        (To_Unbounded_String("<skymapwindow>/Menu/Help"),
         To_Unbounded_String("edthelp")),
      23 =>
        (To_Unbounded_String("<skymapwindow>/Menu/GameOptions"),
         To_Unbounded_String("edtgameoptions")),
      24 =>
        (To_Unbounded_String("<skymapwindow>/Menu/QuitGame"),
         To_Unbounded_String("edtquit")),
      25 =>
        (To_Unbounded_String("<skymapwindow>/Menu/ResignFromGame"),
         To_Unbounded_String("edtresign")),
      26 =>
        (To_Unbounded_String("<skymapwindow>/Menu"),
         To_Unbounded_String("edtmenu")),
      27 =>
        (To_Unbounded_String("<skymapwindow>/Menu/WaitOrders"),
         To_Unbounded_String("edtwaitorders")),
      28 =>
        (To_Unbounded_String("<movemapwindow>/btncenter"),
         To_Unbounded_String("edtcentermap")),
      29 =>
        (To_Unbounded_String("<skymapwindow>/btnmapleft"),
         To_Unbounded_String("edtmovemapleft")),
      30 =>
        (To_Unbounded_String("<skymapwindow>/btnmapright"),
         To_Unbounded_String("edtmovemapright")),
      31 =>
        (To_Unbounded_String("<skymapwindow>/btnmapup"),
         To_Unbounded_String("edtmovemapup")),
      32 =>
        (To_Unbounded_String("<skymapwindow>/btnmapdown"),
         To_Unbounded_String("edtmovemapdown")),
      33 =>
        (To_Unbounded_String("<skymapwindow>/cursorupleft"),
         To_Unbounded_String("edtmovecursorupleft")),
      34 =>
        (To_Unbounded_String("<skymapwindow>/cursorup"),
         To_Unbounded_String("edtmovecursorup")),
      35 =>
        (To_Unbounded_String("<skymapwindow>/cursorupright"),
         To_Unbounded_String("edtmovecursorupright")),
      36 =>
        (To_Unbounded_String("<skymapwindow>/cursorleft"),
         To_Unbounded_String("edtmovecursorleft")),
      37 =>
        (To_Unbounded_String("<skymapwindow>/cursorright"),
         To_Unbounded_String("edtmovecursorright")),
      38 =>
        (To_Unbounded_String("<skymapwindow>/cursordownleft"),
         To_Unbounded_String("edtmovecursordownleft")),
      39 =>
        (To_Unbounded_String("<skymapwindow>/cursordown"),
         To_Unbounded_String("edtmovecursordown")),
      40 =>
        (To_Unbounded_String("<skymapwindow>/cursordownright"),
         To_Unbounded_String("edtmovecursordownright")),
      41 =>
        (To_Unbounded_String("<skymapwindow>/mouseclick"),
         To_Unbounded_String("edtclickmouse")),
      42 =>
        (To_Unbounded_String("<skymapwindow>/Menu/Stories"),
         To_Unbounded_String("edtstories")),
      43 =>
        (To_Unbounded_String("<skymapwindow>/zoomin"),
         To_Unbounded_String("edtzoomin")),
      44 =>
        (To_Unbounded_String("<skymapwindow>/zoomout"),
         To_Unbounded_String("edtzoomout")),
      45 =>
        (To_Unbounded_String("<movemapwindow>/btncenterhomebase"),
         To_Unbounded_String("edtcentermaphomebase")),
      46 =>
        (To_Unbounded_String("<skymapwindow>/btnmapupleft"),
         To_Unbounded_String("edtmovemapupleft")),
      47 =>
        (To_Unbounded_String("<skymapwindow>/btnmapupright"),
         To_Unbounded_String("edtmovemapupright")),
      48 =>
        (To_Unbounded_String("<skymapwindow>/btnmapdownleft"),
         To_Unbounded_String("edtmovemapdownleft")),
      49 =>
        (To_Unbounded_String("<skymapwindow>/btnmapdownright"),
         To_Unbounded_String("edtmovemapdownright")),
      50 =>
        (To_Unbounded_String("<skymapwindow>/fullstop"),
         To_Unbounded_String("edtfullstop")),
      51 =>
        (To_Unbounded_String("<skymapwindow>/quarterspeed"),
         To_Unbounded_String("edtquarterspeed")),
      52 =>
        (To_Unbounded_String("<skymapwindow>/halfspeed"),
         To_Unbounded_String("edthalfspeed")),
      53 =>
        (To_Unbounded_String("<skymapwindow>/fullspeed"),
         To_Unbounded_String("edtfullspeed")),
      54 =>
        (To_Unbounded_String("<skymapwindow>/fullscreen"),
         To_Unbounded_String("edtfullscreen")));
   Setting: Boolean := False;

   procedure CloseOptions(Object: access Gtkada_Builder_Record'Class) is
   begin
      GameSettings.AutoRest :=
        Get_State(Gtk_Switch(Get_Object(Object, "switchautorest")));
      GameSettings.UndockSpeed :=
        ShipSpeed'Val
          (Get_Active(Gtk_Combo_Box(Get_Object(Object, "cmbspeed1"))) + 1);
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
      if Get_State(Gtk_Switch(Get_Object(Object, "switchanimations"))) then
         GameSettings.AnimationsEnabled := 1;
      else
         GameSettings.AnimationsEnabled := 0;
      end if;
      Set_Long_Property
        (Get_Default, "gtk-enable-animations",
         Glong(GameSettings.AnimationsEnabled), "");
      GameSettings.AnimationType :=
        Positive
          (Get_Active(Gtk_Combo_Box(Get_Object(Object, "cmbanimations"))) + 1);
      Set_Transition_Type
        (Gtk_Stack(Get_Object(Builder, "gamestack")),
         Gtk_Stack_Transition_Type'Val(GameSettings.AnimationType));
      Set_Transition_Type
        (Gtk_Stack(Get_Object(Builder, "shipyardstack")),
         Gtk_Stack_Transition_Type'Val(GameSettings.AnimationType));
      Set_Transition_Type
        (Gtk_Stack(Get_Object(Builder, "optionsstack")),
         Gtk_Stack_Transition_Type'Val(GameSettings.AnimationType));
      Set_Transition_Type
        (Gtk_Stack(Get_Object(Builder, "combatstack")),
         Gtk_Stack_Transition_Type'Val(GameSettings.AnimationType));
      GameSettings.MessagesLimit :=
        Positive
          (Get_Value(Gtk_Adjustment(Get_Object(Object, "adjmessageslimit"))));
      if Natural(Messages_List.Length) > GameSettings.MessagesLimit then
         Messages_List.Delete_First
           (Count =>
              (Messages_List.Length - Count_Type(GameSettings.MessagesLimit)));
      end if;
      GameSettings.SavedMessages :=
        Positive
          (Get_Value(Gtk_Adjustment(Get_Object(Object, "adjsavedmessages"))));
      GameSettings.MessagesOrder :=
        MessagesOrderType'Val
          (Get_Active(Gtk_Combo_Box(Get_Object(Object, "cmbmessagesorder"))));
      GameSettings.AutoAskForBases :=
        Get_State(Gtk_Switch(Get_Object(Object, "switchautoaskforbases")));
      GameSettings.AutoAskForEvents :=
        Get_State(Gtk_Switch(Get_Object(Object, "switchautoaskforevents")));
      GameSettings.ShowTooltips :=
        Get_State(Gtk_Switch(Get_Object(Object, "switchshowtooltips")));
      GameSettings.ShowLastMessages :=
        Get_State(Gtk_Switch(Get_Object(Object, "switchshowlastmessages")));
      GameSettings.FullScreen :=
        Get_State(Gtk_Switch(Get_Object(Object, "switchfullscreen")));
      GameSettings.AutoCloseMessagesTime :=
        Positive
          (Get_Value(Gtk_Adjustment(Get_Object(Object, "adjclosemessages"))));
      SaveConfig;
      SetFontSize(ALLFONTS);
      Save(To_String(SaveDirectory) & "keys.cfg");
      ShowSkyMap;
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")), "skymap");
   end CloseOptions;

   function SetAccelerator
     (Self: access Gtk_Widget_Record'Class; Event: Gdk.Event.Gdk_Event_Key)
      return Boolean is
      KeyMods: constant Gdk_Modifier_Type :=
        Event.State and Get_Default_Mod_Mask;
      Changed, Found: Boolean := False;
      Key: Gtk_Accel_Key;
   begin
      for I in Accels'Range loop
         Lookup_Entry(To_String(Accels(I).AccelName), Key, Found);
         if Key.Accel_Key = Event.Keyval and Key.Accel_Mods = KeyMods then
            ShowDialog
              ("This key is set for other action. Please choose a different key.");
            return False;
         end if;
      end loop;
      for I in Accels'Range loop
         if Self =
           Gtk_Widget(Get_Object(Builder, To_String(Accels(I).EntryName))) then
            Changed :=
              Change_Entry
                (To_String(Accels(I).AccelName), Event.Keyval, KeyMods, True);
            exit;
         end if;
      end loop;
      if Changed then
         Set_Text
           (Gtk_Entry(Self), Accelerator_Get_Label(Event.Keyval, KeyMods));
      end if;
      return False;
   end SetAccelerator;

   procedure ResizeFont(User_Data: access GObject_Record'Class) is
   begin
      if Setting then
         return;
      end if;
      if User_Data = Get_Object(Builder, "adjhelpfont") then
         GameSettings.HelpFontSize :=
           Positive
             (Get_Value(Gtk_Adjustment(Get_Object(Builder, "adjhelpfont"))));
         SetFontSize(HELPFONT);
      elsif User_Data = Get_Object(Builder, "adjmapfont") then
         GameSettings.MapFontSize :=
           Positive
             (Get_Value(Gtk_Adjustment(Get_Object(Builder, "adjmapfont"))));
         SetFontSize(MAPFONT);
      else
         GameSettings.InterfaceFontSize :=
           Positive
             (Get_Value
                (Gtk_Adjustment(Get_Object(Builder, "adjinterfacefont"))));
         SetFontSize(INTERFACEFONT);
      end if;
   end ResizeFont;

   procedure ApplyTheme(Object: access Gtkada_Builder_Record'Class) is
   begin
      GameSettings.InterfaceTheme :=
        To_Unbounded_String
          (Get_Active_Id(Gtk_Combo_Box_Text(Get_Object(Object, "cmbtheme"))));
      SetFontSize(ALLFONTS);
      SetMapMoveButtons;
   end ApplyTheme;

   procedure SetFontsSizes is
   begin
      Setting := True;
      Set_Value
        (Gtk_Adjustment(Get_Object(Builder, "adjhelpfont")),
         Gdouble(GameSettings.HelpFontSize));
      Set_Value
        (Gtk_Adjustment(Get_Object(Builder, "adjmapfont")),
         Gdouble(GameSettings.MapFontSize));
      Set_Value
        (Gtk_Adjustment(Get_Object(Builder, "adjinterfacefont")),
         Gdouble(GameSettings.InterfaceFontSize));
      Setting := False;
   end SetFontsSizes;

   procedure SetDefaultFontSize(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
   begin
      ResetFontsSizes;
      SetFontsSizes;
      SetFontSize(ALLFONTS);
   end SetDefaultFontSize;

   function ToggleAnimationType
     (Object: access Gtkada_Builder_Record'Class) return Boolean is
   begin
      if Get_Active(Gtk_Switch(Get_Object(Builder, "switchanimations"))) then
         Show_All(Gtk_Widget(Get_Object(Object, "lblanimationstype")));
         Show_All(Gtk_Widget(Get_Object(Object, "cmbanimations")));
      else
         Hide(Gtk_Widget(Get_Object(Object, "lblanimationstype")));
         Hide(Gtk_Widget(Get_Object(Object, "cmbanimations")));
      end if;
      return False;
   end ToggleAnimationType;

   procedure CreateGameOptions(NewBuilder: Gtkada_Builder) is
   begin
      Builder := NewBuilder;
      Register_Handler(Builder, "Close_Options", CloseOptions'Access);
      Register_Handler(Builder, "Resize_Font", ResizeFont'Access);
      Register_Handler(Builder, "Apply_Theme", ApplyTheme'Access);
      Register_Handler
        (Builder, "Set_Default_Font_Size", SetDefaultFontSize'Access);
      Register_Handler
        (Builder, "Toggle_Animation_Type", ToggleAnimationType'Access);
      for I in Accels'Range loop
         On_Key_Press_Event
           (Gtk_Widget(Get_Object(Builder, To_String(Accels(I).EntryName))),
            SetAccelerator'Access);
      end loop;
      Set_Text
        (Gtk_Label(Get_Object(Builder, "lbldatadir")),
         To_String(DataDirectory));
      Set_Text
        (Gtk_Label(Get_Object(Builder, "lblsavedir")),
         To_String(SaveDirectory));
      Set_Text
        (Gtk_Label(Get_Object(Builder, "lbldocdir")), To_String(DocDirectory));
      Set_Text
        (Gtk_Label(Get_Object(Builder, "lblmodsdir")),
         To_String(ModsDirectory));
      declare
         ThemesComboBox: constant Gtk_Combo_Box_Text :=
           Gtk_Combo_Box_Text(Get_Object(NewBuilder, "cmbtheme"));
      begin
         for I in Themes_List.Iterate loop
            Append
              (ThemesComboBox, Themes_Container.Key(I),
               To_String(Themes_List(I).Name));
         end loop;
         if not Set_Active_Id
             (ThemesComboBox, To_String(GameSettings.InterfaceTheme)) then
            ShowDialog("Can't set current theme");
         end if;
      end;
   end CreateGameOptions;

   procedure ShowGameOptions is
      Key: Gtk_Accel_Key;
      Found: Boolean;
   begin
      Set_State
        (Gtk_Switch(Get_Object(Builder, "switchautorest")),
         GameSettings.AutoRest);
      Set_Active
        (Gtk_Combo_Box(Get_Object(Builder, "cmbspeed1")),
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
      for I in Accels'Range loop
         Lookup_Entry(To_String(Accels(I).AccelName), Key, Found);
         Set_Text
           (Gtk_Entry(Get_Object(Builder, To_String(Accels(I).EntryName))),
            Accelerator_Get_Label(Key.Accel_Key, Key.Accel_Mods));
      end loop;
      if GameSettings.AnimationsEnabled = 1 then
         Set_State(Gtk_Switch(Get_Object(Builder, "switchanimations")), True);
      else
         Set_State(Gtk_Switch(Get_Object(Builder, "switchanimations")), False);
      end if;
      Set_Active
        (Gtk_Combo_Box(Get_Object(Builder, "cmbanimations")),
         Gint(GameSettings.AnimationType - 1));
      Set_Value
        (Gtk_Adjustment(Get_Object(Builder, "adjmessageslimit")),
         Gdouble(GameSettings.MessagesLimit));
      Set_Value
        (Gtk_Adjustment(Get_Object(Builder, "adjsavedmessages")),
         Gdouble(GameSettings.SavedMessages));
      Set_Active
        (Gtk_Combo_Box(Get_Object(Builder, "cmbmessagesorder")),
         (MessagesOrderType'Pos(GameSettings.MessagesOrder)));
      Set_State
        (Gtk_Switch(Get_Object(Builder, "switchautoaskforbases")),
         GameSettings.AutoAskForBases);
      Set_State
        (Gtk_Switch(Get_Object(Builder, "switchautoaskforevents")),
         GameSettings.AutoAskForEvents);
      Set_State
        (Gtk_Switch(Get_Object(Builder, "switchshowtooltips")),
         GameSettings.ShowTooltips);
      Set_State
        (Gtk_Switch(Get_Object(Builder, "switchshowlastmessages")),
         GameSettings.ShowLastMessages);
      Set_State
        (Gtk_Switch(Get_Object(Builder, "switchfullscreen")),
         GameSettings.FullScreen);
      Set_Value
        (Gtk_Adjustment(Get_Object(Builder, "adjclosemessages")),
         Gdouble(GameSettings.AutoCloseMessagesTime));
      SetFontsSizes;
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")), "options");
      Hide(Gtk_Widget(Get_Object(Builder, "lastmessagesframe")));
      Hide(Gtk_Widget(Get_Object(Builder, "btnmenu")));
      if not Get_Active
          (Gtk_Switch(Get_Object(Builder, "switchanimations"))) then
         Hide(Gtk_Widget(Get_Object(Builder, "lblanimationstype")));
         Hide(Gtk_Widget(Get_Object(Builder, "cmbanimations")));
      end if;
   end ShowGameOptions;

end GameOptions;
