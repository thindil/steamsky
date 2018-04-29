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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Label; use Gtk.Label;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Progress_Bar; use Gtk.Progress_Bar;
with Gdk.Types; use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with MainMenu; use MainMenu;
with Game; use Game;
with Messages; use Messages;
with Maps.UI; use Maps.UI;
with Combat.UI; use Combat.UI;
with GameOptions; use GameOptions;
with Help.UI; use Help.UI;

package body Utils.UI is

   procedure ShowDialog(Message: String; Parent: Gtk_Window) is
      MessageDialog: constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New
          (Parent,
           Modal,
           Message_Error,
           Buttons_Close,
           Message);
   begin
      if Run(MessageDialog) /= Gtk_Response_None then
         Destroy(MessageDialog);
      end if;
   end ShowDialog;

   function HideWindow
     (User_Data: access GObject_Record'Class) return Boolean is
   begin
      return Hide_On_Delete(Gtk_Widget(User_Data));
   end HideWindow;

   procedure ShowWindow(User_Data: access GObject_Record'Class) is
   begin
      Show_All(Gtk_Widget(User_Data));
   end ShowWindow;

   function ShowConfirmDialog
     (Message: String;
      Parent: Gtk_Window) return Boolean is
      MessageDialog: constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New
          (Parent,
           Modal,
           Message_Question,
           Buttons_Yes_No,
           Message);
   begin
      if Run(MessageDialog) = Gtk_Response_Yes then
         Destroy(MessageDialog);
         return True;
      end if;
      Destroy(MessageDialog);
      return False;
   end ShowConfirmDialog;

   function QuitGame(User_Data: access GObject_Record'Class) return Boolean is
   begin
      if ShowConfirmDialog
          ("Are you sure want to quit?",
           Gtk_Window(User_Data)) then
         EndGame(True);
         ShowMainMenu;
         return Hide_On_Delete(Gtk_Widget(User_Data));
      end if;
      return True;
   end QuitGame;

   procedure HideLastMessage(Object: access Gtkada_Builder_Record'Class) is
   begin
      Hide(Gtk_Widget(Get_Object(Object, "infolastmessage")));
      LastMessage := Null_Unbounded_String;
   end HideLastMessage;

   procedure ShowLastMessage(Object: access Gtkada_Builder_Record'Class) is
   begin
      if LastMessage = Null_Unbounded_String then
         HideLastMessage(Object);
      else
         Set_Text
           (Gtk_Label(Get_Object(Object, "lbllastmessage")),
            To_String(LastMessage));
         Show_All(Gtk_Widget(Get_Object(Object, "infolastmessage")));
         LastMessage := Null_Unbounded_String;
      end if;
      UpdateHeader;
   end ShowLastMessage;

   function CloseWindow
     (Self: access Gtk_Widget_Record'Class;
      Event: Gdk_Event_Key) return Boolean is
      KeyMods: constant Gdk_Modifier_Type :=
        Event.State and Get_Default_Mod_Mask;
   begin
      if KeyMods = 0 and Event.Keyval = GDK_Escape then
         Close(Gtk_Window(Self));
         return False;
      end if;
      return True;
   end CloseWindow;

   procedure CloseMessages(Object: access Gtkada_Builder_Record'Class) is
   begin
      if Get_Visible_Child_Name(Gtk_Stack(Get_Object(Object, "gamestack"))) =
        "options" then
         CloseOptions(Object);
         return;
      end if;
      if Get_Visible_Child_Name(Gtk_Stack(Get_Object(Object, "gamestack"))) =
        "inventory" then
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Object, "gamestack")),
            "crew");
         return;
      end if;
      Hide(Gtk_Widget(Get_Object(Object, "btnclose")));
      Hide(Gtk_Widget(Get_Object(Object, "btnshowhelp")));
      case PreviousGameState is
         when SkyMap_View =>
            CreateSkyMap;
            Set_Deletable
              (Gtk_Window(Get_Object(Object, "skymapwindow")),
               True);
            Set_Visible_Child_Name
              (Gtk_Stack(Get_Object(Object, "gamestack")),
               "skymap");
            Show_All(Gtk_Widget(Get_Object(Object, "btnmenu")));
         when Combat_View =>
            ShowCombatUI(False);
         when others =>
            null;
      end case;
   end CloseMessages;

   procedure ShowItemDamage(ItemDurability: Natural; DamageBar: GObject) is
      DamagePercent: Gdouble;
   begin
      if ItemDurability < 100 then
         DamagePercent := 1.0 - (Gdouble(ItemDurability) / 100.0);
         Set_Visible(Gtk_Widget(DamageBar), True);
         Set_Fraction(Gtk_Progress_Bar(DamageBar), DamagePercent);
         if DamagePercent < 0.2 then
            Set_Text(Gtk_Progress_Bar(DamageBar), "Slightly used");
         elsif DamagePercent < 0.5 then
            Set_Text(Gtk_Progress_Bar(DamageBar), "Damaged");
         elsif DamagePercent < 0.8 then
            Set_Text(Gtk_Progress_Bar(DamageBar), "Heavily damaged");
         else
            Set_Text(Gtk_Progress_Bar(DamageBar), "Almost destroyed");
         end if;
      else
         Set_Visible(Gtk_Widget(DamageBar), False);
      end if;
   end ShowItemDamage;

   procedure ShowHelp(Object: access Gtkada_Builder_Record'Class) is
   begin
      if Get_Visible_Child_Name(Gtk_Stack(Get_Object(Object, "gamestack"))) =
        "combat" then
         ShowHelpUI(4);
      elsif Get_Visible_Child_Name
          (Gtk_Stack(Get_Object(Object, "gamestack"))) =
        "crafts" then
         ShowHelpUI(5);
      elsif Get_Visible_Child_Name
          (Gtk_Stack(Get_Object(Object, "gamestack"))) =
        "crew" then
         ShowHelpUI(7);
      elsif Get_Visible_Child_Name
          (Gtk_Stack(Get_Object(Object, "gamestack"))) =
        "ship" then
         ShowHelpUI(6);
      elsif Get_Visible_Child_Name
          (Gtk_Stack(Get_Object(Object, "gamestack"))) =
        "trade" then
         ShowHelpUI(3);
      end if;
   end ShowHelp;

end Utils.UI;
