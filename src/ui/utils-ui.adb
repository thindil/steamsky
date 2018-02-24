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
with Gtk.Widget; use Gtk.Widget;
with MainMenu; use MainMenu;
with Game; use Game;
with Messages; use Messages;

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
          ("Are you sure want to quit game?",
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

end Utils.UI;
