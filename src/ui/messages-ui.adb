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
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Text_Iter; use Gtk.Text_Iter;
with Gtk.Text_Tag_Table; use Gtk.Text_Tag_Table;
with Gtk.Window; use Gtk.Window;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Gdk.RGBA; use Gdk.RGBA;
with Game; use Game;
with Maps.UI; use Maps.UI;
with Combat.UI; use Combat.UI;

package body Messages.UI is

   Builder: Gtkada_Builder;
   GameState: GameStates;

   function HideMessages
     (Object: access Gtkada_Builder_Record'Class) return Boolean is
   begin
      Hide(Gtk_Widget(Get_Object(Object, "messageswindow")));
      case GameState is
         when SkyMap_View =>
            CreateSkyMap;
         when Combat_View =>
            ShowCombatUI;
         when others =>
            null;
      end case;
      return True;
   end HideMessages;

   procedure ShowMessages(MessagesType: Message_Type) is
      MessagesBuffer: constant Gtk_Text_Buffer :=
        Gtk_Text_Buffer(Get_Object(Builder, "txtmessages"));
      MessagesIter: Gtk_Text_Iter;
      TagNames: constant array(1 .. 5) of Unbounded_String :=
        (To_Unbounded_String("yellow"),
         To_Unbounded_String("green"),
         To_Unbounded_String("red"),
         To_Unbounded_String("blue"),
         To_Unbounded_String("cyan"));
   begin
      if MessagesAmount(MessagesType) = 0 then
         Set_Text(MessagesBuffer, "There no messages of that type.");
      else
         Set_Text(MessagesBuffer, "");
         Get_Start_Iter(MessagesBuffer, MessagesIter);
         for Message of reverse Messages_List loop
            if Message.MType = MessagesType or MessagesType = Default then
               if Message.Color = 0 then
                  Insert
                    (MessagesBuffer,
                     MessagesIter,
                     To_String(Message.Message));
               else
                  Insert_With_Tags
                    (MessagesBuffer,
                     MessagesIter,
                     To_String(Message.Message),
                     Lookup
                       (Get_Tag_Table(MessagesBuffer),
                        To_String(TagNames(Message.Color))));
               end if;
               Insert(MessagesBuffer, MessagesIter, "" & ASCII.LF);
            end if;
         end loop;
      end if;
      null;
   end ShowMessages;

   procedure SelectMessages(Object: access Gtkada_Builder_Record'Class) is
   begin
      ShowMessages
        (Message_Type'Val
           (Get_Active(Gtk_Combo_Box(Get_Object(Object, "cmbmessages")))));
   end SelectMessages;

   procedure DeleteMessages(Object: access Gtkada_Builder_Record'Class) is
   begin
      if ShowConfirmDialog
          ("Are you sure want to clear all messages?",
           Gtk_Window(Get_Object(Object, "messageswindow"))) then
         ClearMessages;
         ShowMessages(Default);
      end if;
   end DeleteMessages;

   procedure CreateMessagesUI is
      Error: aliased GError;
   begin
      if Builder /= null then
         return;
      end if;
      Gtk_New(Builder);
      if Add_From_File
          (Builder,
           To_String(DataDirectory) & "ui" & Dir_Separator & "messages.glade",
           Error'Access) =
        Guint(0) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      Override_Background_Color
        (Gtk_Widget(Get_Object(Builder, "messagesview")),
         0,
         Black_RGBA);
      Override_Color
        (Gtk_Widget(Get_Object(Builder, "messagesview")),
         0,
         White_RGBA);
      Register_Handler(Builder, "Hide_Window", HideMessages'Access);
      Register_Handler(Builder, "Select_Messages", SelectMessages'Access);
      Register_Handler(Builder, "Delete_Messages", DeleteMessages'Access);
      Do_Connect(Builder);
   end CreateMessagesUI;

   procedure ShowMessagesUI(OldState: GameStates) is
   begin
      GameState := OldState;
      ShowMessages(Default);
      Show_All(Gtk_Widget(Get_Object(Builder, "messageswindow")));
   end ShowMessagesUI;

end Messages.UI;
