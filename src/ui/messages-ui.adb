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

with Gtk.Widget; use Gtk.Widget;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Text_Iter; use Gtk.Text_Iter;
with Gtk.Text_Tag_Table; use Gtk.Text_Tag_Table;
with Gtk.Window; use Gtk.Window;
with Gtk.Stack; use Gtk.Stack;

package body Messages.UI is

   Builder: Gtkada_Builder;

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

   procedure CreateMessagesUI(NewBuilder: Gtkada_Builder) is
   begin
      Builder := NewBuilder;
      Register_Handler(Builder, "Select_Messages", SelectMessages'Access);
      Register_Handler(Builder, "Delete_Messages", DeleteMessages'Access);
      Register_Handler(Builder, "Close_Messages", CloseMessages'Access);
   end CreateMessagesUI;

   procedure ShowMessagesUI(OldState: GameStates) is
   begin
      PreviousGameState := OldState;
      ShowMessages(Default);
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")),
         "lastmessages");
      Set_Deletable(Gtk_Window(Get_Object(Builder, "skymapwindow")), False);
   end ShowMessagesUI;

end Messages.UI;
