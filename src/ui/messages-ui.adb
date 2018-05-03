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
with Gtk.Window; use Gtk.Window;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;

package body Messages.UI is

   Builder: Gtkada_Builder;

   procedure ShowMessages(MessagesType: Message_Type) is
      MessagesList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "messageslist"));
      MessagesIter: Gtk_Tree_Iter;
   begin
      Clear(MessagesList);
      if MessagesAmount(MessagesType) = 0 then
         Append(MessagesList, MessagesIter);
         Set
           (MessagesList,
            MessagesIter,
            0,
            "There are no messages of that type.");
      else
         for Message of reverse Messages_List loop
            if Message.MType = MessagesType or MessagesType = Default then
               Append(MessagesList, MessagesIter);
               case Message.Color is
                  when 1 =>
                     Set
                       (MessagesList,
                        MessagesIter,
                        0,
                        "<span foreground=""yellow"">" &
                        To_String(Message.Message) &
                        "</span>");
                  when 2 =>
                     Set
                       (MessagesList,
                        MessagesIter,
                        0,
                        "<span foreground=""#4E9A06"">" &
                        To_String(Message.Message) &
                        "</span>");
                  when 3 =>
                     Set
                       (MessagesList,
                        MessagesIter,
                        0,
                        "<span foreground=""red"">" &
                        To_String(Message.Message) &
                        "</span>");
                  when 4 =>
                     Set
                       (MessagesList,
                        MessagesIter,
                        0,
                        "<span foreground=""#3465A4"">" &
                        To_String(Message.Message) &
                        "</span>");
                  when 5 =>
                     Set
                       (MessagesList,
                        MessagesIter,
                        0,
                        "<span foreground=""cyan"">" &
                        To_String(Message.Message) &
                        "</span>");
                  when others =>
                     Set
                       (MessagesList,
                        MessagesIter,
                        0,
                        To_String(Message.Message));
               end case;
            end if;
         end loop;
      end if;
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
          ("Are you sure you want to clear all messages?",
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
      Grab_Focus(Gtk_Widget(Get_Object(Builder, "cmbmessages")));
   end ShowMessagesUI;

end Messages.UI;
