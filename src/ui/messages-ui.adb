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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Window; use Gtk.Window;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Gtk.GEntry; use Gtk.GEntry;
with Glib.Object; use Glib.Object;
with Config; use Config;
with Utils.UI; use Utils.UI;

package body Messages.UI is

   Builder: Gtkada_Builder;

   procedure ShowMessages(MessagesType: Message_Type) is
      MessagesList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "messageslist"));
      MessagesIter: Gtk_Tree_Iter;
      procedure ShowMessage(Message: Message_Data) is
      begin
         if Message.MType = MessagesType or MessagesType = Default then
            Append(MessagesList, MessagesIter);
            case Message.Color is
               when YELLOW =>
                  Set
                    (MessagesList, MessagesIter, 0,
                     "<span foreground=""yellow"">" &
                     To_String(Message.Message) & "</span>");
               when GREEN =>
                  Set
                    (MessagesList, MessagesIter, 0,
                     "<span foreground=""#4E9A06"">" &
                     To_String(Message.Message) & "</span>");
               when RED =>
                  Set
                    (MessagesList, MessagesIter, 0,
                     "<span foreground=""red"">" & To_String(Message.Message) &
                     "</span>");
               when BLUE =>
                  Set
                    (MessagesList, MessagesIter, 0,
                     "<span foreground=""#3465A4"">" &
                     To_String(Message.Message) & "</span>");
               when CYAN =>
                  Set
                    (MessagesList, MessagesIter, 0,
                     "<span foreground=""cyan"">" &
                     To_String(Message.Message) & "</span>");
               when others =>
                  Set
                    (MessagesList, MessagesIter, 0,
                     To_String(Message.Message));
            end case;
         end if;
      end ShowMessage;
   begin
      Clear(MessagesList);
      if MessagesAmount(MessagesType) = 0 then
         Append(MessagesList, MessagesIter);
         Set
           (MessagesList, MessagesIter, 0,
            "There are no messages of that type.");
      else
         if GameSettings.MessagesOrder = OLDER_FIRST then
            for Message of Messages_List loop
               ShowMessage(Message);
            end loop;
         else
            for Message of reverse Messages_List loop
               ShowMessage(Message);
            end loop;
         end if;
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
           Gtk_Window(Get_Object(Object, "skymapwindow"))) then
         ClearMessages;
         ShowMessages(Default);
      end if;
   end DeleteMessages;

   procedure SearchMessages(Object: access Gtkada_Builder_Record'Class) is
   begin
      Refilter(Gtk_Tree_Model_Filter(Get_Object(Object, "messagesfilter")));
   end SearchMessages;

   function VisibleMessages(Model: Gtk_Tree_Model;
      Iter: Gtk_Tree_Iter) return Boolean is
      SearchEntry: constant Gtk_GEntry :=
        Gtk_GEntry(Get_Object(Builder, "entrysearch"));
   begin
      if Get_Text(SearchEntry) = "" then
         return True;
      end if;
      if Index(Get_String(Model, Iter, 0), Get_Text(SearchEntry), 1) > 0 then
         return True;
      end if;
      return False;
   end VisibleMessages;

   procedure ClearSearch(User_Data: access GObject_Record'Class) is
   begin
      Set_Text(Gtk_GEntry(User_Data), "");
   end ClearSearch;

   procedure CreateMessagesUI(NewBuilder: Gtkada_Builder) is
   begin
      Builder := NewBuilder;
      Register_Handler(Builder, "Select_Messages", SelectMessages'Access);
      Register_Handler(Builder, "Delete_Messages", DeleteMessages'Access);
      Register_Handler(Builder, "Close_Messages", CloseMessages'Access);
      Register_Handler(Builder, "Search_Messages", SearchMessages'Access);
      Register_Handler(Builder, "Clear_Search", ClearSearch'Access);
      Set_Visible_Func
        (Gtk_Tree_Model_Filter(Get_Object(Builder, "messagesfilter")),
         VisibleMessages'Access);
      On_Key_Press_Event
        (Gtk_Widget(Get_Object(Builder, "entrysearch")), SelectElement'Access,
         Get_Object(Builder, "btnmenu"));
   end CreateMessagesUI;

   procedure ShowMessagesUI is
   begin
      ShowMessages(Default);
      Set_Text(Gtk_GEntry(Get_Object(Builder, "entrysearch")), "");
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")), "lastmessages");
      Grab_Focus(Gtk_Widget(Get_Object(Builder, "cmbmessages")));
      Hide(Gtk_Widget(Get_Object(Builder, "lastmessagesframe")));
   end ShowMessagesUI;

end Messages.UI;
