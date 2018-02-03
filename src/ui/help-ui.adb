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
with Gtk.Label; use Gtk.Label;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Menu_Shell; use Gtk.Menu_Shell;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Game; use Game;
with Utils.UI; use Utils.UI;

package body Help.UI is

   Builder: Gtkada_Builder;

   procedure SelectTopic(Self: access Gtk_Menu_Item_Record'Class) is
      TopicName: constant Unbounded_String :=
        To_Unbounded_String(Get_Label(Self));
   begin
      for I in Help_List.Iterate loop
         if TopicName = Help_List(I).Title then
            ShowHelpUI(Help_Container.To_Index(I));
            exit;
         end if;
      end loop;
   end SelectTopic;

   procedure CreateHelpUI is
      Error: aliased GError;
      MenuTopic: Gtk_Menu_Item;
      TopicList: Gtk_Menu_Shell;
   begin
      if Builder /= null then
         return;
      end if;
      Gtk_New(Builder);
      if Add_From_File
          (Builder,
           To_String(DataDirectory) & "ui" & Dir_Separator & "help.glade",
           Error'Access) =
        Guint(0) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      TopicList := Gtk_Menu_Shell(Get_Object(Builder, "helpmenu"));
      for Help of Help_List loop
         Gtk_New_With_Label(MenuTopic, To_String(Help.Title));
         Append(TopicList, MenuTopic);
         Show(MenuTopic);
         On_Activate(MenuTopic, SelectTopic'Access);
      end loop;
      Register_Handler(Builder, "Hide_Window", HideWindow'Access);
      Do_Connect(Builder);
   end CreateHelpUI;

   procedure ShowHelpUI(Topic: Positive) is
   begin
      Set_Text
        (Gtk_Label(Get_Object(Builder, "lblhelptopic")),
         To_String(Help_List(Topic).Title));
      Set_Text
        (Gtk_Text_Buffer(Get_Object(Builder, "helpbuffer")),
         To_String(Help_List(Topic).Text));
      Show_All(Gtk_Widget(Get_Object(Builder, "helpwindow")));
   end ShowHelpUI;

end Help.UI;
