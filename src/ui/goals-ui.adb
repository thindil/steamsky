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
with Gtk.Widget; use Gtk.Widget;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Glib.Object; use Glib.Object;
with Game; use Game;

package body Goals.UI is

   function HideGoals(User_Data: access Gtkada_Builder_Record'Class) return Boolean is
   begin
      return Hide_On_Delete(Gtk_Widget(Get_Object(User_Data, "goalswindow")));
   end HideGoals;

   procedure ShowGoalsMenu(Object: access Gtkada_Builder_Record'Class) is
   begin
      Show_All(Gtk_Widget(Get_Object(Object, "goalswindow")));
   end ShowGoalsMenu;

   procedure CreateGoalsMenu(Builder: Gtkada_Builder) is
      Error: aliased GError;
   begin
      if Add_From_File
          (Builder,
           To_String(DataDirectory) & "ui" & Dir_Separator & "goals.glade",
           Error'Access) =
        Guint(0) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      Register_Handler(Builder, "Show_Goals", ShowGoalsMenu'Access);
      Register_Handler(Builder, "Hide_Goals", HideGoals'Access);
   end CreateGoalsMenu;

end Goals.UI;
