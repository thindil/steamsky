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

with Gtk.Builder; use Gtk.Builder;
with Gtk.Widget; use Gtk.Widget;
with Game; use Game;

package body MainMenu is

   Builder: Gtk_Builder;
   
   procedure CreateMainMenu is
   begin
      Initialize_From_File(Builder, To_String(DataDirectory) & "ui/mainmenu.glade");
      Show_All(Gtk_Widget(Get_Object(Builder, "mainmenuwindow")));
   end CreateMainMenu;

   procedure ShowErrorInfo(Message: Unbounded_String) is
   begin
      null;
   end ShowErrorInfo;

end MainMenu;
