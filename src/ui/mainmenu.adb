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
with Ada.Directories; use Ada.Directories;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Label; use Gtk.Label;
with Gtk.Main; use Gtk.Main;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Game; use Game;
with HallOfFame; use HallOfFame;

package body MainMenu is

   Builder: Gtkada_Builder;

   procedure Quit(Object: access Gtkada_Builder_Record'Class) is
   begin
      Unref(Object);
      Gtk.Main.Main_Quit;
   end Quit;

   procedure ShowAbout(Object: access Gtkada_Builder_Record'Class) is
   begin
      Show_All(Gtk_Widget(Get_Object(Object, "aboutdialog")));
   end ShowAbout;

   function HideAbout
     (Object: access Gtkada_Builder_Record'Class) return Boolean is
   begin
      return Hide_On_Delete(Gtk_Widget(Get_Object(Object, "aboutdialog")));
   end HideAbout;

   procedure CreateMainMenu is
      Error: aliased GError;
   begin
      Gtk_New(Builder);
      if Add_From_File
          (Builder,
           To_String(DataDirectory) & "ui/mainmenu.glade",
           Error'Access) =
        Guint(0) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      Register_Handler(Builder, "Main_Quit", Quit'Access);
      Register_Handler(Builder, "Show_About", ShowAbout'Access);
      Register_Handler(Builder, "Hide_About", HideAbout'Access);
      Do_Connect(Builder);
      Set_Label(Gtk_Label(Get_Object(Builder, "lblversion")), GameVersion);
      if not Exists(To_String(SaveDirectory) & "savegame.dat") then
         Hide(Gtk_Widget(Get_Object(Builder, "btnloadgame")));
      end if;
      if HallOfFame_Array(1).Name = Null_Unbounded_String then
         Hide(Gtk_Widget(Get_Object(Builder, "btnhalloffame")));
      end if;
      Show_All(Gtk_Widget(Get_Object(Builder, "mainmenuwindow")));
   end CreateMainMenu;

   procedure ShowErrorInfo(Message: Unbounded_String) is
      Label: constant Gtk_Label := Gtk_Label(Get_Object(Builder, "lblerror"));
   begin
      Set_Label
        (Label,
         Get_Label(Label) &
         " from '" &
         To_String(DataDirectory) &
         "' directory.");
      Set_Text
        (Gtk_Text_Buffer(Get_Object(Builder, "errorbuffer")),
         To_String(Message));
      Show_All(Gtk_Widget(Get_Object(Builder, "errordialog")));
   end ShowErrorInfo;

end MainMenu;
