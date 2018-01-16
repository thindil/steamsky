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
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Window; use Gtk.Window;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Game; use Game;
with MainMenu; use MainMenu;
with Utils.UI; use Utils.UI;

package body Maps.UI is

   Builder: Gtkada_Builder;

   function QuitGame
     (Object: access Gtkada_Builder_Record'Class) return Boolean is
      SkyMapWindow: constant Gtk_Window :=
        Gtk_Window(Get_Object(Object, "skymapwindow"));
      MessageDialog: constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New
          (SkyMapWindow,
           Modal,
           Message_Question,
           Buttons_Yes_No,
           "Are you sure want to quit game?");
   begin
      if Run(MessageDialog) = Gtk_Response_Yes then
         EndGame(True);
         Destroy(MessageDialog);
         ShowMainMenu;
         return Hide_On_Delete(Gtk_Widget(SkyMapWindow));
      end if;
      Destroy(MessageDialog);
      return True;
   end QuitGame;

   procedure QuitGameMenu(Object: access Gtkada_Builder_Record'Class) is
      Finished: constant Boolean := QuitGame(Object);
   begin
      if not Finished then
         ShowDialog
           ("Can't quit game.",
            Gtk_Window(Get_Object(Object, "skymapwindow")));
      end if;
   end QuitGameMenu;

   procedure CreateSkyMap is
      Error: aliased GError;
   begin
      if Builder /= null then
         Show_All(Gtk_Widget(Get_Object(Builder, "skymapwindow")));
         Hide(Gtk_Widget(Get_Object(Builder, "lblnofuel")));
         Hide(Gtk_Widget(Get_Object(Builder, "lblnofood")));
         Hide(Gtk_Widget(Get_Object(Builder, "lblnodrink")));
         return;
      end if;
      Gtk_New(Builder);
      if Add_From_File
          (Builder,
           To_String(DataDirectory) & "ui" & Dir_Separator & "skymap.glade",
           Error'Access) =
        Guint(0) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      Register_Handler(Builder, "Quit_Game", QuitGame'Access);
      Register_Handler(Builder, "Quit_Game_Menu", QuitGameMenu'Access);
      Do_Connect(Builder);
      Show_All(Gtk_Widget(Get_Object(Builder, "skymapwindow")));
      Hide(Gtk_Widget(Get_Object(Builder, "lblnofuel")));
      Hide(Gtk_Widget(Get_Object(Builder, "lblnofood")));
      Hide(Gtk_Widget(Get_Object(Builder, "lblnodrink")));
   end CreateSkyMap;

end Maps.UI;
