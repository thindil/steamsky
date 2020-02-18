--    Copyright 2020 Bartek thindil Jasicki
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

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with Gtk.Box; use Gtk.Box;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Label; use Gtk.Label;
with Gtk.Link_Button; use Gtk.Link_Button;
with Gtk.Main;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Text_View; use Gtk.Text_View;
with Gtk.Widget; use Gtk.Widget;
with Glib; use Glib;
with Glib.Properties; use Glib.Properties;
with Game; use Game;
with Game.SaveLoad; use Game.SaveLoad;
with Log; use Log;
with Ships; use Ships;

package body ErrorDialog is

   procedure CreateErrorUI(Parent: Gtk_Window) is
      TextView: constant Gtk_Text_View := Gtk_Text_View_New;
      Scroll: constant Gtk_Scrolled_Window := Gtk_Scrolled_Window_New;
      Frame: constant Gtk_Frame := Gtk_Frame_New("Technical information:");
      Label: Gtk_Label;
      LinkButton: Gtk_Link_Button;
      ErrorBox: Gtk_Vbox;
   begin
      ErrorDialog := Gtk_Dialog_New("Steam Sky - error", Parent, Modal);
      Set_Position(ErrorDialog, Win_Pos_Center_Always);
      if not Set_Icon_From_File(ErrorDialog, "data/ui/images/icon.png") then
         Ada.Text_IO.Put_Line("Can't set icon for the error dialog.");
      end if;
      Set_Default_Size(ErrorDialog, 500, -1);
      ErrorBox := Get_Content_Area(ErrorDialog);
      Label :=
        Gtk_Label_New
          ("Oops, something bad happens and the game has crashed. Game should save your progress, but better verify this yourself. Also, please, remember what you were doing before the crash and report this problem at");
      Set_Line_Wrap(Label, True);
      Set_Property(Label, Name_Property, "normalfont");
      Pack_Start(ErrorBox, Label, False);
      LinkButton :=
        Gtk_Link_Button_New("https://github.com/thindil/steamsky/issues");
      Set_Relief(LinkButton, Relief_None);
      Set_Halign(LinkButton, Align_Center);
      Set_Property(LinkButton, Name_Property, "flatbutton");
      Pack_Start(ErrorBox, LinkButton, False);
      Label :=
        Gtk_Label_New
          ("or if you prefer, on one of the game community options:");
      Set_Line_Wrap(Label, True);
      Set_Property(Label, Name_Property, "normalfont");
      Pack_Start(ErrorBox, Label, False);
      LinkButton := Gtk_Link_Button_New("https://thindil.itch.io/steam-sky");
      Set_Relief(LinkButton, Relief_None);
      Set_Halign(LinkButton, Align_Center);
      Set_Property(LinkButton, Name_Property, "flatbutton");
      Pack_Start(ErrorBox, LinkButton, False);
      Label := Gtk_Label_New("and attach (if possible) file 'error.log'");
      Set_Line_Wrap(Label, True);
      Set_Property(Label, Name_Property, "normalfont");
      Pack_Start(ErrorBox, Label, False);
      Add(Scroll, TextView);
      Add(Frame, Scroll);
      Set_Property(Frame, Name_Property, "normalfont");
      Set_Property(TextView, Name_Property, "normalfont");
      Pack_Start(ErrorBox, Frame);
      if Add_Button(ErrorDialog, "Close", Gtk_Response_Close) = null then
         raise Program_Error with "Can't add Close button to error dialog";
      end if;
   end CreateErrorUI;

   -- ****if* ErrorDialog/ShowErrorInfo
   -- FUNCTION
   -- Show error dialog with information about occured error
   -- PARAMETERS
   -- Message - Full stack message from error
   -- SOURCE
   procedure ShowErrorInfo(Message: Unbounded_String) is
   -- ****
   begin
      Set_Text
        (Get_Buffer
           (Gtk_Text_View
              (Get_Child
                 (Gtk_Scrolled_Window
                    (Get_Child
                       (Gtk_Frame
                          (Get_Child(Get_Content_Area(ErrorDialog), 5))))))),
         To_String(Message));
      Show_All(ErrorDialog);
      if Run(ErrorDialog) /= Gtk_Response_Accept then
         Destroy(ErrorDialog);
         Gtk.Main.Main_Quit;
      end if;
   end ShowErrorInfo;

   procedure SaveException
     (An_Exception: Exception_Occurrence; PrintToTerminal: Boolean) is
      ErrorFile: File_Type;
      ErrorText: Unbounded_String;
   begin
      if Natural(PlayerShip.Crew.Length) > 0 then
         SaveGame;
      end if;
      if Exists(To_String(SaveDirectory) & "error.log") then
         Open(ErrorFile, Append_File, To_String(SaveDirectory) & "error.log");
      else
         Create
           (ErrorFile, Append_File, To_String(SaveDirectory) & "error.log");
      end if;
      Append(ErrorText, Ada.Calendar.Formatting.Image(Clock));
      Append(ErrorText, LF);
      Append(ErrorText, GameVersion);
      Append(ErrorText, LF);
      Append(ErrorText, "Exception: " & Exception_Name(An_Exception));
      Append(ErrorText, LF);
      Append(ErrorText, "Message: " & Exception_Message(An_Exception));
      Append(ErrorText, LF);
      Append(ErrorText, "-------------------------------------------------");
      Append(ErrorText, LF);
      Append(ErrorText, Symbolic_Traceback_No_Hex(An_Exception));
      Append(ErrorText, LF);
      Append(ErrorText, "-------------------------------------------------");
      Put_Line(ErrorFile, To_String(ErrorText));
      Close(ErrorFile);
      if PrintToTerminal then
         Put_Line(To_String(ErrorText));
      else
         ShowErrorInfo(ErrorText);
      end if;
      EndLogging;
   end SaveException;

   procedure On_Exception(An_Exception: Exception_Occurrence) is
   begin
      SaveException(An_Exception, False);
   end On_Exception;

end ErrorDialog;
