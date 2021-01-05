-- Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Time_Stamp; use GNAT.Time_Stamp;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TopLevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.TopLevel.MainWindow; use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Game; use Game;
with Game.SaveLoad; use Game.SaveLoad;
with Log; use Log;
with MainMenu.Commands; use MainMenu.Commands;
with Ships; use Ships;
with Utils.UI; use Utils.UI;

package body ErrorDialog is

   procedure SaveException(An_Exception: Exception_Occurrence) is
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
      Append(ErrorText, Current_Time & LF);
      Append(ErrorText, GameVersion & LF);
      Append(ErrorText, "Exception: " & Exception_Name(An_Exception) & LF);
      Append(ErrorText, "Message: " & Exception_Message(An_Exception) & LF);
      Append
        (ErrorText, "-------------------------------------------------" & LF);
      Append(ErrorText, Symbolic_Traceback(An_Exception) & LF);
      Append(ErrorText, "-------------------------------------------------");
      Put_Line(ErrorFile, To_String(ErrorText));
      Close(ErrorFile);
      EndLogging;
      declare
         use type Interfaces.C.int;

         Interp: Tcl.Tcl_Interp := Get_Context;
         Text: Tk_Text;
         MainWindow: Tk_TopLevel := Get_Main_Window(Interp);
      begin
         begin
            Destroy(MainWindow);
         exception
            when STORAGE_ERROR =>
               null;
         end;
         Interp := Tcl.Tcl_CreateInterp;
         if Tcl.Tcl_Init(Interp) = Tcl.TCL_ERROR then
            Ada.Text_IO.Put_Line
              ("Steam Sky: Tcl.Tcl_Init failed: " &
               Tcl.Ada.Tcl_GetStringResult(Interp));
            return;
         end if;
         if Tcl.Tk.Tk_Init(Interp) = Tcl.TCL_ERROR then
            Ada.Text_IO.Put_Line
              ("Steam Sky: Tcl.Tk.Tk_Init failed: " &
               Tcl.Ada.Tcl_GetStringResult(Interp));
            return;
         end if;
         Set_Context(Interp);
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator &
            "errordialog.tcl");
         AddCommand("OpenLink", Open_Link_Command'Access);
         Text := Get_Widget(".technical.text", Interp);
         Insert(Text, "end", "{" & To_String(ErrorText) & "}");
         configure(Text, "-state disabled");
         Tcl.Tk.Tk_MainLoop;
      end;
   end SaveException;

end ErrorDialog;
