-- Copyright (c) 2020-2024 Bartek thindil Jasicki
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

with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Interfaces.C;
with Interfaces.C.Strings;
with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Time_Stamp;
with GNAT.Traceback.Symbolic;
with Tcl;
with Tcl.Ada;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton;
with Game;
with Game.SaveLoad;
with MainMenu.Commands;
with Ships;
with Utils.UI;

package body ErrorDialog is

   procedure Save_Exception(An_Exception: Exception_Occurrence) is
      use Ada.Characters.Latin_1;
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
      use GNAT.OS_Lib;
      use GNAT.Time_Stamp;
      use GNAT.Traceback.Symbolic;
      use Game;
      use Game.SaveLoad;
      use Ships;

      Error_File: File_Type;
      Error_Text, Error_Details: Unbounded_String := Null_Unbounded_String;
      Can_Save: Boolean := True;
   begin
      Append
        (Source => Error_Text,
         New_Item =>
           Current_Time & LF & Game_Version & LF & "Exception: " &
           Exception_Name(X => An_Exception) & LF & "Message: " &
           Exception_Message(X => An_Exception) & LF &
           "-------------------------------------------------" & LF);
      if Directory_Separator = '/' then
         Append
           (Source => Error_Details,
            New_Item => Symbolic_Traceback(E => An_Exception));
      else
         Append
           (Source => Error_Details,
            New_Item => Exception_Information(X => An_Exception));
      end if;
      if Length(Source => Error_Details) > 5 then
         Append
           (Source => Error_Details,
            New_Item => "-------------------------------------------------");
      end if;
      Open_Error_File_Block :
      begin
         Open
           (File => Error_File, Mode => Append_File,
            Name => To_String(Source => Save_Directory) & "error.log");
      exception
         when Name_Error =>
            Create
              (File => Error_File, Mode => Append_File,
               Name => To_String(Source => Save_Directory) & "error.log");
         when Use_Error =>
            Can_Save := False;
      end Open_Error_File_Block;
      if Can_Save then
         Put_Line
           (File => Error_File,
            Item => To_String(Source => Error_Text & Error_Details));
         Close(File => Error_File);
      end if;
      if Natural(Player_Ship.Crew.Length) > 0 then
         Save_Game;
      end if;
      Show_Error_Dialog_Block :
      declare
         use Interfaces.C.Strings;
         use GNAT.Directory_Operations;
         use Tcl;
         use Tcl.Ada;
         use Tcl.Tk.Ada;
         use Tcl.Tk.Ada.Widgets;
         use MainMenu.Commands;
         use Utils.UI;

         Interp: Tcl.Tcl_Interp := Get_Context;
         function Steam_Sky(Params: chars_ptr) return Tcl.Tcl_Interp with
            Import => True,
            Convention => C,
            External_Name => "steamsky";
      begin
         Destroy_Main_Window_Block :
         declare
            use Tcl.Tk.Ada.Widgets.Toplevel;
            use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;

            Main_Window: Tk_Toplevel := Get_Main_Window(Interp => Interp);
         begin
            Destroy(Widgt => Main_Window);
         exception
            when Storage_Error =>
               null;
         end Destroy_Main_Window_Block;
         Interp := Steam_Sky(Params => New_String(Str => ""));
         Set_Context(Interp => Interp);
         Tcl_EvalFile
           (interp => Interp,
            fileName =>
              To_String(Source => Data_Directory) & "ui" & Dir_Separator &
              "errordialog.tcl");
         Add_Command
           (Name => "OpenLink2", Ada_Command => Open_Link_Command'Access);
         Show_Error_Message_Block :
         declare
            use Tcl.Tk.Ada.Widgets.Text;
            use Tcl.Tk.Ada.Widgets.TtkButton;

            Text_View: constant Tk_Text :=
              Get_Widget(pathName => ".technical.text", Interp => Interp);
            Directory_Button: constant Ttk_Button :=
              Get_Widget(pathName => ".buttons.showdirectory");
         begin
            Insert
              (TextWidget => Text_View, Index => "end",
               Text =>
                 "{" & To_String(Source => Error_Text & Error_Details) & "}");
            configure(Widgt => Text_View, options => "-state disabled");
            configure
              (Widgt => Directory_Button,
               options =>
                 "-command {OpenLink2 {" & To_String(Source => Save_Directory) &
                 " }}");
         end Show_Error_Message_Block;
         Tcl.Tk.Tk_MainLoop;
      end Show_Error_Dialog_Block;
   end Save_Exception;

end ErrorDialog;
