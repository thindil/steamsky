-- Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Game; use Game;

package body MainMenu.Commands is

   package CreateCommands is new Tcl.Ada.Generic_Command(Integer);

   -- ****if* MCommands/Open_Link_Command
   -- FUNCTION
   -- Open the selected link in the proper program
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Open_Link_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Open_Link_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      OsName: constant String := Tcl_GetVar(Get_Context, "tcl_platform(os)");
      Command: Unbounded_String;
      ProcessId: Process_Id;
      SteamSky_Execute_Error: exception;
   begin
      if OsName = "Windows" then
         Command := To_Unbounded_String(Locate_Exec_On_Path("start").all);
      elsif OsName = "Linux" then
         Command := To_Unbounded_String(Locate_Exec_On_Path("xdg-open").all);
      elsif OsName = "Darwin" then
         Command := To_Unbounded_String(Locate_Exec_On_Path("open").all);
      end if;
      ProcessId :=
        Non_Blocking_Spawn
          (To_String(Command),
           Argument_String_To_List(CArgv.Arg(Argv, 1)).all);
      if ProcessId = Invalid_Pid then
         raise SteamSky_Execute_Error with "Can't open link";
      end if;
      return TCL_OK;
   end Open_Link_Command;

   -- ****if* MCommands/Show_File_Command
   -- FUNCTION
   -- Show the selected file content
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Show_File_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_File_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      TextView: Tk_Text;
      ShowFile: File_Type;
      FileName: constant String := CArgv.Arg(Argv, 1);
   begin
      TextView.Interp := Interp;
      TextView.Name := New_String(".showfilemenu.text");
      configure(TextView, "-state normal");
      Delete(TextView, "1.0", "end");
      if not Exists(To_String(DocDirectory) & FileName) then
         Insert
           (TextView, "end",
            "{Can't find file to load. Did '" & FileName & "' file is in '" &
            To_String(DocDirectory) & "' directory?}");
      else
         Open(ShowFile, In_File, To_String(DocDirectory) & FileName);
         while not End_Of_File(ShowFile) loop
            Insert(TextView, "end", "{" & Get_Line(ShowFile) & LF & "}");
         end loop;
         Close(ShowFile);
      end if;
      configure(TextView, "-state disabled");
      Bind_To_Main_Window
        (Interp, "<Alt-b>", "{InvokeButton .showfilemenu.back}");
      Bind_To_Main_Window
        (Interp, "<Escape>", "{InvokeButton .showfilemenu.back}");
      return TCL_OK;
   end Show_File_Command;

   procedure AddCommands is
      procedure AddCommand
        (Name: String; AdaCommand: not null CreateCommands.Tcl_CmdProc) is
         Command: Tcl.Tcl_Command;
         SteamSky_Add_Command_Error: exception;
      begin
         Command :=
           CreateCommands.Tcl_CreateCommand
             (Get_Context, Name, AdaCommand, 0, null);
         if Command = null then
            raise SteamSky_Add_Command_Error with "Can't add command " & Name;
         end if;
      end AddCommand;
   begin
      AddCommand("OpenLink", Open_Link_Command'Access);
      AddCommand("ShowFile", Show_File_Command'Access);
   end AddCommands;

end MainMenu.Commands;
