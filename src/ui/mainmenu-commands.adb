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
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Game; use Game;
with HallOfFame; use HallOfFame;

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

   -- ****iv* MCommands/AllNews
   -- FUNCTION
   -- If true, show all news, not only from last version. Default is false
   -- SOURCE
   AllNews: Boolean := False;
   -- ****

   -- ****if* MCommands/Show_News_Command
   -- FUNCTION
   -- Show changes in the game, all or just recent
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Show_News_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_News_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      TextView: Tk_Text;
      ChangesFile: File_Type;
      FileText: Unbounded_String;
      AllNewsButton: Ttk_Button;
   begin
      AllNewsButton.Interp := Interp;
      AllNewsButton.Name := New_String(".newsmenu.showall");
      if CArgv.Arg(Argv, 1) = "false" then
         AllNews := False;
         configure
           (AllNewsButton,
            "-text {Show all changes} -command {ShowNews true}");
      else
         AllNews := True;
         configure
           (AllNewsButton,
            "-text {Show only newest changes} -command {ShowNews false}");
      end if;
      TextView.Interp := Interp;
      TextView.Name := New_String(".newsmenu.text");
      configure(TextView, "-state normal");
      Delete(TextView, "1.0", "end");
      if not Exists(To_String(DocDirectory) & "CHANGELOG.md") then
         Insert
           (TextView, "end",
            "{Can't find changelog file. Did 'CHANGELOG.md' file is in '" &
            To_String(DocDirectory) & "' directory?}");
      else
         Open(ChangesFile, In_File, To_String(DocDirectory) & "CHANGELOG.md");
         Set_Line(ChangesFile, 6);
         while not End_Of_File(ChangesFile) loop
            FileText := To_Unbounded_String(Get_Line(ChangesFile));
            if Length(FileText) > 1 and not AllNews then
               exit when Slice(FileText, 1, 3) = "## ";
            end if;
            Insert(TextView, "end", "{" & To_String(FileText) & LF & "}");
         end loop;
         Close(ChangesFile);
      end if;
      configure(TextView, "-state disabled");
      return TCL_OK;
   end Show_News_Command;

   -- ****if* MCommands/Show_Hof_Command
   -- FUNCTION
   -- Show the Hall of Fame
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Show_Hall_Of_Fame_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Hall_Of_Fame_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      HofView: Ttk_Tree_View;
   begin
      HofView.Interp := Interp;
      HofView.Name := New_String(".hofmenu.view");
      Delete(HofView, "[list " & Children(HofView, "{}") & "]");
      for I in HallOfFame_Array'Range loop
         exit when HallOfFame_Array(I).Name = Null_Unbounded_String;
         Insert
           (HofView,
            "{} end -values [list " & Positive'Image(I) & " " &
            To_String(HallOfFame_Array(I).Name) & " " &
            Natural'Image(HallOfFame_Array(I).Points) & " " &
            To_String(HallOfFame_Array(I).DeathReason) & "]");
      end loop;
      return TCL_OK;
   end Show_Hall_Of_Fame_Command;

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
      AddCommand("ShowNews", Show_News_Command'Access);
      AddCommand("ShowHallOfFame", Show_Hall_Of_Fame_Command'Access);
   end AddCommands;

end MainMenu.Commands;
