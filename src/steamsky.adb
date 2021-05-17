--    Copyright 2016-2021 Bartek thindil Jasicki
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tklib.Ada.Autoscroll; use Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Config; use Config;
with ErrorDialog; use ErrorDialog;
with Game; use Game;
with HallOfFame; use HallOfFame;
with Log; use Log;
with MainMenu; use MainMenu;
with Themes; use Themes;

procedure Steamsky is

   use type Interfaces.C.int;

   Argc: CArgv.CNatural := 0;
   Argv: CArgv.Chars_Ptr_Ptr;
   Interp: Tcl.Tcl_Interp;

   function Update_Path
     (Path: in out Unbounded_String; Path_Name: String) return Boolean is
   begin
      if Element(Source => Path, Index => Length(Source => Path)) /=
        Dir_Separator then
         Append(Source => Path, New_Item => Dir_Separator);
      end if;
      if not Exists(Name => To_String(Source => Path))
        and then Path_Name not in "Save" | "Modifications" | "Themes" then
         Put_Line
           (Item =>
              "Directory " & To_String(Source => Path) &
              " does not exist. You must use an existing directory as " &
              To_Lower(Item => Path_Name) & " directory.");
         return False;
      end if;
      return True;
   end Update_Path;

begin
   Set_Directory(Directory => Dir_Name(Path => Command_Name));
   -- Command line arguments
   Command_Line_Loop :
   for I in 1 .. Argument_Count loop
      if Argument(Number => I)'Length > 8 then
         if Argument(Number => I)(1 .. 8) = "--debug=" then
            Set_Debug_Mode_Loop :
            for J in Debug_Types loop
               if To_Upper
                   (Item =>
                      Argument(Number => I)(9 .. Argument(Number => I)'Last)) =
                 Debug_Types'Image(J) then
                  Debug_Mode := J;
                  exit Set_Debug_Mode_Loop;
               end if;
            end loop Set_Debug_Mode_Loop;
         elsif Argument(Number => I)(1 .. 8) = "--datadi" then
            Data_Directory :=
              To_Unbounded_String
                (Source =>
                   Argument(Number => I)(11 .. Argument(Number => I)'Last));
            if not Update_Path
                (Path => Data_Directory, Path_Name => "Data") then
               return;
            end if;
         elsif Argument(Number => I)(1 .. 8) = "--savedi" then
            Save_Directory :=
              To_Unbounded_String
                (Source =>
                   Argument(Number => I)(11 .. Argument(Number => I)'Last));
            if not Update_Path
                (Path => Save_Directory, Path_Name => "Save") then
               return;
            end if;
         elsif Argument(Number => I)(1 .. 8) = "--docdir" then
            Doc_Directory :=
              To_Unbounded_String
                (Source =>
                   Argument(Number => I)(10 .. Argument(Number => I)'Last));
            if not Update_Path
                (Path => Doc_Directory, Path_Name => "Documentation") then
               return;
            end if;
         elsif Argument(Number => I)(1 .. 8) = "--modsdi" then
            Mods_Directory :=
              To_Unbounded_String
                (Source =>
                   Argument(Number => I)(11 .. Argument(Number => I)'Last));
            if not Update_Path
                (Path => Mods_Directory, Path_Name => "Modifications") then
               return;
            end if;
         elsif Argument(Number => I)(1 .. 8) = "--themes" then
            Themes_Directory :=
              To_Unbounded_String
                (Source =>
                   Argument(Number => I)(13 .. Argument(Number => I)'Last));
            if not Update_Path
                (Path => Themes_Directory, Path_Name => "Themes") then
               return;
            end if;
         end if;
      end if;
   end loop Command_Line_Loop;

   Create_Path(New_Directory => To_String(Source => Save_Directory));
   Create_Path(New_Directory => To_String(Source => Mods_Directory));
   Create_Path(New_Directory => To_String(Source => Themes_Directory));

   Start_Logging;

   Load_Config;
   Load_Hall_Of_Fame;
   Load_Themes;

   -- Start Tk

   Ada.Environment_Variables.Set
     (Name => "TCL_LIBRARY",
      Value =>
        Current_Directory & Dir_Separator & "libs" & Dir_Separator & "tcl8.6");

   --  Get command-line arguments and put them into C-style "argv"
   --------------------------------------------------------------
   CArgv.Create(Argc => Argc, Argv => Argv);

   --  Tcl needs to know the path name of the executable
   --  otherwise Tcl.Tcl_Init below will fail.
   ----------------------------------------------------
   Tcl.Tcl_FindExecutable(argv0 => Argv.all);

   --  Create one Tcl interpreter
   -----------------------------
   Interp := Tcl.Tcl_CreateInterp;

   --  Initialize Tcl
   -----------------
   if Tcl.Tcl_Init(interp => Interp) = Tcl.TCL_ERROR then
      Ada.Text_IO.Put_Line
        (Item =>
           "Steam Sky: Tcl.Tcl_Init failed: " &
           Tcl.Ada.Tcl_GetStringResult(interp => Interp));
      return;
   end if;

   --  Initialize Tk
   ----------------
   if Tcl.Tk.Tk_Init(interp => Interp) = Tcl.TCL_ERROR then
      Ada.Text_IO.Put_Line
        (Item =>
           "Steam Sky: Tcl.Tk.Tk_Init failed: " &
           Tcl.Ada.Tcl_GetStringResult(interp => Interp));
      return;
   end if;

   --  Set the Tk context so that we may use shortcut Tk
   --  calls that require reference to the interpreter.
   ----------------------------------------------------
   Set_Context(Interp => Interp);

   -- Load required Tcl packages
   Tooltip_Init(Interp => Interp);
   Tcl.Ada.Tcl_Eval(interp => Interp, strng => "package require tksvg");
   Autoscroll_Init(Interp => Interp);

   -- Create and show the main game menu
   Create_Main_Menu;

   --  Loop inside Tk, waiting for commands to execute.
   --  When there are no windows left, Tcl.Tk.Tk_MainLoop returns and we exit.
   --------------------------------------------------------------------------
   Tcl.Tk.Tk_MainLoop;

   End_Logging;
exception
   when An_Exception : others =>
      Save_Exception(An_Exception => An_Exception);
end Steamsky;
