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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tklib.Ada.Autoscroll; use Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with ErrorDialog; use ErrorDialog;
with Game; use Game;
with Config; use Config;
with Log; use Log;
with HallOfFame; use HallOfFame;
with MainMenu; use MainMenu;
with Themes; use Themes;

procedure SteamSky is

   use type Interfaces.C.int;

   Argc: CArgv.CNatural;
   Argv: CArgv.Chars_Ptr_Ptr;
   Interp: Tcl.Tcl_Interp;

   function UpdatePath
     (Path: in out Unbounded_String; PathName: String) return Boolean is
   begin
      if Element(Source => Path, Index => Length(Source => Path)) /=
        Dir_Separator then
         Append(Source => Path, New_Item => Dir_Separator);
      end if;
      if not Exists(Name => To_String(Source => Path))
        and then PathName not in "Save" | "Modifications" | "Themes" then
         Put_Line
           (Item =>
              "Directory " & To_String(Source => Path) &
              " does not exist. You must use an existing directory as " &
              To_Lower(Item => PathName) & " directory.");
         return False;
      end if;
      return True;
   end UpdatePath;

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
                   (Argument(Number => I)(9 .. (Argument(Number => I)'Last))) =
                 Debug_Types'Image(J) then
                  DebugMode := J;
                  exit Set_Debug_Mode_Loop;
               end if;
            end loop Set_Debug_Mode_Loop;
         elsif Argument(Number => I)(1 .. 8) = "--datadi" then
            DataDirectory :=
              To_Unbounded_String
                (Argument(Number => I)(11 .. (Argument(Number => I)'Last)));
            if not UpdatePath(Path => DataDirectory, PathName => "Data") then
               return;
            end if;
         elsif Argument(I)(1 .. 8) = "--savedi" then
            SaveDirectory :=
              To_Unbounded_String(Argument(I)(11 .. (Argument(I)'Last)));
            if not UpdatePath(SaveDirectory, "Save") then
               return;
            end if;
         elsif Argument(I)(1 .. 8) = "--docdir" then
            DocDirectory :=
              To_Unbounded_String(Argument(I)(10 .. (Argument(I)'Last)));
            if not UpdatePath(DocDirectory, "Documentation") then
               return;
            end if;
         elsif Argument(I)(1 .. 8) = "--modsdi" then
            ModsDirectory :=
              To_Unbounded_String(Argument(I)(11 .. (Argument(I)'Last)));
            if not UpdatePath(ModsDirectory, "Modifications") then
               return;
            end if;
         elsif Argument(I)(1 .. 8) = "--themes" then
            ThemesDirectory :=
              To_Unbounded_String(Argument(I)(13 .. (Argument(I)'Last)));
            if not UpdatePath(ModsDirectory, "Themes") then
               return;
            end if;
         end if;
      end if;
   end loop Command_Line_Loop;

   Create_Path(To_String(SaveDirectory));
   Create_Path(To_String(ModsDirectory));
   Create_Path(To_String(ThemesDirectory));

   StartLogging;

   LoadConfig;
   LoadHallOfFame;
   LoadThemes;

   -- Start Tk

   --  Get command-line arguments and put them into C-style "argv"
   --------------------------------------------------------------
   CArgv.Create(Argc, Argv);

   --  Tcl needs to know the path name of the executable
   --  otherwise Tcl.Tcl_Init below will fail.
   ----------------------------------------------------
   Tcl.Tcl_FindExecutable(Argv.all);

   --  Create one Tcl interpreter
   -----------------------------
   Interp := Tcl.Tcl_CreateInterp;

   --  Initialize Tcl
   -----------------
   if Tcl.Tcl_Init(Interp) = Tcl.TCL_ERROR then
      Ada.Text_IO.Put_Line
        ("Steam Sky: Tcl.Tcl_Init failed: " &
         Tcl.Ada.Tcl_GetStringResult(Interp));
      return;
   end if;

   --  Initialize Tk
   ----------------
   if Tcl.Tk.Tk_Init(Interp) = Tcl.TCL_ERROR then
      Ada.Text_IO.Put_Line
        ("Steam Sky: Tcl.Tk.Tk_Init failed: " &
         Tcl.Ada.Tcl_GetStringResult(Interp));
      return;
   end if;

   --  Set the Tk context so that we may use shortcut Tk
   --  calls that require reference to the interpreter.
   ----------------------------------------------------
   Set_Context(Interp);

   -- Load required Tcl packages
   Tooltip_Init(Interp);
   Tcl.Ada.Tcl_Eval(Interp, "package require tksvg");
   Autoscroll_Init(Interp);

   -- Create and show the main game menu
   CreateMainMenu;

   --  Loop inside Tk, waiting for commands to execute.
   --  When there are no windows left, Tcl.Tk.Tk_MainLoop returns and we exit.
   --------------------------------------------------------------------------
   Tcl.Tk.Tk_MainLoop;

   EndLogging;
exception
   when An_Exception : others =>
      SaveException(An_Exception);
end SteamSky;
