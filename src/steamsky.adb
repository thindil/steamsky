--    Copyright 2016-2018 Bartek thindil Jasicki
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
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Interfaces.C; use Interfaces.C;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtk.Main; use Gtk.Main;
with Gtk.Settings; use Gtk.Settings;
with Gtkada.Bindings; use Gtkada.Bindings;
with Glib; use Glib;
with Game; use Game;
with Config; use Config;
with Log; use Log;
with HallOfFame; use HallOfFame;
with MainMenu; use MainMenu;

procedure SteamSky is

   ConfigDirectory: Unbounded_String :=
     To_Unbounded_String
       (Full_Name(Dir_Name(Command_Name)) &
        Dir_Separator &
        "../etc" &
        Dir_Separator);
   ShareDirectory: Unbounded_String :=
     To_Unbounded_String
       (Full_Name(Dir_Name(Command_Name)) &
        Dir_Separator &
        "../share" &
        Dir_Separator);
   function UpdatePath
     (Path: in out Unbounded_String;
      PathName: String) return Boolean is
   begin
      if Element(Path, Length(Path)) /= Dir_Separator then
         Append(Path, Dir_Separator);
      end if;
      if not Ada.Directories.Exists(To_String(Path)) then
         Put_Line
           ("Directory " &
            To_String(Path) &
            " not exists. You must use existing directory as " &
            To_Lower(PathName) &
            " directory.");
         return False;
      end if;
      LogMessage
        (PathName & " directory sets to: " & To_String(Path),
         Everything);
      return True;
   end UpdatePath;

begin
   Set_Directory(Dir_Name(Command_Name));
   -- Command line arguments
   for I in 1 .. Argument_Count loop
      if Argument(I)'Length > 8 then
         if Argument(I)(1 .. 8) = "--debug=" then
            for J in Debug_Types loop
               if To_Upper(Argument(I)(9 .. (Argument(I)'Last))) =
                 Debug_Types'Image(J) then
                  DebugMode := J;
                  exit;
               end if;
            end loop;
            StartLogging;
         elsif Argument(I)(1 .. 8) = "--datadi" then
            DataDirectory :=
              To_Unbounded_String(Argument(I)(11 .. (Argument(I)'Last)));
            if not UpdatePath(DataDirectory, "Data") then
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
         elsif Argument(I)(1 .. 8) = "--etcdir" then
            ConfigDirectory :=
              To_Unbounded_String(Argument(I)(10 .. (Argument(I)'Last)));
            if not UpdatePath(ConfigDirectory, "Configuration") then
               return;
            end if;
         elsif Argument(I)(1 .. 8) = "--shared" then
            ShareDirectory :=
              To_Unbounded_String(Argument(I)(12 .. (Argument(I)'Last)));
            if not UpdatePath(ShareDirectory, "Share") then
               return;
            end if;
         end if;
      end if;
   end loop;

   if not LoadData then
      Put_Line
        ("Can't load game data. Probably missing file " &
         To_String(DataDirectory) &
         "game.dat");
      return;
   end if;

   LoadConfig;
   LoadHallOfFame;

   -- Initializes environment variables (Linux only)
   if Dir_Separator = '/' then
      declare
         VariablesNames: constant array(1 .. 4) of Unbounded_String :=
           (To_Unbounded_String("GDK_PIXBUF_MODULE_FILE"),
            To_Unbounded_String("GDK_PIXBUF_MODULEDIR"),
            To_Unbounded_String("FONTCONFIG_FILE"),
            To_Unbounded_String("XDG_DATA_DIRS"));
         VariablesValues: array(1 .. 4) of Unbounded_String;
         LibraryDirectory: Unbounded_String;
      begin
         if Ada.Environment_Variables.Exists("LD_LIBRARY_PATH") then
            LibraryDirectory := To_Unbounded_String(Value("LD_LIBRARY_PATH"));
            if Ada.Directories.Exists(To_String(LibraryDirectory)) then
               VariablesValues(1) :=
                 LibraryDirectory &
                 To_Unbounded_String("/gdk-pixbuf-2.0/2.10.0/loaders.cache");
               VariablesValues(2) :=
                 LibraryDirectory &
                 To_Unbounded_String("/gdk-pixbuf-2.0/2.10.0/loaders/");
               VariablesValues(3) :=
                 To_Unbounded_String
                   (To_String(ConfigDirectory) & "fonts/fonts.conf");
               VariablesValues(4) := ShareDirectory;
            else
               Put_Line
                 ("Can't set GTK environment, library directory not exists, terminating.");
               return;
            end if;
         else
            declare
               function Sys(Arg: char_array) return Integer;
               pragma Import(C, Sys, "system");
               RunResult: Integer;
               FileName: constant String :=
                 To_String(SaveDirectory) & "gtkada.env";
               TempFile: File_Type;
               FileText: Unbounded_String;
               StartIndex: Natural;
            begin
               RunResult :=
                 Sys(To_C("gtkada-env.sh --print-only > " & FileName));
               if RunResult /= 0 then
                  Put_Line("Can't set GTK environment, terminating.");
                  return;
               end if;
               Open(TempFile, In_File, FileName);
               while not End_Of_File(TempFile) loop
                  FileText := To_Unbounded_String(Get_Line(TempFile));
                  for I in VariablesNames'Range loop
                     if Index(FileText, To_String(VariablesNames(I))) > 0 then
                        StartIndex := Index(FileText, "=");
                        if StartIndex > 0 then
                           VariablesValues(I) :=
                             Unbounded_Slice
                               (FileText,
                                StartIndex + 2,
                                Length(FileText) - 2);
                           exit;
                        end if;
                     end if;
                  end loop;
               end loop;
               Close(TempFile);
               Delete_File(FileName);
            end;
         end if;
         for I in VariablesNames'Range loop
            Set(To_String(VariablesNames(I)), To_String(VariablesValues(I)));
         end loop;
         Set("GSETTINGS_BACKEND", "memory");
      end;
   end if;
   Set("XDG_DATA_HOME", To_String(ShareDirectory));

   --  Initializes GtkAda
   Init;
   Set_On_Exception(On_Exception'Access);
   Set_Long_Property
     (Get_Default,
      "gtk-enable-animations",
      Glong(GameSettings.AnimationsEnabled),
      "");
   CreateMainMenu;
   Main;

   EndLogging;
exception
   when An_Exception : others =>
      On_Exception(An_Exception);
end SteamSky;
