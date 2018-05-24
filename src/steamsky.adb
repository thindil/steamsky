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
with Interfaces.C; use Interfaces.C;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
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
            if Element(DataDirectory, Length(DataDirectory)) /=
              Dir_Separator then
               Append(DataDirectory, Dir_Separator);
            end if;
            LogMessage
              ("Data directory sets to: " & To_String(DataDirectory),
               Everything);
            if not Exists(To_String(DataDirectory)) then
               Put_Line
                 ("Directory " &
                  To_String(DataDirectory) &
                  " not exists. You must use existing directory as data directory.");
               return;
            end if;
         elsif Argument(I)(1 .. 8) = "--savedi" then
            SaveDirectory :=
              To_Unbounded_String(Argument(I)(11 .. (Argument(I)'Last)));
            if Element(SaveDirectory, Length(SaveDirectory)) /=
              Dir_Separator then
               Append(SaveDirectory, Dir_Separator);
            end if;
            LogMessage
              ("Save directory sets to: " & To_String(SaveDirectory),
               Everything);
            if not Exists(To_String(SaveDirectory)) then
               Put_Line
                 ("Directory " &
                  To_String(SaveDirectory) &
                  " not exists. You must use existing directory as save directory.");
               return;
            end if;
         elsif Argument(I)(1 .. 8) = "--docdir" then
            DocDirectory :=
              To_Unbounded_String(Argument(I)(10 .. (Argument(I)'Last)));
            if Element(DocDirectory, Length(DocDirectory)) /=
              Dir_Separator then
               Append(DocDirectory, Dir_Separator);
            end if;
            LogMessage
              ("Documentation directory sets to: " & To_String(DocDirectory),
               Everything);
            if not Exists(To_String(DocDirectory)) then
               Put_Line
                 ("Directory " &
                  To_String(DocDirectory) &
                  " not exists. You must use existing directory as documentation directory.");
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
           (To_Unbounded_String("LD_LIBRARY_PATH"),
            To_Unbounded_String("GDK_PIXBUF_MODULE_FILE"),
            To_Unbounded_String("GDK_PIXBUF_MODULEDIR"),
            To_Unbounded_String("FONTCONFIG_FILE"));
         VariablesValues: array(1 .. 4) of Unbounded_String;
      begin
         if Exists("../lib") then
            VariablesValues(1) := (To_Unbounded_String("../lib"));
            VariablesValues(2) :=
              (To_Unbounded_String
                 ("../lib/gdk-pixbuf-2.0/2.10.0/loaders.cache"));
            VariablesValues(3) :=
              (To_Unbounded_String("../lib/gdk-pixbuf-2.0/2.10.0/loaders/"));
            VariablesValues(4) :=
              (To_Unbounded_String
                 (Full_Name(Dir_Name(Command_Name)) &
                  Dir_Separator &
                  "../etc/fonts/fonts.conf"));
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
            Setenv
              (To_String(VariablesNames(I)),
               To_String(VariablesValues(I)));
         end loop;
         Setenv("GSETTINGS_BACKEND", "memory");
      end;
   end if;

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
