--    Copyright 2016-2020 Bartek thindil Jasicki
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
with Ada.Environment_Variables;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtk.Main; use Gtk.Main;
with Gtk.Settings; use Gtk.Settings;
with Gtkada.Bindings; use Gtkada.Bindings;
with Glib; use Glib;
with ErrorDialog; use ErrorDialog;
with Game; use Game;
with Config; use Config;
with Log; use Log;
with HallOfFame; use HallOfFame;
with MainMenu; use MainMenu;

procedure SteamSky is

   function UpdatePath
     (Path: in out Unbounded_String; PathName: String) return Boolean is
   begin
      if Element(Path, Length(Path)) /= Dir_Separator then
         Append(Path, Dir_Separator);
      end if;
      if not Exists(To_String(Path))
        and then
        (PathName /= "Save" and PathName /= "Modifications" and
         PathName /= "Themes") then
         Put_Line
           ("Directory " & To_String(Path) &
            " does not exist. You must use an existing directory as " &
            To_Lower(PathName) & " directory.");
         return False;
      end if;
      return True;
   end UpdatePath;

begin
   if Dir_Separator = '/'
     and then not Ada.Environment_Variables.Exists("RUNFROMSCRIPT") then
      Put_Line
        ("The game can be run only via the 'steamsky.sh' script. Please don't run the binary directly.");
      return;
   end if;
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
   end loop;

   if not Exists(To_String(SaveDirectory)) then
      Create_Path(To_String(SaveDirectory));
   end if;
   if not Exists(To_String(ModsDirectory)) then
      Create_Path(To_String(ModsDirectory));
   end if;
   if not Exists(To_String(ThemesDirectory)) then
      Create_Path(To_String(ThemesDirectory));
   end if;

   StartLogging;

   LoadConfig;
   LoadHallOfFame;

   --  Initializes GtkAda
   Init;
   Set_On_Exception(On_Exception'Access);
   Set_Long_Property
     (Get_Default, "gtk-enable-animations",
      Glong(GameSettings.AnimationsEnabled), "");
   CreateMainMenu;
   Main;

   EndLogging;
exception
   when An_Exception : others =>
      SaveException(An_Exception, True);
end SteamSky;
