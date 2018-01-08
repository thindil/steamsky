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

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with Gtk.Main; use Gtk.Main;
with Game; use Game;
with Config; use Config;
with Log; use Log;
with HallOfFame; use HallOfFame;

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

   --  Initializes GtkAda
   Init;

   EndLogging;
exception
   when An_Exception : others =>
      declare
         ErrorFile: File_Type;
      begin
         if Exists(To_String(SaveDirectory) & "error.log") then
            Open(ErrorFile, Append_File, To_String(SaveDirectory) & "error.log");
         else
            Create
               (ErrorFile,
               Append_File,
               To_String(SaveDirectory) & "error.log");
         end if;
         Put_Line(ErrorFile, Ada.Calendar.Formatting.Image(Clock));
         Put_Line(ErrorFile, GameVersion);
         Put_Line(ErrorFile, "Exception: " & Exception_Name(An_Exception));
         Put_Line(ErrorFile, "Message: " & Exception_Message(An_Exception));
         Put_Line(ErrorFile, "-------------------------------------------------");
         Put_Line(ErrorFile, Symbolic_Traceback(An_Exception));
         Put_Line(ErrorFile, "-------------------------------------------------");
         Close(ErrorFile);
      end;
      EndLogging;
end SteamSky;
