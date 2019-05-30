--    Copyright 2017-2019 Bartek thindil Jasicki
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

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Game; use Game;

package body Log is

   LogFile: File_Type; -- Debug log file

   procedure StartLogging is
   begin
      if DebugMode = None then
         return;
      end if;
      if Exists(To_String(SaveDirectory) & "debug.log") then
         Open(LogFile, Append_File, To_String(SaveDirectory) & "debug.log");
      else
         Create(LogFile, Append_File, To_String(SaveDirectory) & "debug.log");
      end if;
      LogMessage
        ("Start game in debug mode: " & Debug_Types'Image(DebugMode) & ".",
         DebugMode);
   end StartLogging;

   procedure LogMessage
     (Message: String; MessageType: Debug_Types;
      NewLine, TimeStamp: Boolean := True) is
      NewMessage: Unbounded_String;
   begin
      if DebugMode = None or
        (MessageType /= DebugMode and DebugMode /= Everything) then
         return;
      end if;
      if not Is_Open(LogFile) then
         return;
      end if;
      if TimeStamp then
         NewMessage := To_Unbounded_String("[");
         Append
           (NewMessage, Ada.Calendar.Formatting.Image(Clock) & "]:" & Message);
      else
         NewMessage := To_Unbounded_String(Message);
      end if;
      if NewLine then
         Put_Line(LogFile, To_String(NewMessage));
      else
         Put(LogFile, To_String(NewMessage));
      end if;
   end LogMessage;

   procedure EndLogging is
   begin
      if DebugMode = None or not Is_Open(LogFile) then
         return;
      end if;
      LogMessage
        ("Ending game in debug mode: " & Debug_Types'Image(DebugMode) & ".",
         DebugMode);
      Close(LogFile);
   end EndLogging;

end Log;
