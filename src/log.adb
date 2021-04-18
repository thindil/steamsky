--    Copyright 2017-2021 Bartek thindil Jasicki
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

   -- ****iv* Log/Log.Log_File
   -- FUNCTION
   -- Debug log file
   -- SOURCE
   Log_File: File_Type;
   -- ****

   procedure Start_Logging is
   begin
      if Debug_Mode = Default_Debug_Mode then
         return;
      end if;
      if Exists(Name => To_String(Source => Save_Directory) & "debug.log") then
         Open
           (File => Log_File, Mode => Append_File,
            Name => To_String(Source => Save_Directory) & "debug.log");
      else
         Create
           (File => Log_File, Mode => Append_File,
            Name => To_String(Source => Save_Directory) & "debug.log");
      end if;
      Log_Message
        (Message =>
           "Start game in debug mode: " & Debug_Types'Image(Debug_Mode) & ".",
         Message_Type => Debug_Mode);
   end Start_Logging;

   procedure Log_Message
     (Message: String; Message_Type: Debug_Types;
      New_Line, Time_Stamp: Boolean := True) is
      New_Message: Unbounded_String;
   begin
      if Debug_Mode = Default_Debug_Mode or
        (Message_Type /= Debug_Mode and Debug_Mode /= EVERYTHING) then
         return;
      end if;
      if not Is_Open
          (Log_File) then --## rule line off DIRECTLY_ACCESSED_GLOBALS
         return;
      end if;
      New_Message :=
        (if Time_Stamp then
           To_Unbounded_String
             ("[" & Ada.Calendar.Formatting.Image(Clock) & "]:" & Message)
         else To_Unbounded_String(Message));
      if New_Line then
         Put_Line
           (Log_File, --## rule line off DIRECTLY_ACCESSED_GLOBALS

            To_String(New_Message));
      else
         Put(Log_File, To_String(New_Message));
      end if;
   end Log_Message;

   procedure End_Logging is
   begin
      if Debug_Mode = Default_Debug_Mode or not Is_Open(Log_File) then
         return;
      end if;
      Log_Message
        ("Ending game in debug mode: " & Debug_Types'Image(Debug_Mode) & ".",
         Debug_Mode);
      Close(Log_File);
   end End_Logging;

end Log;
