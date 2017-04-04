--    Copyright 2017 Bartek thindil Jasicki
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

with Ada.Numerics.Discrete_Random; use Ada.Numerics;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

package body Utils is

   function GetRandom(Min: Natural; Max: Positive) return Natural is
      subtype Rand_Range is Natural range Min .. Max;
      package Rand_Roll is new Discrete_Random(Rand_Range);
      Generator: Rand_Roll.Generator;
   begin
      Rand_Roll.Reset(Generator);
      return Rand_Roll.Random(Generator);
   end GetRandom;

   procedure Log(Message: String; MessageType: Debug_Types) is
      LogFile: File_Type;
   begin
      if DebugMode = None or
        (MessageType /= DebugMode and DebugMode /= Everything) then
         return;
      end if;
      if Exists("data/debug.log") then
         Open(LogFile, Append_File, "data/debug.log");
      else
         Create(LogFile, Append_File, "data/debug.log");
      end if;
      Put_Line
        (LogFile,
         "[" & Ada.Calendar.Formatting.Image(Clock) & "]: " & Message);
      Close(LogFile);
   end Log;

end Utils;
