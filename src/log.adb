--    Copyright 2017-2022 Bartek thindil Jasicki
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

with Interfaces.C.Strings;

package body Log is

   procedure Log_Message
     (Message: String; Message_Type: Debug_Types;
      New_Line, Time_Stamp: Boolean := True) is
      pragma Unreferenced(New_Line, Time_Stamp);
      use Interfaces.C.Strings;

      procedure Nim_Log_Message(C_Message: chars_ptr; Debug_Type: Integer) with
         Import => True,
         Convention => C,
         External_Name => "logMessage";
   begin
      Nim_Log_Message
        (C_Message => New_String(Str => Message),
         Debug_Type => Debug_Types'Pos(Message_Type));
   end Log_Message;

end Log;
