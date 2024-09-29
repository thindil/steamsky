--    Copyright 2016-2024 Bartek thindil Jasicki
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

package body Messages is

   function Formated_Time(Time: Date_Record := Game_Date) return String is
      use Interfaces.C.Strings;

      function Nim_Formatted_Time
        (Year, Month, Day, Hour, Minutes: Integer) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "formattedTime";
   begin
      return
        Value
          (Item =>
             Nim_Formatted_Time
               (Year => Time.Year, Month => Time.Month, Day => Time.Day,
                Hour => Time.Hour, Minutes => Time.Minutes));
   end Formated_Time;

   function Get_Last_Message_Index return Natural is
      function Nim_Get_Last_Message_Index return Integer with
         Import => True,
         Convention => C,
         External_Name => "getLastMessageIndex";
   begin
      return Nim_Get_Last_Message_Index + 1;
   end Get_Last_Message_Index;

end Messages;
