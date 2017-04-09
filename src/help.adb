--    Copyright 2016-2017 Bartek thindil Jasicki
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

package body Help is

   procedure LoadHelp is
      HelpFile: File_Type;
      RawData: Unbounded_String;
      TmpHelp: Help_Data;
   begin
      if Help_List.Length > 0 then
         return;
      end if;
      if not Exists("data/help.dat") then
         raise Help_File_Not_Found;
      end if;
      TmpHelp :=
        (Title => Null_Unbounded_String, Text => Null_Unbounded_String);
      Open(HelpFile, In_File, "data/help.dat");
      while not End_Of_File(HelpFile) loop
         RawData := To_Unbounded_String(Get_Line(HelpFile));
         if Element(RawData, 1) /= '[' then
            Append(TmpHelp.Text, RawData);
            Append(TmpHelp.Text, ASCII.LF);
         else
            if TmpHelp.Text /= Null_Unbounded_String then
               Help_List.Append(New_Item => TmpHelp);
               TmpHelp :=
                 (Title => Null_Unbounded_String,
                  Text => Null_Unbounded_String);
            end if;
            if Length(RawData) > 2 then
               TmpHelp.Title :=
                 To_Unbounded_String(Slice(RawData, 2, Length(RawData) - 1));
            end if;
         end if;
      end loop;
      Close(HelpFile);
   end LoadHelp;

end Help;
