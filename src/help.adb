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
with Log; use Log;

package body Help is

   procedure LoadHelp is
      HelpFile: File_Type;
      RawData: Unbounded_String;
      TmpHelp: Help_Data;
      Files: Search_Type;
      FoundFile: Directory_Entry_Type;
   begin
      if Help_List.Length > 0 then
         return;
      end if;
      if not Exists("data/help/") then
         raise Help_Directory_Not_Found;
      end if;
      Start_Search(Files, "data/help/", "*.dat");
      if not More_Entries(Files) then
         raise Help_Files_Not_Found;
      end if;
      while More_Entries(Files) loop
         Get_Next_Entry(Files, FoundFile);
         TmpHelp :=
           (Title => Null_Unbounded_String, Text => Null_Unbounded_String);
         LogMessage("Loading help file: " & Full_Name(FoundFile), Everything);
         Open(HelpFile, In_File, Full_Name(FoundFile));
         while not End_Of_File(HelpFile) loop
            RawData := To_Unbounded_String(Get_Line(HelpFile));
            if Element(RawData, 1) /= '[' then
               Append(TmpHelp.Text, RawData);
               Append(TmpHelp.Text, ASCII.LF);
            else
               if TmpHelp.Text /= Null_Unbounded_String then
                  Help_List.Append(New_Item => TmpHelp);
                  LogMessage
                    ("Help added: " & To_String(TmpHelp.Title),
                     Everything);
                  TmpHelp :=
                    (Title => Null_Unbounded_String,
                     Text => Null_Unbounded_String);
               end if;
               if Length(RawData) > 2 then
                  TmpHelp.Title :=
                    To_Unbounded_String
                      (Slice(RawData, 2, Length(RawData) - 1));
               end if;
            end if;
         end loop;
         Close(HelpFile);
      end loop;
      End_Search(Files);
   end LoadHelp;

end Help;
