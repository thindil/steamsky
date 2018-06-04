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

with Ada.Directories; use Ada.Directories;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with DOM.Readers; use DOM.Readers;
with Input_Sources.File; use Input_Sources.File;
with Log; use Log;
with Game; use Game;

package body Help is

   procedure LoadHelp is
      TmpHelp: Help_Data;
      Files: Search_Type;
      FoundFile: Directory_Entry_Type;
      HelpFile: File_Input;
      Reader: Tree_Reader;
      NodesList: Node_List;
      HelpData: Document;
   begin
      if Help_List.Length > 0 then
         return;
      end if;
      if not Exists(To_String(DataDirectory) & "help" & Dir_Separator) then
         raise Help_Directory_Not_Found;
      end if;
      Start_Search
        (Files,
         To_String(DataDirectory) & "help" & Dir_Separator,
         "*.dat");
      if not More_Entries(Files) then
         raise Help_Files_Not_Found;
      end if;
      while More_Entries(Files) loop
         Get_Next_Entry(Files, FoundFile);
         TmpHelp :=
           (Title => Null_Unbounded_String, Text => Null_Unbounded_String);
         LogMessage("Loading help file: " & Full_Name(FoundFile), Everything);
         Open(Full_Name(FoundFile), HelpFile);
         Parse(Reader, HelpFile);
         Close(HelpFile);
         HelpData := Get_Tree(Reader);
         NodesList :=
           DOM.Core.Documents.Get_Elements_By_Tag_Name(HelpData, "entry");
         for I in 0 .. Length(NodesList) - 1 loop
            TmpHelp.Title :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "title"));
            TmpHelp.Text :=
              To_Unbounded_String(Node_Value(First_Child(Item(NodesList, I))));
            Help_List.Append(New_Item => TmpHelp);
            LogMessage("Help added: " & To_String(TmpHelp.Title), Everything);
            TmpHelp :=
              (Title => Null_Unbounded_String, Text => Null_Unbounded_String);
         end loop;
      end loop;
      End_Search(Files);
   end LoadHelp;

end Help;
