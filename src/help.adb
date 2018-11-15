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

with DOM.Core; use DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with Log; use Log;
with Game; use Game;

package body Help is

   procedure LoadHelp(Reader: Tree_Reader) is
      TmpHelp: Help_Data;
      NodesList: Node_List;
      HelpData: Document;
      Action: Unbounded_String;
      HelpIndex: Natural;
      HelpNode: Node;
   begin
      TmpHelp :=
        (Title => Null_Unbounded_String, Text => Null_Unbounded_String);
      HelpData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(HelpData, "entry");
      for I in 0 .. Length(NodesList) - 1 loop
         HelpNode := Item(NodesList, I);
         TmpHelp.Title :=
           To_Unbounded_String(Get_Attribute(HelpNode, "title"));
         Action := To_Unbounded_String(Get_Attribute(HelpNode, "action"));
         HelpIndex := 0;
         for J in Help_List.Iterate loop
            if Help_List(J).Title = TmpHelp.Title then
               HelpIndex := Help_Container.To_Index(J);
               exit;
            end if;
         end loop;
         if
           (Action = To_Unbounded_String("update") or
            Action = To_Unbounded_String("remove")) then
            if HelpIndex = 0 then
               raise Data_Loading_Error
                 with "Can't " & To_String(Action) & " help '" &
                 To_String(TmpHelp.Title) &
                 "', there no help with that title.";
            end if;
         elsif HelpIndex > 0 then
            raise Data_Loading_Error
              with "Can't add help '" & To_String(TmpHelp.Title) &
              "', there is one with that title.";
         end if;
         if Action /= To_Unbounded_String("remove") then
            if Action = To_Unbounded_String("update") then
               TmpHelp := Help_List(HelpIndex);
            end if;
            if Has_Child_Nodes(HelpNode) then
               TmpHelp.Text :=
                 To_Unbounded_String(Node_Value(First_Child(HelpNode)));
            end if;
            if Action /= To_Unbounded_String("update") then
               Help_List.Append(New_Item => TmpHelp);
               LogMessage
                 ("Help added: " & To_String(TmpHelp.Title), Everything);
            else
               Help_List(HelpIndex) := TmpHelp;
            end if;
         else
            Help_List.Delete(Index => HelpIndex);
            LogMessage
              ("Help removed: " & To_String(TmpHelp.Title), Everything);
         end if;
         TmpHelp :=
           (Title => Null_Unbounded_String, Text => Null_Unbounded_String);
      end loop;
   end LoadHelp;

end Help;
