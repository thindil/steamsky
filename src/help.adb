--    Copyright 2016-2021 Bartek thindil Jasicki
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
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
      Action: Data_Action;
      HelpIndex, HelpTitle: Unbounded_String;
      HelpNode: Node;
   begin
      HelpData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(HelpData, "entry");
      Load_Help_Data:
      for I in 0 .. Length(NodesList) - 1 loop
         TmpHelp :=
           (Index => Null_Unbounded_String, Text => Null_Unbounded_String);
         HelpNode := Item(NodesList, I);
         Action :=
           (if
              Get_Attribute(HelpNode, "action")'Length > 0
            then
              Data_Action'Value(Get_Attribute(HelpNode, "action"))
            else ADD);
         HelpIndex := To_Unbounded_String(Get_Attribute(HelpNode, "index"));
         HelpTitle := To_Unbounded_String(Get_Attribute(HelpNode, "title"));
         if Action in UPDATE | REMOVE then
            if not Help_Container.Contains(Help_List, HelpTitle) then
               raise Data_Loading_Error
                 with "Can't " &
                 To_Lower(Data_Action'Image(Action)) &
                 " help '" &
                 To_String(HelpTitle) &
                 "', there no help with that title.";
            end if;
         elsif Help_Container.Contains(Help_List, HelpTitle) then
            raise Data_Loading_Error
              with "Can't add help '" &
              To_String(HelpTitle) &
              "', there is one with that title.";
         end if;
         if Action /= REMOVE then
            TmpHelp.Index := HelpIndex;
            if Action = UPDATE then
               TmpHelp := Help_List(HelpTitle);
            end if;
            if Has_Child_Nodes(HelpNode) then
               TmpHelp.Text :=
                 To_Unbounded_String(Node_Value(First_Child(HelpNode)));
            end if;
            if Action /= UPDATE then
               Help_Container.Include(Help_List, HelpTitle, TmpHelp);
               Log_Message("Help added: " & To_String(HelpTitle), EVERYTHING);
            else
               Help_List(HelpTitle) := TmpHelp;
            end if;
         else
            Help_Container.Exclude(Help_List, HelpTitle);
            Log_Message("Help removed: " & To_String(HelpTitle), EVERYTHING);
         end if;
      end loop Load_Help_Data;
   end LoadHelp;

end Help;
