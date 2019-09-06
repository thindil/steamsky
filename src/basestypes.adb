--    Copyright 2019 Bartek thindil Jasicki
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

package body BasesTypes is

   procedure LoadBasesTypes(Reader: Tree_Reader) is
      TempRecord: BaseType_Data;
      NodesList: Node_List;
      BasesData: Document;
      TmpTrades: BasesTrade_Container.Map;
      TmpRecipes: UnboundedString_Container.Vector;
      TmpFlags: UnboundedString_Container.Vector;
      BaseNode: Node;
      BaseIndex: Unbounded_String;
      Action: DataAction;
   begin
      BasesData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(BasesData, "base");
      for I in 0 .. Length(NodesList) - 1 loop
         TempRecord :=
           (Name => Null_Unbounded_String, Color => "#FFFFFF",
            Trades => TmpTrades, Recipes => TmpRecipes, Flags => TmpFlags,
            Description => Null_Unbounded_String);
         BaseNode := Item(NodesList, I);
         BaseIndex := To_Unbounded_String(Get_Attribute(BaseNode, "index"));
         if Get_Attribute(BaseNode, "action")'Length > 0 then
            Action := DataAction'Value(Get_Attribute(BaseNode, "action"));
         else
            Action := ADD;
         end if;
         if (Action = UPDATE or Action = REMOVE) then
            if not BasesTypes_Container.Contains
                (BasesTypes_List, BaseIndex) then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(DataAction'Image(Action)) &
                 " base type '" & To_String(BaseIndex) &
                 "', there no base type with that index.";
            end if;
         elsif BasesTypes_Container.Contains(BasesTypes_List, BaseIndex) then
            raise Data_Loading_Error
              with "Can't add base type '" & To_String(BaseIndex) &
              "', there is one with that index.";
         end if;
         if Action /= REMOVE then
            if Action = UPDATE then
               TempRecord := BasesTypes_List(BaseIndex);
            end if;
            if Get_Attribute(BaseNode, "name") /= "" then
               TempRecord.Name :=
                 To_Unbounded_String(Get_Attribute(BaseNode, "name"));
            end if;
            if Get_Attribute(BaseNode, "color") /= "" then
               TempRecord.Color := Get_Attribute(BaseNode, "color");
            end if;
            if Get_Attribute(BaseNode, "description") /= "" then
               TempRecord.Description :=
                 To_Unbounded_String(Get_Attribute(BaseNode, "description"));
            end if;
            if Action /= UPDATE then
               BasesTypes_Container.Include
                 (BasesTypes_List, BaseIndex, TempRecord);
               LogMessage
                 ("Base type added: " & To_String(TempRecord.Name),
                  Everything);
            else
               BasesTypes_List(BaseIndex) := TempRecord;
               LogMessage
                 ("Base type updated: " & To_String(TempRecord.Name),
                  Everything);
            end if;
         else
            BasesTypes_Container.Exclude(BasesTypes_List, BaseIndex);
            LogMessage
              ("Base type removed: " & To_String(BaseIndex), Everything);
         end if;
      end loop;
   end LoadBasesTypes;

end BasesTypes;
