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
with Crafts; use Crafts;

package body BasesTypes is

   procedure LoadBasesTypes(Reader: Tree_Reader) is
      TempRecord: BaseType_Data;
      NodesList, ChildNodes: Node_List;
      BasesData: Document;
      TmpTrades: BasesTrade_Container.Map;
      TmpRecipes: UnboundedString_Container.Vector;
      TmpFlags: UnboundedString_Container.Vector;
      BaseNode, ChildNode: Node;
      BaseIndex, ItemIndex: Unbounded_String;
      Action, SubAction: DataAction;
      procedure AddChildNode
        (Data: in out UnboundedString_Container.Vector; Name: String;
         Index: Natural) is
         Value: Unbounded_String;
         DeleteIndex: Positive;
      begin
         ChildNodes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name
             (Item(NodesList, Index), Name);
         for J in 0 .. Length(ChildNodes) - 1 loop
            ChildNode := Item(ChildNodes, J);
            if Name = "flag" then
               Value := To_Unbounded_String(Get_Attribute(ChildNode, "name"));
            else
               Value := To_Unbounded_String(Get_Attribute(ChildNode, "index"));
            end if;
            if Get_Attribute(ChildNode, "action")'Length > 0 then
               SubAction :=
                 DataAction'Value(Get_Attribute(ChildNode, "action"));
            else
               SubAction := ADD;
            end if;
            if Name = "recipe" and then not Recipes_List.Contains(Value) then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(DataAction'Image(Action)) &
                 " base type '" & To_String(BaseIndex) &
                 "', no recipe with index '" & To_String(Value) & "'.";
            end if;
            if SubAction /= REMOVE then
               Data.Append(New_Item => Value);
            else
               DeleteIndex := Data.First_Index;
               while DeleteIndex <= Data.Last_Index loop
                  if Data(DeleteIndex) = Value then
                     Data.Delete(Index => DeleteIndex);
                     exit;
                  end if;
                  DeleteIndex := DeleteIndex + 1;
               end loop;
            end if;
         end loop;
      end AddChildNode;
   begin
      BasesData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(BasesData, "base");
      for I in 0 .. Length(NodesList) - 1 loop
         TempRecord :=
           (Name => Null_Unbounded_String, Color => "ffffff",
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
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (BaseNode, "description");
            if Length(ChildNodes) > 0 then
               TempRecord.Description :=
                 To_Unbounded_String
                   (Node_Value(First_Child(Item(ChildNodes, 0))));
            end if;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Item(NodesList, I), "item");
            for J in 0 .. Length(ChildNodes) - 1 loop
               ChildNode := Item(ChildNodes, J);
               ItemIndex :=
                 To_Unbounded_String(Get_Attribute(ChildNode, "index"));
               if Get_Attribute(ChildNode, "action")'Length > 0 then
                  SubAction :=
                    DataAction'Value(Get_Attribute(ChildNode, "action"));
               else
                  SubAction := ADD;
               end if;
               if not Items_List.Contains(ItemIndex) then
                  raise Data_Loading_Error
                    with "Can't " & To_Lower(DataAction'Image(Action)) &
                    " base type '" & To_String(BaseIndex) &
                    "', no item with index '" & To_String(ItemIndex) & "'.";
               end if;
               if SubAction /= REMOVE then
                  TempRecord.Trades.Include
                    (Key => ItemIndex,
                     New_Item =>
                       (Natural'Value(Get_Attribute(ChildNode, "sellprice")),
                        0));
               else
                  TempRecord.Trades.Delete(ItemIndex);
               end if;
            end loop;
            AddChildNode(TempRecord.Recipes, "recipe", I);
            AddChildNode(TempRecord.Flags, "flag", I);
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

   function Is_Buyable(BaseType, ItemIndex: Unbounded_String) return Boolean is
   begin
      if not BasesTypes_List(BaseType).Trades.Contains(ItemIndex) then
         return False;
      end if;
      if BasesTypes_List(BaseType).Trades(ItemIndex)(1) = 0 then
         return False;
      end if;
      return True;
   end Is_Buyable;

   function Get_Price(BaseType, ItemIndex: Unbounded_String) return Natural is
   begin
      if Items_List(ItemIndex).Price = 0 then
         return 0;
      end if;
      if BasesTypes_List(BaseType).Trades.Contains(ItemIndex) then
         if BasesTypes_List(BaseType).Trades(ItemIndex)(1) > 0 then
            return BasesTypes_List(BaseType).Trades(ItemIndex)(1);
         elsif BasesTypes_List(BaseType).Trades(ItemIndex)(2) > 0 then
            return BasesTypes_List(BaseType).Trades(ItemIndex)(2);
         end if;
      end if;
      return Items_List(ItemIndex).Price;
   end Get_Price;

   function BaseTypeIndex(BaseType: Unbounded_String) return Positive is
   begin
      if BaseType = To_Unbounded_String("0") then
         return 1;
      elsif BaseType = To_Unbounded_String("1") then
         return 2;
      elsif BaseType = To_Unbounded_String("2") then
         return 3;
      elsif BaseType = To_Unbounded_String("3") then
         return 4;
      elsif BaseType = To_Unbounded_String("4") then
         return 5;
      end if;
      return 1;
   end BaseTypeIndex;

end BasesTypes;
