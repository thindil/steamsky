--    Copyright 2019-2021 Bartek thindil Jasicki
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
with Bases; use Bases;
with Crafts; use Crafts;
with Log; use Log;

package body BasesTypes is

   procedure Load_Bases_Types(Reader: Tree_Reader) is
      TempRecord: Base_Type_Data;
      NodesList, ChildNodes: Node_List;
      BasesData: Document;
      TmpTrades: BasesTrade_Container.Map;
      TmpRecipes: UnboundedString_Container.Vector;
      TmpFlags: UnboundedString_Container.Vector;
      BaseNode, ChildNode: Node;
      BaseIndex, ItemIndex: Unbounded_String;
      Action, SubAction: Data_Action;
      BuyPrice, SellPrice: Natural;
      procedure AddChildNode
        (Data: in out UnboundedString_Container.Vector; Name: String;
         Index: Natural) is
         Value: Unbounded_String;
         DeleteIndex: Positive;
      begin
         ChildNodes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name
             (Item(NodesList, Index), Name);
         Read_Child_Node_Loop :
         for J in 0 .. Length(ChildNodes) - 1 loop
            ChildNode := Item(ChildNodes, J);
            Value :=
              (if Name = "flag" then
                 To_Unbounded_String(Get_Attribute(ChildNode, "name"))
               else To_Unbounded_String(Get_Attribute(ChildNode, "index")));
            SubAction :=
              (if Get_Attribute(ChildNode, "action")'Length > 0 then
                 Data_Action'Value(Get_Attribute(ChildNode, "action"))
               else ADD);
            if Name = "recipe" then
               if not Recipes_List.Contains(Value) then
                  raise Data_Loading_Error
                    with "Can't " & To_Lower(Data_Action'Image(Action)) &
                    " base type '" & To_String(BaseIndex) &
                    "', no recipe with index '" & To_String(Value) & "'.";
               end if;
               if Data.Contains(Value) and SubAction = ADD then
                  raise Data_Loading_Error
                    with "Can't " & To_Lower(Data_Action'Image(Action)) &
                    " base type '" & To_String(BaseIndex) & "', recipe '" &
                    To_String(Value) & "' already added.";
               end if;
            end if;
            if SubAction /= REMOVE then
               Data.Append(New_Item => Value);
            else
               DeleteIndex := Data.First_Index;
               Delete_Child_Data_Loop :
               while DeleteIndex <= Data.Last_Index loop
                  if Data(DeleteIndex) = Value then
                     Data.Delete(Index => DeleteIndex);
                     exit Delete_Child_Data_Loop;
                  end if;
                  DeleteIndex := DeleteIndex + 1;
               end loop Delete_Child_Data_Loop;
            end if;
         end loop Read_Child_Node_Loop;
      end AddChildNode;
   begin
      BasesData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(BasesData, "base");
      Read_Bases_Types_Loop :
      for I in 0 .. Length(NodesList) - 1 loop
         TempRecord :=
           (Name => Null_Unbounded_String, Color => "ffffff",
            Trades => TmpTrades, Recipes => TmpRecipes, Flags => TmpFlags,
            Description => Null_Unbounded_String);
         BaseNode := Item(NodesList, I);
         BaseIndex := To_Unbounded_String(Get_Attribute(BaseNode, "index"));
         Action :=
           (if Get_Attribute(BaseNode, "action")'Length > 0 then
              Data_Action'Value(Get_Attribute(BaseNode, "action"))
            else ADD);
         if Action in UPDATE | REMOVE then
            if not BasesTypes_Container.Contains
                (Bases_Types_List, BaseIndex) then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(Data_Action'Image(Action)) &
                 " base type '" & To_String(BaseIndex) &
                 "', there no base type with that index.";
            end if;
         elsif BasesTypes_Container.Contains(Bases_Types_List, BaseIndex) then
            raise Data_Loading_Error
              with "Can't add base type '" & To_String(BaseIndex) &
              "', there is one with that index.";
         end if;
         if Action /= REMOVE then
            if Action = UPDATE then
               TempRecord := Bases_Types_List(BaseIndex);
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
            Read_Items_Loop :
            for J in 0 .. Length(ChildNodes) - 1 loop
               ChildNode := Item(ChildNodes, J);
               ItemIndex :=
                 To_Unbounded_String(Get_Attribute(ChildNode, "index"));
               SubAction :=
                 (if Get_Attribute(ChildNode, "action")'Length > 0 then
                    Data_Action'Value(Get_Attribute(ChildNode, "action"))
                  else ADD);
               if not Items_List.Contains(ItemIndex) then
                  raise Data_Loading_Error
                    with "Can't " & To_Lower(Data_Action'Image(Action)) &
                    " base type '" & To_String(BaseIndex) &
                    "', no item with index '" & To_String(ItemIndex) & "'.";
               end if;
               if SubAction = ADD
                 and then TempRecord.Trades.Contains(ItemIndex) then
                  raise Data_Loading_Error
                    with "Can't " & To_Lower(Data_Action'Image(Action)) &
                    " base type '" & To_String(BaseIndex) &
                    "', item with index '" & To_String(ItemIndex) &
                    "' already added.";
               end if;
               if SubAction /= REMOVE then
                  SellPrice := 0;
                  if Get_Attribute(ChildNode, "sellprice") /= "" then
                     SellPrice :=
                       Natural'Value(Get_Attribute(ChildNode, "sellprice"));
                  end if;
                  BuyPrice := 0;
                  if Get_Attribute(ChildNode, "buyprice") /= "" then
                     BuyPrice :=
                       Natural'Value(Get_Attribute(ChildNode, "buyprice"));
                  end if;
                  TempRecord.Trades.Include
                    (Key => ItemIndex, New_Item => (SellPrice, BuyPrice));
               else
                  TempRecord.Trades.Delete(ItemIndex);
               end if;
            end loop Read_Items_Loop;
            AddChildNode(TempRecord.Recipes, "recipe", I);
            AddChildNode(TempRecord.Flags, "flag", I);
            if Action /= UPDATE then
               BasesTypes_Container.Include
                 (Bases_Types_List, BaseIndex, TempRecord);
               Log_Message
                 ("Base type added: " & To_String(TempRecord.Name),
                  EVERYTHING);
            else
               Bases_Types_List(BaseIndex) := TempRecord;
               Log_Message
                 ("Base type updated: " & To_String(TempRecord.Name),
                  EVERYTHING);
            end if;
         else
            BasesTypes_Container.Exclude(Bases_Types_List, BaseIndex);
            Log_Message
              ("Base type removed: " & To_String(BaseIndex), EVERYTHING);
         end if;
      end loop Read_Bases_Types_Loop;
   end Load_Bases_Types;

   function Is_Buyable
     (Base_Type, Item_Index: Unbounded_String; Check_Flag: Boolean := True;
      Base_Index: Extended_Base_Range := 0) return Boolean is
   begin
      if Base_Index > 0
        and then Sky_Bases(Base_Index).Reputation(1) <
          Items_List(Item_Index).Reputation then
         return False;
      end if;
      if Check_Flag
        and then
        (Bases_Types_List(Base_Type).Flags.Contains
           (To_Unbounded_String("blackmarket")) and
         Get_Price(Base_Type, Item_Index) > 0) then
         return True;
      end if;
      if not Bases_Types_List(Base_Type).Trades.Contains(Item_Index) then
         return False;
      end if;
      if Bases_Types_List(Base_Type).Trades(Item_Index)(1) = 0 then
         return False;
      end if;
      return True;
   end Is_Buyable;

   function Get_Price
     (Base_Type, Item_Index: Unbounded_String) return Natural is
   begin
      if Items_List(Item_Index).Price = 0 then
         return 0;
      end if;
      if Bases_Types_List(Base_Type).Trades.Contains(Item_Index) then
         if Bases_Types_List(Base_Type).Trades(Item_Index)(1) > 0 then
            return Bases_Types_List(Base_Type).Trades(Item_Index)(1);
         elsif Bases_Types_List(Base_Type).Trades(Item_Index)(2) > 0 then
            return Bases_Types_List(Base_Type).Trades(Item_Index)(2);
         end if;
      end if;
      return Items_List(Item_Index).Price;
   end Get_Price;

end BasesTypes;
