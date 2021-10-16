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
      Temp_Record: Base_Type_Data;
      Nodes_List, Child_Nodes: Node_List;
      Bases_Data: Document;
      Tmp_Trades: BasesTrade_Container.Map;
      Tmp_Recipes: UnboundedString_Container.Vector;
      Tmp_Flags: UnboundedString_Container.Vector;
      Base_Node, Child_Node: Node;
      Base_Index, Item_Index: Unbounded_String;
      Action, Sub_Action: Data_Action;
      Buy_Price, Sell_Price: Natural;
      procedure Add_Child_Node
        (Data: in out UnboundedString_Container.Vector; Name: String;
         Index: Natural) is
         Value: Unbounded_String;
         Delete_Index: Positive;
      begin
         Child_Nodes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name
             (Elem => Item(List => Nodes_List, Index => Index), Name => Name);
         Read_Child_Node_Loop :
         for J in 0 .. Length(Child_Nodes) - 1 loop
            Child_Node := Item(Child_Nodes, J);
            Value :=
              (if Name = "flag" then
                 To_Unbounded_String(Get_Attribute(Child_Node, "name"))
               else To_Unbounded_String(Get_Attribute(Child_Node, "index")));
            Sub_Action :=
              (if Get_Attribute(Child_Node, "action")'Length > 0 then
                 Data_Action'Value(Get_Attribute(Child_Node, "action"))
               else ADD);
            if Name = "recipe" then
               if not Recipes_List.Contains(Value) then
                  raise Data_Loading_Error
                    with "Can't " & To_Lower(Data_Action'Image(Action)) &
                    " base type '" & To_String(Base_Index) &
                    "', no recipe with index '" & To_String(Value) & "'.";
               end if;
               if Data.Contains(Value) and Sub_Action = ADD then
                  raise Data_Loading_Error
                    with "Can't " & To_Lower(Data_Action'Image(Action)) &
                    " base type '" & To_String(Base_Index) & "', recipe '" &
                    To_String(Value) & "' already added.";
               end if;
            end if;
            if Sub_Action /= REMOVE then
               Data.Append(New_Item => Value);
            else
               Delete_Index := Data.First_Index;
               Delete_Child_Data_Loop :
               while Delete_Index <= Data.Last_Index loop
                  if Data(Delete_Index) = Value then
                     Data.Delete(Index => Delete_Index);
                     exit Delete_Child_Data_Loop;
                  end if;
                  Delete_Index := Delete_Index + 1;
               end loop Delete_Child_Data_Loop;
            end if;
         end loop Read_Child_Node_Loop;
      end Add_Child_Node;
   begin
      Bases_Data := Get_Tree(Reader);
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(Bases_Data, "base");
      Read_Bases_Types_Loop :
      for I in 0 .. Length(Nodes_List) - 1 loop
         Temp_Record :=
           (Name => Null_Unbounded_String, Color => "ffffff",
            Trades => Tmp_Trades, Recipes => Tmp_Recipes, Flags => Tmp_Flags,
            Description => Null_Unbounded_String);
         Base_Node := Item(Nodes_List, I);
         Base_Index := To_Unbounded_String(Get_Attribute(Base_Node, "index"));
         Action :=
           (if Get_Attribute(Base_Node, "action")'Length > 0 then
              Data_Action'Value(Get_Attribute(Base_Node, "action"))
            else ADD);
         if Action in UPDATE | REMOVE then
            if not BasesTypes_Container.Contains
                (Bases_Types_List, Base_Index) then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(Data_Action'Image(Action)) &
                 " base type '" & To_String(Base_Index) &
                 "', there no base type with that index.";
            end if;
         elsif BasesTypes_Container.Contains(Bases_Types_List, Base_Index) then
            raise Data_Loading_Error
              with "Can't add base type '" & To_String(Base_Index) &
              "', there is one with that index.";
         end if;
         if Action /= REMOVE then
            if Action = UPDATE then
               Temp_Record := Bases_Types_List(Base_Index);
            end if;
            if Get_Attribute(Base_Node, "name") /= "" then
               Temp_Record.Name :=
                 To_Unbounded_String(Get_Attribute(Base_Node, "name"));
            end if;
            if Get_Attribute(Base_Node, "color") /= "" then
               Temp_Record.Color := Get_Attribute(Base_Node, "color");
            end if;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Base_Node, "description");
            if Length(Child_Nodes) > 0 then
               Temp_Record.Description :=
                 To_Unbounded_String
                   (Node_Value(First_Child(Item(Child_Nodes, 0))));
            end if;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Item(Nodes_List, I), "item");
            Read_Items_Loop :
            for J in 0 .. Length(Child_Nodes) - 1 loop
               Child_Node := Item(Child_Nodes, J);
               Item_Index :=
                 To_Unbounded_String(Get_Attribute(Child_Node, "index"));
               Sub_Action :=
                 (if Get_Attribute(Child_Node, "action")'Length > 0 then
                    Data_Action'Value(Get_Attribute(Child_Node, "action"))
                  else ADD);
               if not Items_List.Contains(Item_Index) then
                  raise Data_Loading_Error
                    with "Can't " & To_Lower(Data_Action'Image(Action)) &
                    " base type '" & To_String(Base_Index) &
                    "', no item with index '" & To_String(Item_Index) & "'.";
               end if;
               if Sub_Action = ADD
                 and then Temp_Record.Trades.Contains(Item_Index) then
                  raise Data_Loading_Error
                    with "Can't " & To_Lower(Data_Action'Image(Action)) &
                    " base type '" & To_String(Base_Index) &
                    "', item with index '" & To_String(Item_Index) &
                    "' already added.";
               end if;
               if Sub_Action /= REMOVE then
                  Sell_Price := 0;
                  if Get_Attribute(Child_Node, "sellprice") /= "" then
                     Sell_Price :=
                       Natural'Value(Get_Attribute(Child_Node, "sellprice"));
                  end if;
                  Buy_Price := 0;
                  if Get_Attribute(Child_Node, "buyprice") /= "" then
                     Buy_Price :=
                       Natural'Value(Get_Attribute(Child_Node, "buyprice"));
                  end if;
                  Temp_Record.Trades.Include
                    (Key => Item_Index, New_Item => (Sell_Price, Buy_Price));
               else
                  Temp_Record.Trades.Delete(Item_Index);
               end if;
            end loop Read_Items_Loop;
            Add_Child_Node(Temp_Record.Recipes, "recipe", I);
            Add_Child_Node(Temp_Record.Flags, "flag", I);
            if Action /= UPDATE then
               BasesTypes_Container.Include
                 (Bases_Types_List, Base_Index, Temp_Record);
               Log_Message
                 ("Base type added: " & To_String(Temp_Record.Name),
                  EVERYTHING);
            else
               Bases_Types_List(Base_Index) := Temp_Record;
               Log_Message
                 ("Base type updated: " & To_String(Temp_Record.Name),
                  EVERYTHING);
            end if;
         else
            BasesTypes_Container.Exclude(Bases_Types_List, Base_Index);
            Log_Message
              ("Base type removed: " & To_String(Base_Index), EVERYTHING);
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
