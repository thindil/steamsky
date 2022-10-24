--    Copyright 2019-2022 Bartek thindil Jasicki
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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C.Strings;
with Ada.Characters.Handling;
with DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Nodes;
with DOM.Core.Elements;
with Bases;
with Crafts;
with Log;

package body BasesTypes is

   procedure Load_Bases_Types(Reader: Tree_Reader; File_Name: String) is
      use Ada.Characters.Handling;
      use Interfaces.C;
      use Interfaces.C.Strings;
      use DOM.Core;
      use DOM.Core.Nodes;
      use DOM.Core.Elements;
      use Log;
      use Tiny_String;

      --## rule off IMPROPER_INITIALIZATION
      Temp_Record: Base_Type_Data;
      Nodes_List, Child_Nodes: Node_List;
      Bases_Data: Document;
      Tmp_Trades: BasesTrade_Container.Map;
      Tmp_Recipes: UnboundedString_Container.Vector;
      Tmp_Flags: UnboundedString_Container.Vector;
      --## rule on IMPROPER_INITIALIZATION
      Base_Node, Child_Node: Node;
      Base_Index: Bounded_String := Null_Bounded_String;
      Action, Sub_Action: Data_Action := ADD;
      Buy_Price, Sell_Price: Natural := 0;
      Item_Index: Objects_Container.Extended_Index := 0;
      type Base_Type_Nim_Data is record
         Name: chars_ptr;
         Color: chars_ptr;
         Description: chars_ptr;
      end record;
      Temp_Nim_Record: Base_Type_Nim_Data;
      Index: Positive := 1;
      Index2: Natural := 0;
      Base_Data: Unbounded_String := Null_Unbounded_String;
      procedure Add_Child_Node
        (Data: in out UnboundedString_Container.Vector; Name: String;
         Index: Natural) is
         use Crafts;

         Value: Unbounded_String := Null_Unbounded_String;
         Delete_Index: Positive := 1;
      begin
         Child_Nodes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name
             (Elem => Item(List => Nodes_List, Index => Index), Name => Name);
         Read_Child_Node_Loop :
         for J in 0 .. Length(List => Child_Nodes) - 1 loop
            Child_Node := Item(List => Child_Nodes, Index => J);
            Value :=
              (if Name = "flag" then
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute(Elem => Child_Node, Name => "name"))
               else To_Unbounded_String
                   (Source =>
                      Get_Attribute(Elem => Child_Node, Name => "index")));
            Sub_Action :=
              (if
                 Get_Attribute(Elem => Child_Node, Name => "action")'Length > 0
               then
                 Data_Action'Value
                   (Get_Attribute(Elem => Child_Node, Name => "action"))
               else ADD);
            if Name = "recipe" then
               if not Recipes_List.Contains
                   (Key =>
                      To_Bounded_String
                        (Source => To_String(Source => Value))) then
                  raise Data_Loading_Error
                    with "Can't " &
                    To_Lower(Item => Data_Action'Image(Action)) &
                    " base type '" & To_String(Source => Base_Index) &
                    "', no recipe with index '" & To_String(Source => Value) &
                    "'.";
               end if;
               if Data.Contains(Item => Value) and Sub_Action = ADD then
                  raise Data_Loading_Error
                    with "Can't " &
                    To_Lower(Item => Data_Action'Image(Action)) &
                    " base type '" & To_String(Source => Base_Index) &
                    "', recipe '" & To_String(Source => Value) &
                    "' already added.";
               end if;
            end if;
            if Sub_Action = REMOVE then
               Delete_Index := Data.First_Index;
               --## rule off SIMPLIFIABLE_STATEMENTS
               Delete_Child_Data_Loop :
               while Delete_Index <= Data.Last_Index loop
                  if Data(Delete_Index) = Value then
                     Data.Delete(Index => Delete_Index);
                     exit Delete_Child_Data_Loop;
                  end if;
                  Delete_Index := Delete_Index + 1;
               end loop Delete_Child_Data_Loop;
               --## rule on SIMPLIFIABLE_STATEMENTS
            else
               Data.Append(New_Item => Value);
            end if;
         end loop Read_Child_Node_Loop;
      end Add_Child_Node;
      procedure Load_Ada_Bases_Types(Name: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "loadAdaBasesTypes";
      procedure Get_Ada_Base_Type
        (Base_Index: chars_ptr; Ada_Base_Type: out Base_Type_Nim_Data) with
         Import => True,
         Convention => C,
         External_Name => "getAdaBaseType";
      function Get_Ada_Base_Data
        (Base_Index: chars_ptr; Item_Index: Integer; Data_Type: chars_ptr)
         return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaBaseData";
   begin
      Load_Ada_Bases_Types(Name => New_String(Str => File_Name));
      Load_Bases_Types_Loop :
      loop
         Get_Ada_Base_Type
           (Base_Index => New_String(Str => Index'Img),
            Ada_Base_Type => Temp_Nim_Record);
         exit Load_Bases_Types_Loop when Strlen(Item => Temp_Nim_Record.Name) =
           0;
         Temp_Record.Name :=
           To_Unbounded_String(Source => Value(Item => Temp_Nim_Record.Name));
         Temp_Record.Color := Value(Item => Temp_Nim_Record.Color);
         Temp_Record.Description :=
           To_Unbounded_String
             (Source => Value(Item => Temp_Nim_Record.Description));
         Index2 := 0;
         Load_Base_Recipes_Loop :
         loop
            Base_Data :=
              To_Unbounded_String
                (Source =>
                   (Value
                      (Item =>
                         Get_Ada_Base_Data
                           (Base_Index => New_String(Str => Index'Img),
                            Item_Index => Index2,
                            Data_Type => New_String(Str => "recipe")))));
            exit Load_Base_Recipes_Loop when Length(Source => Base_Data) = 0;
            Temp_Record.Recipes.Append(New_Item => Base_Data);
            Index2 := Index2 + 1;
         end loop Load_Base_Recipes_Loop;
         Index2 := 0;
         Load_Base_Flags_Loop :
         loop
            Base_Data :=
              To_Unbounded_String
                (Source =>
                   (Value
                      (Item =>
                         Get_Ada_Base_Data
                           (Base_Index => New_String(Str => Index'Img),
                            Item_Index => Index2,
                            Data_Type => New_String(Str => "flag")))));
            exit Load_Base_Flags_Loop when Length(Source => Base_Data) = 0;
            Temp_Record.Flags.Append(New_Item => Base_Data);
            Index2 := Index2 + 1;
         end loop Load_Base_Flags_Loop;
         BasesTypes_Container.Include
           (Container => Bases_Types_List,
            Key => To_Bounded_String(Source => Index'Img),
            New_Item => Temp_Record);
         Index := Index + 1;
      end loop Load_Bases_Types_Loop;
      Bases_Data := Get_Tree(Read => Reader);
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Bases_Data, Tag_Name => "base");
      Read_Bases_Types_Loop :
      for I in 0 .. Length(List => Nodes_List) - 1 loop
         Temp_Record :=
           (Name => Null_Unbounded_String, Color => "ffffff",
            Trades => Tmp_Trades, Recipes => Tmp_Recipes, Flags => Tmp_Flags,
            Description => Null_Unbounded_String);
         Base_Node := Item(List => Nodes_List, Index => I);
         Base_Index :=
           To_Bounded_String
             (Source => Get_Attribute(Elem => Base_Node, Name => "index"));
         Action :=
           (if Get_Attribute(Elem => Base_Node, Name => "action")'Length > 0
            then
              Data_Action'Value
                (Get_Attribute(Elem => Base_Node, Name => "action"))
            else ADD);
         if Action in UPDATE | REMOVE then
            if not BasesTypes_Container.Contains
                (Container => Bases_Types_List, Key => Base_Index) then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(Item => Data_Action'Image(Action)) &
                 " base type '" & To_String(Source => Base_Index) &
                 "', there no base type with that index.";
            end if;
         elsif BasesTypes_Container.Contains
             (Container => Bases_Types_List, Key => Base_Index) then
            raise Data_Loading_Error
              with "Can't add base type '" & To_String(Source => Base_Index) &
              "', there is one with that index.";
         end if;
         if Action = REMOVE then
            BasesTypes_Container.Exclude
              (Container => Bases_Types_List, Key => Base_Index);
            Log_Message
              (Message =>
                 "Base type removed: " & To_String(Source => Base_Index),
               Message_Type => EVERYTHING);
         else
            if Action = UPDATE then
               Temp_Record := Bases_Types_List(Base_Index);
            end if;
            if Get_Attribute(Elem => Base_Node, Name => "name") /= "" then
               Temp_Record.Name :=
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute(Elem => Base_Node, Name => "name"));
            end if;
            if Get_Attribute(Elem => Base_Node, Name => "color") /= "" then
               Temp_Record.Color :=
                 Get_Attribute(Elem => Base_Node, Name => "color");
            end if;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Elem => Base_Node, Name => "description");
            if Length(List => Child_Nodes) > 0 then
               Temp_Record.Description :=
                 To_Unbounded_String
                   (Source =>
                      Node_Value
                        (N =>
                           First_Child
                             (N => Item(List => Child_Nodes, Index => 0))));
            end if;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Elem => Item(List => Nodes_List, Index => I), Name => "item");
            Read_Items_Loop :
            for J in 0 .. Length(List => Child_Nodes) - 1 loop
               Child_Node := Item(List => Child_Nodes, Index => J);
               Item_Index :=
                 Positive'Value
                   (Get_Attribute(Elem => Child_Node, Name => "index"));
               Sub_Action :=
                 (if
                    Get_Attribute(Elem => Child_Node, Name => "action")'
                      Length >
                    0
                  then
                    Data_Action'Value
                      (Get_Attribute(Elem => Child_Node, Name => "action"))
                  else ADD);
               if Item_Index not in
                   Objects_Container.First_Index(Container => Items_List) ..
                         Objects_Container.Last_Index
                           (Container => Items_List) then
                  raise Data_Loading_Error
                    with "Can't " &
                    To_Lower(Item => Data_Action'Image(Action)) &
                    " base type '" & To_String(Source => Base_Index) &
                    "', no item with index '" &
                    Objects_Container.Extended_Index'Image(Item_Index) & "'.";
               end if;
               if Sub_Action = ADD
                 and then Temp_Record.Trades.Contains
                   (Key =>
                      To_Bounded_String
                        (Source =>
                           Trim
                             (Source => Positive'Image(Item_Index),
                              Side => Left))) then
                  raise Data_Loading_Error
                    with "Can't " &
                    To_Lower(Item => Data_Action'Image(Action)) &
                    " base type '" & To_String(Source => Base_Index) &
                    "', item with index '" & Positive'Image(Item_Index) &
                    "' already added.";
               end if;
               if Sub_Action = REMOVE then
                  Temp_Record.Trades.Delete
                    (Key =>
                       To_Bounded_String
                         (Source =>
                            Trim
                              (Source => Positive'Image(Item_Index),
                               Side => Left)));
               else
                  Sell_Price := 0;
                  if Get_Attribute(Elem => Child_Node, Name => "sellprice") /=
                    "" then
                     Sell_Price :=
                       Natural'Value
                         (Get_Attribute
                            (Elem => Child_Node, Name => "sellprice"));
                  end if;
                  Buy_Price := 0;
                  if Get_Attribute(Elem => Child_Node, Name => "buyprice") /=
                    "" then
                     Buy_Price :=
                       Natural'Value
                         (Get_Attribute
                            (Elem => Child_Node, Name => "buyprice"));
                  end if;
                  Temp_Record.Trades.Include
                    (Key =>
                       To_Bounded_String
                         (Source =>
                            Trim
                              (Source => Positive'Image(Item_Index),
                               Side => Left)),
                     New_Item => (1 => Sell_Price, 2 => Buy_Price));
               end if;
            end loop Read_Items_Loop;
            Add_Child_Node
              (Data => Temp_Record.Recipes, Name => "recipe", Index => I);
            Add_Child_Node
              (Data => Temp_Record.Flags, Name => "flag", Index => I);
            if Action = UPDATE then
               Bases_Types_List(Base_Index) := Temp_Record;
               Log_Message
                 (Message =>
                    "Base type updated: " &
                    To_String(Source => Temp_Record.Name),
                  Message_Type => EVERYTHING);
            else
               BasesTypes_Container.Include
                 (Container => Bases_Types_List, Key => Base_Index,
                  New_Item => Temp_Record);
               Log_Message
                 (Message =>
                    "Base type added: " &
                    To_String(Source => Temp_Record.Name),
                  Message_Type => EVERYTHING);
            end if;
         end if;
      end loop Read_Bases_Types_Loop;
   end Load_Bases_Types;

   function Is_Buyable
     (Base_Type: Tiny_String.Bounded_String;
      Item_Index: Objects_Container.Extended_Index;
      Check_Flag: Boolean := True; Base_Index: Extended_Base_Range := 0)
      return Boolean is
      use Bases;
      use Tiny_String;

   begin
      if Base_Index > 0
        and then Sky_Bases(Base_Index).Reputation.Level <
          Objects_Container.Element
            (Container => Items_List, Index => Item_Index)
            .Reputation then
         return False;
      end if;
      if Check_Flag
        and then
        (Bases_Types_List(Base_Type).Flags.Contains
           (Item => To_Unbounded_String(Source => "blackmarket")) and
         Get_Price(Base_Type => Base_Type, Item_Index => Item_Index) > 0) then
         return True;
      end if;
      if not Bases_Types_List(Base_Type).Trades.Contains
          (Key =>
             To_Bounded_String
               (Source =>
                  Trim
                    (Source => Positive'Image(Item_Index), Side => Left))) then
         return False;
      end if;
      if Bases_Types_List(Base_Type).Trades
          (To_Bounded_String
             (Source =>
                Trim(Source => Positive'Image(Item_Index), Side => Left)))
          (1) =
        0 then
         return False;
      end if;
      return True;
   end Is_Buyable;

   function Get_Price
     (Base_Type: Tiny_String.Bounded_String;
      Item_Index: Objects_Container.Extended_Index) return Natural is
      New_Item_Index: constant Tiny_String.Bounded_String :=
        Tiny_String.To_Bounded_String
          (Source => Trim(Source => Positive'Image(Item_Index), Side => Left));
   begin
      if Objects_Container.Element
          (Container => Items_List, Index => Item_Index)
          .Price =
        0 then
         return 0;
      end if;
      if Bases_Types_List(Base_Type).Trades.Contains
          (Key => New_Item_Index) then
         if Bases_Types_List(Base_Type).Trades(New_Item_Index)(1) > 0 then
            return Bases_Types_List(Base_Type).Trades(New_Item_Index)(1);
         elsif Bases_Types_List(Base_Type).Trades(New_Item_Index)(2) > 0 then
            return Bases_Types_List(Base_Type).Trades(New_Item_Index)(2);
         end if;
      end if;
      return
        Objects_Container.Element(Container => Items_List, Index => Item_Index)
          .Price;
   end Get_Price;

end BasesTypes;
