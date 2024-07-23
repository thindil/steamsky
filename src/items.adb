--    Copyright 2016-2024 Bartek thindil Jasicki
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

package body Items is

   function Find_Proto_Item
     (Item_Type: Tiny_String.Bounded_String) return Natural is
      use Tiny_String;

      function Find_Ada_Proto_Item(Itype: chars_ptr) return Integer with
         Import => True,
         Convention => C,
         External_Name => "findAdaProtoItem";
   begin
      return
        Find_Ada_Proto_Item
          (Itype => New_String(Str => To_String(Source => Item_Type)));
   end Find_Proto_Item;

   function Get_Item_Damage
     (Item_Durability: Items_Durability;
      To_Lower, With_Colors: Boolean := False) return String is

      function Get_Ada_Item_Damage
        (I_Durability: Items_Durability; Lower, Colors: Integer)
         return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaItemDamage";
   begin
      return
        Value
          (Item =>
             Get_Ada_Item_Damage
               (I_Durability => Item_Durability,
                Lower => (if To_Lower then 1 else 0),
                Colors => (if With_Colors then 1 else 0)));
   end Get_Item_Damage;

   function Get_Item_Name
     (Item: Inventory_Data; Damage_Info, To_Lower: Boolean := True)
      return String is
      use Tiny_String;

      function Get_Ada_Item_Name
        (Name: chars_ptr; Proto_Index, Durability, D_Info, Lower: Integer)
         return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaItemName";
   begin
      return
        Value
          (Item =>
             Get_Ada_Item_Name
               (Name => New_String(Str => To_String(Source => Item.Name)),
                Proto_Index => Item.Proto_Index, Durability => Item.Durability,
                D_Info => (if Damage_Info then 1 else 0),
                Lower => (if To_Lower then 1 else 0)));
   end Get_Item_Name;

   function Find_Item
     (Inventory: Inventory_Container.Vector; Proto_Index: Natural := 0;
      Item_Type: Tiny_String.Bounded_String := Tiny_String.Null_Bounded_String;
      Durability: Items_Durability := Items_Durability'Last;
      Quality: Positive := 100) return Natural is
      use Tiny_String;

      function Find_Ada_Item
        (Inv: Nim_Inventory_Array; P_Index: Integer; I_Type: chars_ptr;
         Dur: Integer; Q: Integer) return Integer with
         Import => True,
         Convention => C,
         External_Name => "findAdaItem";
   begin
      return
        Find_Ada_Item
          (Inv => Inventory_To_Nim(Inventory => Inventory),
           P_Index => Proto_Index,
           I_Type => New_String(Str => To_String(Source => Item_Type)),
           Dur => Durability, Q => Quality);
   end Find_Item;

   function Get_Item_Chance_To_Damage(Item_Data: Natural) return String is
      function Get_Ada_Item_Chance_To_Damage
        (I_Data: Integer) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaItemChanceToDamage";
   begin
      return Value(Item => Get_Ada_Item_Chance_To_Damage(I_Data => Item_Data));
   end Get_Item_Chance_To_Damage;

   function Is_Tool(Item_Type: Tiny_String.Bounded_String) return Boolean is
      function Is_Ada_Tool(I_Type: chars_ptr) return Integer with
         Import => True,
         Convention => C,
         External_Name => "isAdaTool";
   begin
      if Is_Ada_Tool
          (I_Type =>
             New_String(Str => Tiny_String.To_String(Source => Item_Type))) =
        1 then
         return True;
      end if;
      return False;
   end Is_Tool;

   function Inventory_To_Nim
     (Inventory: Inventory_Container.Vector) return Nim_Inventory_Array is
      Nim_Inventory: Nim_Inventory_Array :=
        (others =>
           (Proto_Index => 0, Amount => 1, Name => New_String(Str => ""),
            Durability => 0, Price => 0));
   begin
      Fill_Nim_Array_Loop :
      for I in
        Inventory_Container.First_Index(Container => Inventory) ..
          Inventory_Container.Last_Index(Container => Inventory) loop
         Set_Item_Block :
         declare
            use Tiny_String;

            Item: constant Inventory_Data :=
              Inventory_Container.Element(Container => Inventory, Index => I);
         begin
            Nim_Inventory(I - 1) :=
              (Proto_Index => Item.Proto_Index, Amount => Item.Amount,
               Name => New_String(Str => To_String(Source => Item.Name)),
               Durability => Item.Durability, Price => Item.Price);
         end Set_Item_Block;
      end loop Fill_Nim_Array_Loop;
      return Nim_Inventory;
   end Inventory_To_Nim;

   function Inventory_From_Nim
     (Inventory: Nim_Inventory_Array; Size: Positive)
      return Inventory_Container.Vector is
      use Tiny_String;

      --## rule off IMPROPER_INITIALIZATION
      Ada_Inventory: Inventory_Container.Vector (Capacity => Count_Type(Size));
      --## rule on IMPROPER_INITIALIZATION
   begin
      Fill_Ada_Inventory_Loop :
      for Item of Inventory loop
         exit Fill_Ada_Inventory_Loop when Item.Proto_Index = 0;
         Inventory_Container.Append
           (Container => Ada_Inventory,
            New_Item =>
              (Proto_Index => Item.Proto_Index, Amount => Item.Amount,
               Name => To_Bounded_String(Source => Value(Item => Item.Name)),
               Durability => Item.Durability, Price => Item.Price));
      end loop Fill_Ada_Inventory_Loop;
      return Ada_Inventory;
   end Inventory_From_Nim;

   function Get_Proto_Item(Index: Positive) return Object_Data is
      use Short_String;
      use Tiny_String;

      --## rule off TYPE_INITIAL_VALUES
      type Object_Nim_Data is record
         Name: chars_ptr;
         Weight: Integer;
         I_Type: chars_ptr;
         Price: Integer;
         Value: Integer_Array (Values_Range);
         Show_Type: chars_ptr;
         Description: chars_ptr;
         Reputation: Integer;
      end record;
      --## rule on TYPE_INITIAL_VALUES
      Temp_Nim_Record: Object_Nim_Data;
      procedure Get_Ada_Item
        (I_Index: Integer; Ada_Item: out Object_Nim_Data) with
         Import => True,
         Convention => C,
         External_Name => "getAdaItem";
   begin
      Get_Ada_Item(I_Index => Index, Ada_Item => Temp_Nim_Record);
      return
        (Name =>
           To_Bounded_String(Source => Value(Item => Temp_Nim_Record.Name)),
         Weight => Temp_Nim_Record.Weight,
         I_Type =>
           To_Bounded_String(Source => Value(Item => Temp_Nim_Record.I_Type)),
         Price => Temp_Nim_Record.Price, Value => Temp_Nim_Record.Value,
         Show_Type =>
           To_Bounded_String
             (Source => Value(Item => Temp_Nim_Record.Show_Type)),
         Description =>
           To_Bounded_String
             (Source => Value(Item => Temp_Nim_Record.Description)),
         Reputation => Temp_Nim_Record.Reputation);
   end Get_Proto_Item;

end Items;
