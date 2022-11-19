--    Copyright 2016-2022 Bartek thindil Jasicki
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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Utils; use Utils;
with Crew; use Crew;
with Crew.Inventory; use Crew.Inventory;

package body Items is

   procedure Load_Items(File_Name: String) is
      use Short_String;
      use Tiny_String;

      Temp_Record: Object_Data;
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
      type Items_Nim_Array is array(0 .. 63) of Integer;
      Temp_Nim_Record: Object_Nim_Data;
      Nim_Items_List: Items_Nim_Array := (others => 0);
      Index: Positive := 1;
      function Load_Ada_Items(Name: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "loadAdaItems";
      procedure Get_Ada_Item
        (Index: Integer; Ada_Item: out Object_Nim_Data) with
         Import => True,
         Convention => C,
         External_Name => "getAdaItem";
      procedure Get_Ada_Items_List
        (Name: chars_ptr; I_List: out Items_Nim_Array) with
         Import => True,
         Convention => C,
         External_Name => "getAdaItemsList";
   begin
      Money_Name :=
        To_Unbounded_String
          (Source =>
             Value
               (Item => Load_Ada_Items(Name => New_String(Str => File_Name))));
      Load_Items_Loop :
      loop
         Get_Ada_Item(Index => Index, Ada_Item => Temp_Nim_Record);
         exit Load_Items_Loop when Temp_Nim_Record.Weight = 0;
         Temp_Record :=
           (Name =>
              To_Bounded_String(Source => Value(Item => Temp_Nim_Record.Name)),
            Weight => Temp_Nim_Record.Weight,
            I_Type =>
              To_Bounded_String
                (Source => Value(Item => Temp_Nim_Record.I_Type)),
            Price => Temp_Nim_Record.Price, Value => Temp_Nim_Record.Value,
            Show_Type =>
              To_Bounded_String
                (Source => Value(Item => Temp_Nim_Record.Show_Type)),
            Description =>
              To_Bounded_String
                (Source => Value(Item => Temp_Nim_Record.Description)),
            Reputation => Temp_Nim_Record.Reputation);
         Objects_Container.Append
           (Container => Items_List, New_Item => Temp_Record);
         Index := Index + 1;
      end loop Load_Items_Loop;
      Get_Ada_Items_List
        (Name => New_String(Str => "weapons"), I_List => Nim_Items_List);
      Load_Weapons_List_Loop :
      for I of Nim_Items_List loop
         exit Load_Weapons_List_Loop when I = 0;
         Positive_Indefinite_Container.Append
           (Container => Weapons_List, New_Item => I);
      end loop Load_Weapons_List_Loop;
      Get_Ada_Items_List
        (Name => New_String(Str => "shields"), I_List => Nim_Items_List);
      Load_Shields_List_Loop :
      for I of Nim_Items_List loop
         exit Load_Shields_List_Loop when I = 0;
         Positive_Indefinite_Container.Append
           (Container => Shields_List, New_Item => I);
      end loop Load_Shields_List_Loop;
      Get_Ada_Items_List
        (Name => New_String(Str => "headarmors"), I_List => Nim_Items_List);
      Load_Head_Armors_List_Loop :
      for I of Nim_Items_List loop
         exit Load_Head_Armors_List_Loop when I = 0;
         Positive_Indefinite_Container.Append
           (Container => Head_Armors_List, New_Item => I);
      end loop Load_Head_Armors_List_Loop;
      Get_Ada_Items_List
        (Name => New_String(Str => "chestarmors"), I_List => Nim_Items_List);
      Load_Chest_Armors_List_Loop :
      for I of Nim_Items_List loop
         exit Load_Chest_Armors_List_Loop when I = 0;
         Positive_Indefinite_Container.Append
           (Container => Chest_Armors_List, New_Item => I);
      end loop Load_Chest_Armors_List_Loop;
      Get_Ada_Items_List
        (Name => New_String(Str => "armsarmors"), I_List => Nim_Items_List);
      Load_Arms_Armors_List_Loop :
      for I of Nim_Items_List loop
         exit Load_Arms_Armors_List_Loop when I = 0;
         Positive_Indefinite_Container.Append
           (Container => Arms_Armors_List, New_Item => I);
      end loop Load_Arms_Armors_List_Loop;
      Get_Ada_Items_List
        (Name => New_String(Str => "legsarmors"), I_List => Nim_Items_List);
      Load_Legs_Armors_List_Loop :
      for I of Nim_Items_List loop
         exit Load_Legs_Armors_List_Loop when I = 0;
         Positive_Indefinite_Container.Append
           (Container => Legs_Armors_List, New_Item => I);
      end loop Load_Legs_Armors_List_Loop;
   end Load_Items;

   function Find_Proto_Item
     (Item_Type: Tiny_String.Bounded_String)
      return Objects_Container.Extended_Index is
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
     (Item_Durability: Items_Durability; To_Lower: Boolean := False)
      return String is

      function Get_Ada_Item_Damage
        (I_Durability: Items_Durability; Lower: Integer) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaItemDamage";
   begin
      return
        Value
          (Item =>
             Get_Ada_Item_Damage
               (I_Durability => Item_Durability,
                Lower => (if To_Lower then 1 else 0)));
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

   procedure Damage_Item
     (Inventory: in out Inventory_Container.Vector; Item_Index: Positive;
      Skill_Level, Member_Index: Natural := 0; Ship: in out Ship_Record) is
      Damage_Chance: Integer :=
        Objects_Container.Element
          (Container => Items_List,
           Index =>
             Inventory_Container.Element
               (Container => Inventory, Index => Item_Index)
               .Proto_Index)
          .Value
          (1);
      I: Inventory_Container.Extended_Index :=
        Inventory_Container.First_Index(Container => Inventory);
      Item: Inventory_Data :=
        Inventory_Container.Element
          (Container => Inventory, Index => Item_Index);
   begin
      if Skill_Level > 0 then
         Damage_Chance := Damage_Chance - (Skill_Level / 5);
         if Damage_Chance < 1 then
            Damage_Chance := 1;
         end if;
      end if;
      if Get_Random(Min => 1, Max => 100) >
        Damage_Chance then -- Item not damaged
         return;
      end if;
      if Inventory_Container.Element
          (Container => Inventory, Index => Item_Index)
          .Amount >
        1 then
         Inventory_Container.Append
           (Container => Inventory,
            New_Item =>
              (Proto_Index => Item.Proto_Index, Amount => (Item.Amount - 1),
               Name => Item.Name, Durability => Item.Durability,
               Price => Item.Price));
         Item.Amount := 1;
      end if;
      Item.Durability := Item.Durability - 1;
      if Item.Durability = 0 then -- Item destroyed
         if Member_Index = 0 then
            Update_Cargo
              (Ship => Ship, Cargo_Index => Item_Index, Amount => -1);
         else
            Update_Inventory
              (Member_Index => Member_Index, Amount => -1,
               Inventory_Index => Item_Index, Ship => Ship);
         end if;
         return;
      end if;
      Inventory_Container.Replace_Element
        (Container => Inventory, Index => Item_Index, New_Item => Item);
      Update_Inventory_Loop :
      while I <= Inventory_Container.Last_Index(Container => Inventory) loop
         Find_Item_Loop :
         for J in
           Inventory_Container.First_Index(Container => Inventory) ..
             Inventory_Container.Last_Index(Container => Inventory) loop
            if Inventory_Container.Element(Container => Inventory, Index => I)
                .Proto_Index =
              Inventory_Container.Element(Container => Inventory, Index => J)
                .Proto_Index and
              Inventory_Container.Element(Container => Inventory, Index => I)
                  .Durability =
                Inventory_Container.Element(Container => Inventory, Index => J)
                  .Durability and
              I /= J then
               if Member_Index = 0 then
                  Update_Cargo
                    (Ship => Ship, Cargo_Index => J,
                     Amount =>
                       -(Inventory_Container.Element
                          (Container => Inventory, Index => J)
                          .Amount));
                  Update_Cargo
                    (Ship => Ship, Cargo_Index => I,
                     Amount =>
                       Inventory_Container.Element
                         (Container => Inventory, Index => J)
                         .Amount);
               else
                  Update_Inventory
                    (Member_Index => Member_Index,
                     Amount =>
                       -(Inventory_Container.Element
                          (Container => Inventory, Index => J)
                          .Amount),
                     Inventory_Index => J, Ship => Ship);
                  Update_Inventory
                    (Member_Index => Member_Index,
                     Amount =>
                       Inventory_Container.Element
                         (Container => Inventory, Index => J)
                         .Amount,
                     Inventory_Index => I, Ship => Ship);
               end if;
               I := I - 1;
               exit Find_Item_Loop;
            end if;
         end loop Find_Item_Loop;
         I := I + 1;
      end loop Update_Inventory_Loop;
   end Damage_Item;

   function Find_Item
     (Inventory: Inventory_Container.Vector;
      Proto_Index: Objects_Container.Extended_Index := 0;
      Item_Type: Tiny_String.Bounded_String := Tiny_String.Null_Bounded_String;
      Durability: Items_Durability := Items_Durability'Last;
      Quality: Positive := 100) return Natural is
      use Tiny_String;

      type Nim_Inventory_Data is record
         Proto_Index: Objects_Container.Extended_Index;
         Amount: Natural := 1;
         Name: chars_ptr;
         Durability: Items_Durability;
         Price: Natural := 0;
      end record;
      type Nim_Inventory_Array is array(0 .. 127) of Nim_Inventory_Data;
      Nim_Inventory: Nim_Inventory_Array :=
        (others =>
           (Proto_Index => 0, Amount => 1, Name => New_String(Str => ""),
            Durability => 0, Price => 0));
      function Find_Ada_Item
        (Inv: Nim_Inventory_Array; P_Index: Integer; I_Type: chars_ptr;
         Dur: Integer; Q: Integer) return Integer with
         Import => True,
         Convention => C,
         External_Name => "findAdaItem";
   begin
      Fill_Nim_Array_Loop :
      for I in
        Inventory_Container.First_Index(Container => Inventory) ..
          Inventory_Container.Last_Index(Container => Inventory) loop
         Set_Item_Block :
         declare
            Item: constant Inventory_Data :=
              Inventory_Container.Element(Container => Inventory, Index => I);
         begin
            Nim_Inventory(I - 1) :=
              (Proto_Index => Item.Proto_Index, Amount => Item.Amount,
               Name => New_String(Str => To_String(Source => Item.Name)),
               Durability => Item.Durability, Price => Item.Price);
         end Set_Item_Block;
      end loop Fill_Nim_Array_Loop;
      return
        Find_Ada_Item
          (Inv => Nim_Inventory, P_Index => Proto_Index,
           I_Type => New_String(Str => To_String(Source => Item_Type)),
           Dur => Durability, Q => Quality);
   end Find_Item;

   procedure Set_Tools_List is
      use Interfaces.C;

      type Items_Nim_Array is array(0 .. 63) of chars_ptr;
      Nim_Items_List: Items_Nim_Array := (others => New_String(Str => ""));
      procedure Set_Ada_Tools_List with
         Import => True,
         Convention => C,
         External_Name => "setAdaToolsList";
      procedure Get_Ada_Tools_List(T_List: out Items_Nim_Array) with
         Import => True,
         Convention => C,
         External_Name => "getAdaToolsList";
   begin
      if TinyString_Indefinite_Container.Length(Container => Tools_List) >
        0 then
         return;
      end if;
      Set_Ada_Tools_List;
      Get_Ada_Tools_List(T_List => Nim_Items_List);
      Load_Tools_List_Loop :
      for I of Nim_Items_List loop
         exit Load_Tools_List_Loop when Strlen(Item => I) = 0;
         TinyString_Indefinite_Container.Append
           (Container => Tools_List,
            New_Item =>
              Tiny_String.To_Bounded_String(Source => Value(Item => I)));
      end loop Load_Tools_List_Loop;
   end Set_Tools_List;

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

end Items;
