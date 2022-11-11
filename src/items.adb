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
with Crafts; use Crafts;
with Config; use Config;

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
      Temp_Nim_Record: Object_Nim_Data;
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
      Set_Items_Lists_Loop :
      for I in
        Objects_Container.First_Index(Container => Items_List) ..
          Objects_Container.Last_Index(Container => Items_List) loop
         if Objects_Container.Element(Container => Items_List, Index => I)
             .I_Type =
           Weapon_Type then
            Positive_Indefinite_Container.Append
              (Container => Weapons_List, New_Item => I);
         elsif Objects_Container.Element(Container => Items_List, Index => I)
             .I_Type =
           Shield_Type then
            Positive_Indefinite_Container.Append
              (Container => Shields_List, New_Item => I);
         elsif Objects_Container.Element(Container => Items_List, Index => I)
             .I_Type =
           Head_Armor then
            Positive_Indefinite_Container.Append
              (Container => Head_Armors_List, New_Item => I);
         elsif Objects_Container.Element(Container => Items_List, Index => I)
             .I_Type =
           Chest_Armor then
            Positive_Indefinite_Container.Append
              (Container => Chest_Armors_List, New_Item => I);
         elsif Objects_Container.Element(Container => Items_List, Index => I)
             .I_Type =
           Arms_Armor then
            Positive_Indefinite_Container.Append
              (Container => Arms_Armors_List, New_Item => I);
         elsif Objects_Container.Element(Container => Items_List, Index => I)
             .I_Type =
           Legs_Armor then
            Positive_Indefinite_Container.Append
              (Container => Legs_Armors_List, New_Item => I);
         end if;
      end loop Set_Items_Lists_Loop;
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
          (Item => Get_Ada_Item_Damage
             (I_Durability => Item_Durability,
              Lower => (if To_Lower then 1 else 0)));
   end Get_Item_Damage;

   function Get_Item_Name
     (Item: Inventory_Data; Damage_Info, To_Lower: Boolean := True)
      return String is
      use Tiny_String;

      Item_Name: Unbounded_String :=
        (if Item.Name /= Null_Bounded_String then
           To_Unbounded_String(Source => To_String(Source => Item.Name))
         else To_Unbounded_String
             (Source =>
                To_String
                  (Source =>
                     Objects_Container.Element
                       (Container => Items_List, Index => Item.Proto_Index)
                       .Name)));
   begin
      if Damage_Info and then Item.Durability < 100 then
         Append
           (Source => Item_Name,
            New_Item =>
              " (" &
              Get_Item_Damage
                (Item_Durability => Item.Durability, To_Lower => To_Lower) &
              ")");
      end if;
      return To_String(Source => Item_Name);
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
   begin
      if Proto_Index > 0 then
         Find_Item_With_Proto_Loop :
         for I in
           Inventory_Container.First_Index(Container => Inventory) ..
             Inventory_Container.Last_Index(Container => Inventory) loop
            if Inventory_Container.Element(Container => Inventory, Index => I)
                .Proto_Index =
              Proto_Index
              and then
              (Objects_Container.Element
                 (Container => Items_List,
                  Index =>
                    Inventory_Container.Element
                      (Container => Inventory, Index => I)
                      .Proto_Index)
                 .Value
                 (1) <=
               Quality) then
               if Durability < Items_Durability'Last
                 and then
                   Inventory_Container.Element
                     (Container => Inventory, Index => I)
                     .Durability =
                   Durability then
                  return I;
               else
                  return I;
               end if;
            end if;
         end loop Find_Item_With_Proto_Loop;
      elsif Item_Type /= Null_Bounded_String then
         Find_Item_Loop :
         for I in
           Inventory_Container.First_Index(Container => Inventory) ..
             Inventory_Container.Last_Index(Container => Inventory) loop
            if Objects_Container.Element
                (Container => Items_List,
                 Index =>
                   Inventory_Container.Element
                     (Container => Inventory, Index => I)
                     .Proto_Index)
                .I_Type =
              Item_Type
              and then
              (Objects_Container.Element
                 (Container => Items_List,
                  Index =>
                    Inventory_Container.Element
                      (Container => Inventory, Index => I)
                      .Proto_Index)
                 .Value
                 (1) <=
               Quality) then
               if Durability < Items_Durability'Last
                 and then
                   Inventory_Container.Element
                     (Container => Inventory, Index => I)
                     .Durability =
                   Durability then
                  return I;
               else
                  return I;
               end if;
            end if;
         end loop Find_Item_Loop;
      end if;
      return 0;
   end Find_Item;

   procedure Set_Tools_List is
   begin
      if TinyString_Indefinite_Container.Length(Container => Tools_List) >
        0 then
         return;
      end if;
      TinyString_Indefinite_Container.Append
        (Container => Tools_List, New_Item => Repair_Tools);
      TinyString_Indefinite_Container.Append
        (Container => Tools_List, New_Item => Cleaning_Tools);
      TinyString_Indefinite_Container.Append
        (Container => Tools_List, New_Item => Alchemy_Tools);
      Recipes_Loop :
      for Recipe of Recipes_List loop
         if TinyString_Indefinite_Container.Find_Index
             (Container => Tools_List, Item => Recipe.Tool) =
           TinyString_Indefinite_Container.No_Index then
            TinyString_Indefinite_Container.Append
              (Container => Tools_List, New_Item => Recipe.Tool);
         end if;
      end loop Recipes_Loop;
      Skills_Loop :
      for I in 1 .. Skills_Amount loop
         if TinyString_Indefinite_Container.Find_Index
             (Container => Tools_List,
              Item =>
                SkillsData_Container.Element
                  (Container => Skills_List, Index => I)
                  .Tool) =
           TinyString_Indefinite_Container.No_Index then
            TinyString_Indefinite_Container.Append
              (Container => Tools_List,
               New_Item =>
                 SkillsData_Container.Element
                   (Container => Skills_List, Index => I)
                   .Tool);
         end if;
      end loop Skills_Loop;
   end Set_Tools_List;

   function Get_Item_Chance_To_Damage(Item_Data: Natural) return String is
   begin
      if Game_Settings.Show_Numbers then
         return Natural'Image(Item_Data) & "%";
      end if;
      case Item_Data is
         when 1 =>
            return "Almost never";
         when 2 =>
            return "Very small";
         when 3 =>
            return "Small";
         when 4 .. 9 =>
            return "Below average";
         when 10 .. 14 =>
            return "Average";
         when 15 .. 19 =>
            return "High";
         when others =>
            return "Very high";
      end case;
   end Get_Item_Chance_To_Damage;

end Items;
