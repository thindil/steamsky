--    Copyright 2017-2023 Bartek thindil Jasicki
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

package body Crew.Inventory is

   procedure Equipment_To_Ada
     (M_Index: Integer; Equipment: out Nim_Equipment_Array) with
      Import => True,
      Convention => C,
      External_Name => "equipmentToAda";

   procedure Update_Inventory
     (Member_Index: Positive; Amount: Integer; Proto_Index: Natural := 0;
      Durability: Items_Durability := 0; Inventory_Index, Price: Natural := 0;
      Ship: in out Ship_Record) is
      use Tiny_String;
      Nim_Inventory: Nim_Inventory_Array :=
        Inventory_To_Nim
          (Inventory => Player_Ship.Crew(Member_Index).Inventory);
      Nim_Equipment: Nim_Equipment_Array;
      function Update_Ada_Inventory
        (M_Index, Amnt, P_Index, Dur, I_Index, Pric, In_Player_Ship: Integer)
         return Integer with
         Import => True,
         Convention => C,
         External_Name => "updateAdaInventory";
   begin
      Get_Ada_Crew(Ship => Ship);
      Get_Ada_Crew_Inventory
        (Inventory => Nim_Inventory, Member_Index => Member_Index,
         Get_Player_Ship => (if Ship = Player_Ship then 1 else 0));
      if Update_Ada_Inventory
          (M_Index => Member_Index, Amnt => Amount, P_Index => Proto_Index,
           Dur => Durability, I_Index => Inventory_Index, Pric => Price,
           In_Player_Ship => (if Ship = Player_Ship then 1 else 0)) =
        0 then
         raise Crew_No_Space_Error
           with To_String(Source => Ship.Crew(Member_Index).Name) &
           " doesn't have any free space in their inventory.";
      end if;
      Set_Ada_Crew_Inventory
        (Inventory => Nim_Inventory, Member_Index => Member_Index,
         Get_Player_Ship => (if Ship = Player_Ship then 1 else 0));
      Ship.Crew(Member_Index).Inventory :=
        Inventory_From_Nim(Inventory => Nim_Inventory, Size => 32);
      Equipment_To_Ada(M_Index => Member_Index, Equipment => Nim_Equipment);
      Update_Equipment_Loop :
      for I in Nim_Equipment'Range loop
         Player_Ship.Crew(Member_Index).Equipment
           (Equipment_Locations'Val(I)) :=
           Nim_Equipment(I);
      end loop Update_Equipment_Loop;
   end Update_Inventory;

   function Free_Inventory
     (Member_Index: Positive; Amount: Integer; Update_Nim: Boolean := True)
      return Integer is
      function Free_Ada_Inventory(M_Index, Amnt: Integer) return Integer with
         Import => True,
         Convention => C,
         External_Name => "freeAdaInventory";
   begin
      if Update_Nim then
         Get_Ada_Crew;
         Get_Ada_Crew_Inventory
           (Inventory =>
              Inventory_To_Nim
                (Inventory => Player_Ship.Crew(Member_Index).Inventory),
            Member_Index => Member_Index);
      end if;
      return Free_Ada_Inventory(M_Index => Member_Index, Amnt => Amount);
   end Free_Inventory;

   procedure Take_Off_Item
     (Member_Index, Item_Index: Positive; Update_Nim: Boolean := True) is
      Nim_Equipment: Nim_Equipment_Array;
      procedure Take_Ada_Off_Item(M_Index, I_Index: Integer) with
         Import => True,
         Convention => C,
         External_Name => "takeAdaOffItem";
   begin
      if Update_Nim then
         Get_Ada_Crew;
         Get_Ada_Crew_Inventory
           (Inventory =>
              Inventory_To_Nim
                (Inventory => Player_Ship.Crew(Member_Index).Inventory),
            Member_Index => Member_Index);
      end if;
      Take_Ada_Off_Item(M_Index => Member_Index, I_Index => Item_Index);
      Equipment_To_Ada(M_Index => Member_Index, Equipment => Nim_Equipment);
      Update_Equipment_Loop :
      for I in Nim_Equipment'Range loop
         Player_Ship.Crew(Member_Index).Equipment
           (Equipment_Locations'Val(I)) :=
           Nim_Equipment(I);
      end loop Update_Equipment_Loop;
   end Take_Off_Item;

   function Item_Is_Used
     (Member_Index, Item_Index: Positive; Update_Nim: Boolean := True)
      return Boolean is
      function Item_Ada_Is_Used(M_Index, I_Index: Integer) return Integer with
         Import => True,
         Convention => C,
         External_Name => "itemAdaIsUsed";
   begin
      if Update_Nim then
         Get_Ada_Crew;
         Get_Ada_Crew_Inventory
           (Inventory =>
              Inventory_To_Nim
                (Inventory => Player_Ship.Crew(Member_Index).Inventory),
            Member_Index => Member_Index);
      end if;
      if Item_Ada_Is_Used(M_Index => Member_Index, I_Index => Item_Index) =
        1 then
         return True;
      end if;
      return False;
   end Item_Is_Used;

   function Find_Tools
     (Member_Index: Positive; Item_Type: Tiny_String.Bounded_String;
      Order: Crew_Orders; Tool_Quality: Positive := 100) return Natural is
      use Tiny_String;

      Tools_Index: Natural := 0;
      Nim_Equipment: Nim_Equipment_Array;
      Nim_Inventory: Nim_Inventory_Array :=
        Inventory_To_Nim
          (Inventory => Player_Ship.Crew(Member_Index).Inventory);
      function Find_Ada_Tools
        (M_Index: Integer; I_Type: chars_ptr; M_Order, T_Quality: Integer)
         return Natural with
         Import => True,
         Convention => C,
         External_Name => "findAdaTools";
   begin
      Get_Ada_Crew;
      Get_Ada_Crew_Inventory
        (Inventory => Nim_Inventory, Member_Index => Member_Index);
      Tools_Index :=
        Find_Ada_Tools
          (M_Index => Member_Index,
           I_Type => New_String(Str => To_String(Source => Item_Type)),
           M_Order => Crew_Orders'Pos(Order), T_Quality => Tool_Quality);
      Set_Ada_Crew_Inventory
        (Inventory => Nim_Inventory, Member_Index => Member_Index,
         Get_Player_Ship => 1);
      Player_Ship.Crew(Member_Index).Inventory :=
        Inventory_From_Nim(Inventory => Nim_Inventory, Size => 32);
      Equipment_To_Ada(M_Index => Member_Index, Equipment => Nim_Equipment);
      Update_Equipment_Loop :
      for I in Nim_Equipment'Range loop
         Player_Ship.Crew(Member_Index).Equipment
           (Equipment_Locations'Val(I)) :=
           Nim_Equipment(I);
      end loop Update_Equipment_Loop;
      return Tools_Index;
   end Find_Tools;

end Crew.Inventory;
